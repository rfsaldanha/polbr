(function () {
  "use strict";

  const overlays = new Map();
  const rasterPreloads = new Map();
  const jsonPreloads = new Map();
  const latestRasterTokens = new Map();
  const mapLanguageRequests = new Map();
  const weatherObservationStates = new Map();
  const forecastOpacityRequests = new Map();
  const lightningOverlays = new Map();
  const firePulseStates = new Map();
  let totemActive = false;
  let fullscreenEventsBound = false;
  let handlersRegistered = false;
  let urlTotemApplied = false;

  function remember(cache, key, promise, limit) {
    cache.set(key, promise);
    while (cache.size > limit) cache.delete(cache.keys().next().value);
    promise.catch(() => {
      if (cache.get(key) === promise) cache.delete(key);
    });
    return promise;
  }

  function loadImage(url) {
    if (rasterPreloads.has(url)) return rasterPreloads.get(url);
    const promise = new Promise((resolve, reject) => {
      const image = new Image();
      image.onload = () => resolve(url);
      image.onerror = () => reject(new Error("Falha ao carregar " + url));
      image.src = url;
    });
    return remember(rasterPreloads, url, promise, 32);
  }

  function loadJson(url) {
    if (jsonPreloads.has(url)) return jsonPreloads.get(url);
    const promise = fetch(url, {cache: "force-cache"}).then(response => {
      if (!response.ok) throw new Error("HTTP " + response.status);
      return response.json();
    });
    return remember(jsonPreloads, url, promise, 24);
  }

  function preloadResources(message) {
    for (const url of message.rasterUrls || []) loadImage(url).catch(() => {});
    for (const url of message.windUrls || []) loadJson(url).catch(() => {});
  }

  function getMapElement(id) {
    return document.getElementById(id) || document.querySelector("#" + CSS.escape(id));
  }

  function parseWind(records) {
    if (!Array.isArray(records)) return null;
    const u = records.find(r => r.header && (r.header.parameterNumber === 2 || /u-component/i.test(r.header.parameterNumberName || "")));
    const v = records.find(r => r.header && (r.header.parameterNumber === 3 || /v-component/i.test(r.header.parameterNumberName || "")));
    if (!u || !v) return null;
    const h = u.header;
    return {
      u: u.data, v: v.data, nx: h.nx, ny: h.ny,
      lo1: h.lo1, la1: h.la1, dx: h.dx, dy: h.dy,
      lo2: h.lo2, la2: h.la2
    };
  }

  function interpolate(grid, lon, lat) {
    let x = (lon - grid.lo1) / grid.dx;
    if (x < 0 && grid.lo1 >= 0) x = (lon + 360 - grid.lo1) / grid.dx;
    const descending = grid.la1 > grid.la2;
    const y = descending ? (grid.la1 - lat) / grid.dy : (lat - grid.la1) / grid.dy;
    const x0 = Math.floor(x), y0 = Math.floor(y), x1 = x0 + 1, y1 = y0 + 1;
    if (x0 < 0 || y0 < 0 || x1 >= grid.nx || y1 >= grid.ny) return null;
    const rx = x - x0, ry = y - y0;
    const i00 = y0 * grid.nx + x0, i10 = y0 * grid.nx + x1;
    const i01 = y1 * grid.nx + x0, i11 = y1 * grid.nx + x1;
    const blend = arr => (arr[i00] * (1-rx) * (1-ry)) + (arr[i10] * rx * (1-ry)) + (arr[i01] * (1-rx) * ry) + (arr[i11] * rx * ry);
    const u = blend(grid.u), v = blend(grid.v);
    if (!Number.isFinite(u) || !Number.isFinite(v)) return null;
    return {u, v, speed: Math.sqrt(u*u + v*v)};
  }

  function makeOverlay(el, map) {
    const canvas = document.createElement("canvas");
    canvas.className = "wind-canvas";
    el.appendChild(canvas);
    const ctx = canvas.getContext("2d");
    const state = {
      el, map, canvas, ctx, grid: null, active: false, moving: false,
      frame: null, particles: [], token: 0
    };
    // Visual advection scale only; wind values and color classes stay unchanged.
    const particleAdvection = .0035;

    function resize() {
      const dpr = Math.min(window.devicePixelRatio || 1, 2);
      const rect = el.getBoundingClientRect();
      canvas.width = Math.max(1, Math.round(rect.width * dpr));
      canvas.height = Math.max(1, Math.round(rect.height * dpr));
      canvas.style.width = rect.width + "px";
      canvas.style.height = rect.height + "px";
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      seed();
    }

    function clear() {
      ctx.clearRect(0, 0, el.clientWidth, el.clientHeight);
    }

    function stop() {
      if (state.frame) cancelAnimationFrame(state.frame);
      state.frame = null;
    }

    function start() {
      if (!state.active || !state.grid || state.moving || state.frame) return;
      state.frame = requestAnimationFrame(draw);
    }

    function randomParticle() {
      const bounds = map.getBounds();
      let west = bounds.getWest(), east = bounds.getEast();
      let south = bounds.getSouth(), north = bounds.getNorth();
      if (state.grid) {
        west = Math.max(west, Math.min(state.grid.lo1, state.grid.lo2));
        east = Math.min(east, Math.max(state.grid.lo1, state.grid.lo2));
        south = Math.max(south, Math.min(state.grid.la1, state.grid.la2));
        north = Math.min(north, Math.max(state.grid.la1, state.grid.la2));
        if (west >= east || south >= north) {
          west = Math.min(state.grid.lo1, state.grid.lo2);
          east = Math.max(state.grid.lo1, state.grid.lo2);
          south = Math.min(state.grid.la1, state.grid.la2);
          north = Math.max(state.grid.la1, state.grid.la2);
        }
      }
      return {
        lon: west + Math.random() * (east - west),
        lat: south + Math.random() * (north - south),
        age: Math.floor(Math.random() * 90)
      };
    }

    function seed() {
      const count = Math.max(260, Math.min(1250, Math.round((el.clientWidth * el.clientHeight) / 1150)));
      state.particles = Array.from({length: count}, randomParticle);
      clear();
    }

    const windColors = [
      "rgba(86,165,190,.50)",
      "rgba(83,217,199,.66)",
      "rgba(190,239,121,.80)",
      "rgba(255,226,112,.90)"
    ];

    function colorIndex(speed) {
      if (speed < 2) return 0;
      if (speed < 5) return 1;
      if (speed < 9) return 2;
      return 3;
    }

    function draw() {
      state.frame = null;
      const mapIsMoving = typeof map.isMoving === "function" && map.isMoving();
      if (!state.active || !state.grid || state.moving || mapIsMoving) {
        clear();
        return;
      }
      const width = el.clientWidth, height = el.clientHeight;
      ctx.globalCompositeOperation = "destination-in";
      ctx.fillStyle = "rgba(0,0,0,.91)";
      ctx.fillRect(0, 0, width, height);
      ctx.globalCompositeOperation = "source-over";
      ctx.lineWidth = .95;
      const paths = [[], [], [], []];

      for (let i = 0; i < state.particles.length; i++) {
        let p = state.particles[i];
        if (p.age++ > 105) p = state.particles[i] = randomParticle();
        const wind = interpolate(state.grid, p.lon, p.lat);
        if (!wind) { state.particles[i] = randomParticle(); continue; }
        const a = map.project([p.lon, p.lat]);
        const latFactor = Math.max(.25, Math.cos(p.lat * Math.PI / 180));
        const lonStep = wind.u * particleAdvection / latFactor;
        const latStep = wind.v * particleAdvection;
        let nextLon = p.lon + lonStep;
        let nextLat = p.lat + latStep;
        let b = map.project([nextLon, nextLat]);
        const projectedLength = Math.hypot(b.x - a.x, b.y - a.y);
        const maxProjectedStep = 1.6 + Math.min(wind.speed, 16) * .14;

        // A fixed geographic step becomes excessively long at high zoom.
        // Scale only the visual advection so the meteorological vector remains unchanged.
        if (Number.isFinite(projectedLength) && projectedLength > maxProjectedStep) {
          const scale = maxProjectedStep / projectedLength;
          nextLon = p.lon + lonStep * scale;
          nextLat = p.lat + latStep * scale;
          b = map.project([nextLon, nextLat]);
        }

        if (
          !Number.isFinite(a.x) || !Number.isFinite(a.y) ||
          !Number.isFinite(b.x) || !Number.isFinite(b.y) ||
          a.x < 0 || a.y < 0 || a.x > width || a.y > height ||
          b.x < 0 || b.y < 0 || b.x > width || b.y > height
        ) {
          state.particles[i] = randomParticle(); continue;
        }
        paths[colorIndex(wind.speed)].push(a.x, a.y, b.x, b.y);
        p.lon = nextLon; p.lat = nextLat;
      }

      for (let index = 0; index < paths.length; index++) {
        const path = paths[index];
        if (!path.length) continue;
        ctx.strokeStyle = windColors[index];
        ctx.beginPath();
        for (let offset = 0; offset < path.length; offset += 4) {
          ctx.moveTo(path[offset], path[offset + 1]);
          ctx.lineTo(path[offset + 2], path[offset + 3]);
        }
        ctx.stroke();
      }
      start();
    }

    state.setActive = function(active) {
      const nextActive = Boolean(active);
      if (state.active === nextActive) {
        if (nextActive) start();
        return;
      }
      state.active = nextActive;
      if (!nextActive) {
        stop();
        clear();
      } else if (state.grid) {
        seed();
        start();
      }
    };
    state.setGrid = function(grid) {
      state.grid = grid;
      if (state.active) {
        seed();
        start();
      }
    };
    new ResizeObserver(resize).observe(el);
    map.on("movestart", () => {
      state.moving = true;
      stop();
      clear();
    });
    map.on("moveend", () => {
      state.moving = false;
      if (!state.active || !state.grid) return;
      seed();
      start();
    });
    resize();
    return state;
  }

  async function updateWind(message) {
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    while (!el.map && attempts++ < 40) await new Promise(resolve => setTimeout(resolve, 100));
    if (!el.map) return;
    let overlay = overlays.get(message.mapId);
    if (!overlay) {
      overlay = makeOverlay(el, el.map);
      overlays.set(message.mapId, overlay);
    }
    const token = ++overlay.token;
    overlay.setActive(Boolean(message.active));
    if (!message.active || !message.url) return;
    try {
      const grid = parseWind(await loadJson(message.url));
      if (token === overlay.token && grid) overlay.setGrid(grid);
    } catch (error) {
      console.warn("Camada de vento indisponivel:", error);
      overlay.setActive(false);
    }
  }

  function makeLightningOverlay(el, map) {
    const canvas = document.createElement("canvas");
    canvas.className = "lightning-canvas";
    canvas.setAttribute("aria-hidden", "true");
    el.appendChild(canvas);
    const ctx = canvas.getContext("2d");
    const state = {
      canvas, ctx, flashes: [], active: false, moving: false,
      frame: null, lastDraw: 0, windowSeconds: 300,
      initialized: false, knownKeys: new Set()
    };

    function resize() {
      const dpr = Math.min(window.devicePixelRatio || 1, 2);
      const rect = el.getBoundingClientRect();
      canvas.width = Math.max(1, Math.round(rect.width * dpr));
      canvas.height = Math.max(1, Math.round(rect.height * dpr));
      canvas.style.width = rect.width + "px";
      canvas.style.height = rect.height + "px";
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      clear();
    }

    function clear() {
      ctx.clearRect(0, 0, el.clientWidth, el.clientHeight);
    }

    function stop() {
      if (state.frame) cancelAnimationFrame(state.frame);
      state.frame = null;
      clear();
    }

    function start() {
      if (!state.active || state.moving || state.frame) return;
      state.frame = requestAnimationFrame(draw);
    }

    function draw(timestamp) {
      state.frame = null;
      if (!state.active || state.moving || (typeof map.isMoving === "function" && map.isMoving())) {
        clear();
        return;
      }
      if (timestamp - state.lastDraw < 33) {
        start();
        return;
      }
      state.lastDraw = timestamp;
      clear();
      const now = Date.now() / 1000;
      const width = el.clientWidth;
      const height = el.clientHeight;
      const visible = [];
      const ageBands = 20;
      const energyBands = 3;
      const burstBands = 18;
      const residualBuckets = Array.from(
        {length: ageBands * energyBands},
        () => ({glow: new Path2D(), core: new Path2D(), count: 0})
      );
      const burstBuckets = Array.from(
        {length: burstBands * energyBands},
        () => ({ring: new Path2D(), flash: new Path2D(), ringCount: 0, flashCount: 0})
      );
      const animationNow = performance.now() / 1000;
      ctx.globalCompositeOperation = "lighter";

      for (let index = 0; index < state.flashes.length; index++) {
        const flash = state.flashes[index];
        const age = Math.max(0, now - flash.observedAt);
        if (!Number.isFinite(age) || age > state.windowSeconds) continue;
        visible.push(flash);
        const burstAge = Number.isFinite(flash.burstAt) ? animationNow - flash.burstAt : Infinity;
        if (burstAge < 0) continue;
        const point = map.project([flash.lon, flash.lat]);
        if (
          !Number.isFinite(point.x) || !Number.isFinite(point.y) ||
          point.x < -20 || point.y < -20 || point.x > width + 20 || point.y > height + 20
        ) continue;

        const energyLevel = Math.max(0, Math.min(1, (Math.log10(Math.max(flash.energy, 1e-15)) + 15) / 4));
        const ageBand = Math.min(ageBands - 1, Math.floor(age / state.windowSeconds * ageBands));
        const energyBand = Math.min(energyBands - 1, Math.floor(energyLevel * energyBands));
        const residualIndex = ageBand * energyBands + energyBand;
        const coreRadius = .9 + energyBand * .5;
        const glowRadius = coreRadius + 1.8;
        residualBuckets[residualIndex].glow.moveTo(point.x + glowRadius, point.y);
        residualBuckets[residualIndex].glow.arc(point.x, point.y, glowRadius, 0, Math.PI * 2);
        residualBuckets[residualIndex].core.moveTo(point.x + coreRadius, point.y);
        residualBuckets[residualIndex].core.arc(point.x, point.y, coreRadius, 0, Math.PI * 2);
        residualBuckets[residualIndex].count++;

        if (burstAge <= 2) {
          const progress = Math.max(0, Math.min(1, burstAge / 2));
          const progressBand = Math.min(burstBands - 1, Math.floor(progress * burstBands));
          const burstIndex = progressBand * energyBands + energyBand;
          const eased = 1 - Math.pow(1 - progress, 3);
          const ringRadius = coreRadius + 2.2 + eased * 10;
          burstBuckets[burstIndex].ring.moveTo(point.x + ringRadius, point.y);
          burstBuckets[burstIndex].ring.arc(point.x, point.y, ringRadius, 0, Math.PI * 2);
          burstBuckets[burstIndex].ringCount++;
          if (burstAge <= .4) {
            const flashProgress = burstAge / .4;
            const flashRadius = coreRadius + (1 - flashProgress) * 3;
            burstBuckets[burstIndex].flash.moveTo(point.x + flashRadius, point.y);
            burstBuckets[burstIndex].flash.arc(point.x, point.y, flashRadius, 0, Math.PI * 2);
            burstBuckets[burstIndex].flashCount++;
          }
        }
      }

      state.flashes = visible;
      for (let ageBand = ageBands - 1; ageBand >= 0; ageBand--) {
        const fade = Math.pow(1 - (ageBand + .5) / ageBands, 1.35);
        for (let energyBand = 0; energyBand < energyBands; energyBand++) {
          const bucket = residualBuckets[ageBand * energyBands + energyBand];
          if (!bucket.count) continue;
          const alpha = fade * (.07 + energyBand * .04);
          ctx.fillStyle = `rgba(255,177,43,${alpha})`;
          ctx.fill(bucket.glow);
          ctx.fillStyle = `rgba(255,246,181,${Math.min(1, alpha * 2.7)})`;
          ctx.fill(bucket.core);
        }
      }

      for (let progressBand = 0; progressBand < burstBands; progressBand++) {
        const progress = (progressBand + .5) / burstBands;
        const flashProgress = Math.min(1, progress * 5);
        for (let energyBand = 0; energyBand < energyBands; energyBand++) {
          const bucket = burstBuckets[progressBand * energyBands + energyBand];
          if (bucket.ringCount) {
            ctx.strokeStyle = `rgba(255,190,61,${Math.pow(1 - progress, 1.35) * (.55 + energyBand * .1)})`;
            ctx.lineWidth = 1.2 + energyBand * .3;
            ctx.stroke(bucket.ring);
          }
          if (bucket.flashCount) {
            ctx.fillStyle = `rgba(255,252,220,${(1 - flashProgress) * (.76 + energyBand * .1)})`;
            ctx.fill(bucket.flash);
          }
        }
      }
      ctx.globalCompositeOperation = "source-over";
      if (state.flashes.length) start();
    }

    state.setActive = function(active) {
      state.active = Boolean(active);
      if (state.active) start();
      else stop();
    };
    state.setFlashes = function(flashes, windowSeconds) {
      state.windowSeconds = Number(windowSeconds) || 300;
      if (!flashes.length) {
        state.flashes = [];
        state.knownKeys.clear();
        state.initialized = false;
        clear();
        return;
      }

      const previous = new Map(state.flashes.map(flash => [flash.key, flash]));
      const firstLoad = !state.initialized || !state.flashes.length;
      const newFlashes = [];
      for (const flash of flashes) {
        const prior = previous.get(flash.key);
        flash.burstAt = prior && Number.isFinite(prior.burstAt) ? prior.burstAt : null;
        if (!firstLoad && !state.knownKeys.has(flash.key)) newFlashes.push(flash);
      }

      if (newFlashes.length) {
        newFlashes.sort((a, b) => a.observedAt - b.observedAt);
        const firstTime = newFlashes[0].observedAt;
        const lastTime = newFlashes[newFlashes.length - 1].observedAt;
        const span = lastTime - firstTime;
        const animationStart = performance.now() / 1000;
        for (let index = 0; index < newFlashes.length; index++) {
          const sequence = span > 0
            ? (newFlashes[index].observedAt - firstTime) / span
            : index / Math.max(1, newFlashes.length - 1);
          newFlashes[index].burstAt = animationStart + sequence * 4;
        }
      }

      state.flashes = flashes;
      state.knownKeys = new Set(flashes.map(flash => flash.key));
      state.initialized = true;
      if (state.active) start();
    };
    new ResizeObserver(resize).observe(el);
    map.on("movestart", () => {
      state.moving = true;
      stop();
    });
    map.on("moveend", () => {
      state.moving = false;
      start();
    });
    resize();
    return state;
  }

  function messageArray(value) {
    if (Array.isArray(value)) return value;
    return value == null ? [] : [value];
  }

  async function updateLightning(message) {
    const el = getMapElement(message.mapId);
    if (!el) return;
    let overlay = lightningOverlays.get(message.mapId);
    if (!message.active && !overlay) return;
    let attempts = 0;
    while (!el.map && attempts++ < 80) await new Promise(resolve => setTimeout(resolve, 100));
    if (!el.map) return;
    if (!overlay) {
      overlay = makeLightningOverlay(el, el.map);
      lightningOverlays.set(message.mapId, overlay);
    }
    overlay.setActive(Boolean(message.active));
    if (!message.active) {
      overlay.setFlashes([], message.windowSeconds);
      return;
    }

    const columns = message.flashes || {};
    const longitude = messageArray(columns.lon);
    const latitude = messageArray(columns.lat);
    const energy = messageArray(columns.energy);
    const observedAt = messageArray(columns.observedAt);
    const length = Math.min(longitude.length, latitude.length, energy.length, observedAt.length);
    const flashes = [];
    for (let index = 0; index < length; index++) {
      const flash = {
        lon: Number(longitude[index]),
        lat: Number(latitude[index]),
        energy: Number(energy[index]),
        observedAt: Number(observedAt[index])
      };
      if (Object.values(flash).every(Number.isFinite)) {
        flash.key = [
          flash.observedAt,
          flash.lat.toFixed(5),
          flash.lon.toFixed(5),
          flash.energy.toExponential(4)
        ].join("|");
        flashes.push(flash);
      }
    }
    overlay.setFlashes(flashes, message.windowSeconds);
  }

  function makeFirePulse(map, layerId) {
    const glowLayerId = layerId + "-pulse";
    const state = {
      active: false,
      frame: null,
      lastDraw: 0
    };

    function ensureGlowLayer() {
      const pointLayer = map.getLayer(layerId);
      if (!pointLayer) return false;
      if (!map.getLayer(glowLayerId)) {
        const glowLayer = {
          id: glowLayerId,
          type: "circle",
          source: pointLayer.source,
          paint: {
            "circle-radius": 4,
            "circle-color": "#ff2419",
            "circle-opacity": .24,
            "circle-blur": .65,
            "circle-stroke-width": 0
          }
        };
        if (pointLayer["source-layer"]) glowLayer["source-layer"] = pointLayer["source-layer"];
        if (pointLayer.filter) glowLayer.filter = pointLayer.filter;
        map.addLayer(glowLayer, layerId);
      }
      return true;
    }

    function stop() {
      if (state.frame) cancelAnimationFrame(state.frame);
      state.frame = null;
    }

    function start() {
      if (!state.active || state.frame) return;
      state.frame = requestAnimationFrame(draw);
    }

    function draw(timestamp) {
      state.frame = null;
      if (!state.active || !ensureGlowLayer()) return;
      if (timestamp - state.lastDraw < 40) {
        start();
        return;
      }
      state.lastDraw = timestamp;
      const phase = (timestamp / 1000 * .25) % 1;
      const pulse = (1 - Math.cos(phase * Math.PI * 2)) / 2;
      map.setPaintProperty(glowLayerId, "circle-radius", 4 + pulse * 5.5);
      map.setPaintProperty(glowLayerId, "circle-opacity", .25 * (1 - pulse * .78));
      map.setPaintProperty(glowLayerId, "circle-blur", .62 + pulse * .22);
      map.setPaintProperty(layerId, "circle-radius", 2.35 + pulse * .45);
      map.setPaintProperty(layerId, "circle-opacity", .94 - pulse * .08);
      start();
    }

    state.setActive = function(active) {
      state.active = Boolean(active);
      if (!ensureGlowLayer()) return;
      map.setLayoutProperty(glowLayerId, "visibility", state.active ? "visible" : "none");
      if (state.active) start();
      else stop();
    };
    return state;
  }

  async function updateFirePulse(message) {
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    const layerId = message.layerId || "fires";
    while ((!el.map || !el.map.isStyleLoaded() || !el.map.getLayer(layerId)) && attempts++ < 80) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map || !el.map.getLayer(layerId)) return;
    const stateKey = message.mapId + ":" + layerId;
    let state = firePulseStates.get(stateKey);
    if (!state) {
      state = makeFirePulse(el.map, layerId);
      firePulseStates.set(stateKey, state);
    }
    state.setActive(Boolean(message.active));
  }

  async function updateRaster(message) {
    const token = String(message.token || "");
    latestRasterTokens.set(message.mapId, token);
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    while ((!el.map || !el.map.isStyleLoaded()) && attempts++ < 40) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map || !el.map.isStyleLoaded()) return;
    if (latestRasterTokens.get(message.mapId) !== token) return;

    const acknowledge = (() => {
      let sent = false;
      return (ok, error) => {
        if (sent) return;
        sent = true;
        Shiny.setInputValue(message.mapId + "_raster_ready", {
          token: message.token,
          ok: ok,
          error: error || null,
          timestamp: Date.now()
        }, {priority: "event"});
      };
    })();

    try {
      await loadImage(message.url);
      if (latestRasterTokens.get(message.mapId) !== token) return;

      const map = el.map;
      const source = map.getSource("forecast");
      map.once("idle", () => {
        if (latestRasterTokens.get(message.mapId) === token) acknowledge(true);
      });
      if (source && typeof source.updateImage === "function") {
        source.updateImage({url: message.url, coordinates: message.coordinates});
      } else {
        if (map.getLayer("forecast")) map.removeLayer("forecast");
        if (map.getSource("forecast")) map.removeSource("forecast");
        map.addSource("forecast", {
          type: "image",
          url: message.url,
          coordinates: message.coordinates
        });
        map.addLayer({
          id: "forecast",
          type: "raster",
          source: "forecast",
          layout: {
            visibility: (forecastOpacityRequests.get(message.mapId) ?? .82) <= 0 ? "none" : "visible"
          },
          paint: {
            "raster-opacity": forecastOpacityRequests.get(message.mapId) ?? .82,
            "raster-fade-duration": 120,
            "raster-resampling": "linear"
          }
        });
      }
      map.triggerRepaint();
      window.setTimeout(() => {
        if (latestRasterTokens.get(message.mapId) === token) acknowledge(true);
      }, 3000);
    } catch (error) {
      console.error("Falha ao atualizar raster:", error);
      acknowledge(false, String(error));
    }
  }

  async function updateForecastOpacity(message) {
    const requested = Number(message.opacity);
    const opacity = Number.isFinite(requested) ? Math.max(0, Math.min(1, requested)) : .82;
    forecastOpacityRequests.set(message.mapId, opacity);
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    while ((!el.map || !el.map.isStyleLoaded()) && attempts++ < 80) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map || !el.map.isStyleLoaded()) return;
    if (el.map.getLayer("forecast")) {
      el.map.setLayoutProperty("forecast", "visibility", opacity <= 0 ? "none" : "visible");
      el.map.setPaintProperty("forecast", "raster-opacity", opacity);
      el.map.triggerRepaint();
    }
  }

  function weatherMetadataUrl(tileUrl) {
    return String(tileUrl || "")
      .replace("{z}", "0")
      .replace("{y}", "0")
      .replace("{x}", "0");
  }

  async function reportWeatherObservationTime(message) {
    if (!message.timeInputId || !message.url) return;
    try {
      const response = await fetch(weatherMetadataUrl(message.url), {
        method: "HEAD",
        cache: "no-store"
      });
      if (!response.ok) return;
      const observedAt = response.headers.get("layer-time-actual");
      if (observedAt && window.Shiny && typeof Shiny.setInputValue === "function") {
        Shiny.setInputValue(message.timeInputId, {
          observedAt: observedAt,
          sourceId: message.sourceId,
          productId: message.productId,
          receivedAt: Date.now()
        }, {priority: "event"});
      }
    } catch (error) {
      console.debug("Horario da imagem meteorologica indisponivel:", error);
    }
  }

  async function updateWeatherObservation(message) {
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    while ((!el.map || !el.map.isStyleLoaded()) && attempts++ < 80) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map || !el.map.isStyleLoaded()) return;

    const map = el.map;
    const layerId = "weather-observation";
    const sourceId = "weather-observation";
    const active = Boolean(message.active && message.url);
    const signature = [message.sourceId, message.productId, message.url, message.refreshKey].join("|");
    const previous = weatherObservationStates.get(message.mapId);
    const shouldRefresh = !previous || previous.signature !== signature || !previous.active;

    if (!active) {
      if (map.getLayer(layerId)) map.setLayoutProperty(layerId, "visibility", "none");
      weatherObservationStates.set(message.mapId, Object.assign({}, previous, {active: false}));
      return;
    }

    try {
      if (shouldRefresh || !map.getSource(sourceId)) {
        if (map.getLayer(layerId)) map.removeLayer(layerId);
        if (map.getSource(sourceId)) map.removeSource(sourceId);
        map.addSource(sourceId, {
          type: "raster",
          tiles: [message.url],
          tileSize: 256,
          maxzoom: Number(message.maxzoom) || 7,
          attribution: message.attribution || "NASA GIBS · NOAA GOES-East"
        });
        const beforeId = map.getLayer("forecast") ? "forecast" : undefined;
        map.addLayer({
          id: layerId,
          type: "raster",
          source: sourceId,
          layout: {visibility: "visible"},
          paint: {
            "raster-opacity": Number(message.opacity) || .78,
            "raster-fade-duration": 300,
            "raster-resampling": "linear"
          }
        }, beforeId);
      } else {
        map.setLayoutProperty(layerId, "visibility", "visible");
        map.setPaintProperty(layerId, "raster-opacity", Number(message.opacity) || .78);
      }
      weatherObservationStates.set(message.mapId, {active: true, signature: signature});
      map.triggerRepaint();
      if (shouldRefresh) reportWeatherObservationTime(message);
    } catch (error) {
      console.warn("Imagem meteorologica indisponivel:", error);
    }
  }

  function applyMapLanguage(el, language) {
    if (!el.map || !el.map.isStyleLoaded()) return false;
    const textField = [
      "coalesce",
      ["get", "name:" + language],
      ["get", "name"],
      ["get", "name:en"],
      ["get", "name_en"]
    ];

    for (const layer of el.map.getStyle().layers || []) {
      const current = layer.layout && layer.layout["text-field"];
      if (layer.type !== "symbol" || current == null) continue;
      const serialized = JSON.stringify(current).toLowerCase();
      if (!serialized.includes("name") || serialized.includes("housenumber")) continue;
      try {
        el.map.setLayoutProperty(layer.id, "text-field", textField);
      } catch (error) {
        console.debug("Rotulo nao localizado:", layer.id, error);
      }
    }
    return true;
  }

  function bindMapLanguageLifecycle(el, mapId) {
    if (!el.map || el._alertarLanguageMap === el.map) return;
    el._alertarLanguageMap = el.map;
    const reapply = () => {
      const language = mapLanguageRequests.get(mapId);
      if (language) applyMapLanguage(el, language);
    };
    el.map.on("load", reapply);
    el.map.on("style.load", reapply);
  }

  async function localizeMap(message) {
    const mapId = message.mapId;
    const language = String(message.language || "pt").toLowerCase();
    mapLanguageRequests.set(mapId, language);
    const el = getMapElement(mapId);
    if (!el) return;
    let attempts = 0;
    while ((!el.map || !el.map.isStyleLoaded()) && attempts++ < 80) {
      if (el.map) bindMapLanguageLifecycle(el, mapId);
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map) return;
    bindMapLanguageLifecycle(el, mapId);
    applyMapLanguage(el, language);

    // MapLibre and the remote CARTO style can finish initialization in
    // different orders on the first visit. Reapply after those late stages.
    for (const delay of [250, 900, 1800]) {
      window.setTimeout(() => {
        if (mapLanguageRequests.get(mapId) === language) applyMapLanguage(el, language);
      }, delay);
    }
  }

  function updateInterface(message) {
    document.documentElement.lang = message.language || "pt";
    if (message.title) document.title = message.title;

    for (const [id, value] of Object.entries(message.text || {})) {
      const element = document.getElementById(id);
      if (element) element.textContent = value;
    }

    for (const [selector, value] of Object.entries(message.mapControls || {})) {
      for (const element of document.querySelectorAll(selector)) {
        element.title = value;
        element.setAttribute("aria-label", value);
      }
    }

    const territory = document.getElementById("territory");
    if (territory && territory.selectize && message.territoryPlaceholder) {
      territory.selectize.settings.placeholder = message.territoryPlaceholder;
      territory.selectize.updatePlaceholder();
    }

    const timezone = document.getElementById("timezone");
    if (timezone && message.timezoneLabel) {
      timezone.title = message.timezoneLabel;
      timezone.setAttribute("aria-label", message.timezoneLabel);
    }

    const forecastChart = document.getElementById("forecast_spark");
    if (forecastChart && message.chartLabel) {
      forecastChart.title = message.chartLabel;
      forecastChart.setAttribute("aria-label", message.chartLabel);
    }

    const totemButton = document.getElementById("toggle_totem");
    if (totemButton && message.totemToggle) {
      totemButton.dataset.enterLabel = message.totemToggle.enter;
      totemButton.dataset.exitLabel = message.totemToggle.exit;
      updateTotemButton();
    }

    const detailsButton = document.getElementById("toggle_details");
    if (detailsButton && message.detailsToggle) {
      detailsButton.dataset.minimizeLabel = message.detailsToggle.minimize;
      detailsButton.dataset.restoreLabel = message.detailsToggle.restore;
      const minimized = detailsButton.getAttribute("aria-expanded") === "false";
      const label = minimized ? message.detailsToggle.restore : message.detailsToggle.minimize;
      detailsButton.title = label;
      detailsButton.setAttribute("aria-label", label);
    }

  }

  function updateTerritorySelection(message) {
    const input = document.getElementById(message.inputId || "territory");
    if (!input || !input.selectize || message.value == null) return;

    const selectize = input.selectize;
    const value = String(message.value);
    const valueField = selectize.settings.valueField || "value";
    const labelField = selectize.settings.labelField || "label";
    if (!selectize.options[value]) {
      const option = {};
      option[valueField] = value;
      option[labelField] = message.label || value;
      selectize.addOption(option);
    }
    selectize.setValue(value);
  }

  function toggleDetailsPanel() {
    const body = document.getElementById("place-panel-body");
    const button = document.getElementById("toggle_details");
    if (!body || !button) return;

    const minimized = !body.hidden;
    body.hidden = minimized;
    button.textContent = minimized ? "+" : "−";
    button.setAttribute("aria-expanded", String(!minimized));
    button.closest(".place-panel")?.classList.toggle("is-minimized", minimized);

    const label = minimized
      ? (button.dataset.restoreLabel || "Restaurar painel")
      : (button.dataset.minimizeLabel || "Minimizar painel");
    button.title = label;
    button.setAttribute("aria-label", label);
  }

  function bindDetailsToggle() {
    const button = document.getElementById("toggle_details");
    if (!button || button.dataset.toggleBound === "true") return;
    button.dataset.toggleBound = "true";
    button.addEventListener("click", toggleDetailsPanel);
    if (typeof window.matchMedia === "function" && window.matchMedia("(max-width: 520px), (max-height: 620px)").matches) {
      const body = document.getElementById("place-panel-body");
      if (body && !body.hidden) toggleDetailsPanel();
    }
  }

  function minimizeMapAttribution(root) {
    const attribution = root.querySelector(".maplibregl-ctrl-attrib");
    if (!attribution || attribution.dataset.initialCompact === "true") return;
    attribution.classList.add("maplibregl-compact");
    attribution.classList.remove("maplibregl-compact-show");
    attribution.removeAttribute("open");
    attribution.dataset.initialCompact = "true";
  }

  function bindMapAttribution() {
    const mapRoot = document.getElementById("forecast_map");
    if (!mapRoot || mapRoot.dataset.attributionObserverBound === "true") return;
    mapRoot.dataset.attributionObserverBound = "true";
    minimizeMapAttribution(mapRoot);
    new MutationObserver(() => minimizeMapAttribution(mapRoot)).observe(mapRoot, {
      childList: true,
      subtree: true
    });
  }

  function seededRandom(seed) {
    let value = seed >>> 0;
    return function () {
      value += 0x6D2B79F5;
      let result = value;
      result = Math.imul(result ^ (result >>> 15), result | 1);
      result ^= result + Math.imul(result ^ (result >>> 7), result | 61);
      return ((result ^ (result >>> 14)) >>> 0) / 4294967296;
    };
  }

  function drawStarField(canvas, root) {
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const width = Math.max(1, root.clientWidth);
    const height = Math.max(1, root.clientHeight);
    canvas.width = Math.round(width * dpr);
    canvas.height = Math.round(height * dpr);
    canvas.style.width = width + "px";
    canvas.style.height = height + "px";

    const ctx = canvas.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, width, height);
    const random = seededRandom(0xA17EA5);
    const count = Math.max(180, Math.min(620, Math.round(width * height / 4200)));

    for (let index = 0; index < count; index++) {
      const x = random() * width;
      const y = random() * height;
      const bright = random() > .94;
      const radius = bright ? .85 + random() * 1.15 : .25 + random() * .62;
      const alpha = bright ? .55 + random() * .32 : .18 + random() * .46;

      if (bright) {
        const glow = ctx.createRadialGradient(x, y, 0, x, y, radius * 4.5);
        glow.addColorStop(0, `rgba(205, 234, 255, ${alpha})`);
        glow.addColorStop(.18, `rgba(152, 211, 239, ${alpha * .55})`);
        glow.addColorStop(1, "rgba(91, 165, 203, 0)");
        ctx.fillStyle = glow;
        ctx.beginPath();
        ctx.arc(x, y, radius * 4.5, 0, Math.PI * 2);
        ctx.fill();
      } else {
        ctx.fillStyle = `rgba(216, 237, 248, ${alpha})`;
        ctx.beginPath();
        ctx.arc(x, y, radius, 0, Math.PI * 2);
        ctx.fill();
      }
    }
  }

  async function bindStarField() {
    const root = document.getElementById("forecast_map");
    if (!root || root.dataset.starFieldBound === "true") return;
    root.dataset.starFieldBound = "true";

    const canvas = document.createElement("canvas");
    canvas.className = "star-field";
    canvas.setAttribute("aria-hidden", "true");
    root.prepend(canvas);
    drawStarField(canvas, root);
    new ResizeObserver(() => drawStarField(canvas, root)).observe(root);

    let attempts = 0;
    while (!root.map && attempts++ < 50) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!root.map || typeof root.map.setSky !== "function") return;

    const applySpace = () => {
      try {
        root.map.setSky({
          "sky-color": "rgba(2, 6, 10, 0.08)",
          "horizon-color": "rgba(17, 42, 53, 0.74)",
          "fog-color": "rgba(18, 36, 45, 0.54)",
          "sky-horizon-blend": .42,
          "horizon-fog-blend": .7,
          "atmosphere-blend": ["interpolate", ["linear"], ["zoom"], 0, .72, 6, .4, 10, 0]
        });
      } catch (error) {
        console.debug("Não foi possível configurar o céu do globo.", error);
      }
    };
    applySpace();
    root.map.on("style.load", applySpace);
  }

  function fullscreenElement() {
    return document.fullscreenElement || document.webkitFullscreenElement || null;
  }

  function updateTotemButton() {
    const button = document.getElementById("toggle_totem");
    if (!button) return;
    const label = totemActive
      ? (button.dataset.exitLabel || "Sair do modo totem")
      : (button.dataset.enterLabel || "Modo totem");
    button.classList.toggle("is-active", totemActive);
    button.setAttribute("aria-pressed", String(totemActive));
    button.setAttribute("aria-label", label);
    button.title = label;
    const text = document.getElementById("label-totem");
    if (text) text.textContent = label;
  }

  function notifyTotemState() {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    Shiny.setInputValue("totem_mode", totemActive, {priority: "event"});
  }

  async function requestAppFullscreen() {
    const target = document.querySelector(".app-shell");
    if (!target || fullscreenElement()) return;
    const request = target.requestFullscreen || target.webkitRequestFullscreen;
    if (!request) return;
    try {
      await request.call(target);
    } catch (error) {
      console.warn("Tela cheia indisponível; mantendo o modo totem na janela.", error);
    }
  }

  async function exitAppFullscreen() {
    if (!fullscreenElement()) return;
    const exit = document.exitFullscreen || document.webkitExitFullscreen;
    if (!exit) return;
    try {
      await exit.call(document);
    } catch (error) {
      console.warn("Não foi possível sair da tela cheia.", error);
    }
  }

  function setTotemMode(active, manageFullscreen = true, notify = true) {
    totemActive = Boolean(active);
    document.body.classList.toggle("totem-mode", totemActive);
    updateTotemButton();
    if (manageFullscreen) {
      if (totemActive) requestAppFullscreen();
      else exitAppFullscreen();
    }
    if (notify) notifyTotemState();
  }

  function urlRequestsTotemMode() {
    const parameters = new URLSearchParams(window.location.search);
    if (!parameters.has("totem")) return false;
    const value = String(parameters.get("totem") || "").trim().toLowerCase();
    return value === "" || ["1", "true", "yes", "on", "sim"].includes(value);
  }

  function applyUrlStartupMode() {
    if (urlTotemApplied || !urlRequestsTotemMode()) return;
    urlTotemApplied = true;
    // Fullscreen requests require a user gesture in regular browsers. A browser
    // launched in kiosk mode is already fullscreen, so only activate app behavior.
    setTotemMode(true, false, true);
  }

  function bindTotemToggle() {
    const button = document.getElementById("toggle_totem");
    if (!button || button.dataset.toggleBound === "true") return;
    button.dataset.toggleBound = "true";
    button.addEventListener("click", () => setTotemMode(!totemActive));
    if (!fullscreenEventsBound) {
      fullscreenEventsBound = true;
      const handleFullscreenChange = () => {
        if (totemActive && !fullscreenElement()) setTotemMode(false, false, true);
      };
      document.addEventListener("fullscreenchange", handleFullscreenChange);
      document.addEventListener("webkitfullscreenchange", handleFullscreenChange);
    }
  }

  function bindCompactNativeSelects() {
    for (const select of document.querySelectorAll("select.compact-native-select")) {
      if (select.dataset.compactLabelsBound === "true") continue;
      select.dataset.compactLabelsBound = "true";
      const setExpanded = expanded => {
        for (const option of select.options) {
          const label = expanded ? option.dataset.fullLabel : option.dataset.shortLabel;
          if (label) option.textContent = label;
        }
      };
      const collapse = () => setExpanded(false);
      select.addEventListener("mousedown", () => setExpanded(true));
      select.addEventListener("touchstart", () => setExpanded(true), {passive: true});
      select.addEventListener("keydown", event => {
        if (["Enter", " ", "ArrowDown", "ArrowUp"].includes(event.key)) setExpanded(true);
      });
      select.addEventListener("change", collapse);
      select.addEventListener("blur", collapse);
      collapse();
    }
  }

  function bindLocalControls() {
    bindDetailsToggle();
    bindMapAttribution();
    bindStarField();
    bindTotemToggle();
    bindCompactNativeSelects();
  }

  function registerHandlers() {
    if (handlersRegistered || !window.Shiny) return;
    handlersRegistered = true;
    Shiny.addCustomMessageHandler("alertar:wind", updateWind);
    Shiny.addCustomMessageHandler("alertar:lightning", updateLightning);
    Shiny.addCustomMessageHandler("alertar:fire-pulse", updateFirePulse);
    Shiny.addCustomMessageHandler("alertar:raster", updateRaster);
    Shiny.addCustomMessageHandler("alertar:forecast-opacity", updateForecastOpacity);
    Shiny.addCustomMessageHandler("alertar:weather-observation", updateWeatherObservation);
    Shiny.addCustomMessageHandler("alertar:preload", preloadResources);
    Shiny.addCustomMessageHandler("alertar:language", localizeMap);
    Shiny.addCustomMessageHandler("alertar:interface", updateInterface);
    Shiny.addCustomMessageHandler("alertar:territory-selection", updateTerritorySelection);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bindLocalControls, {once: true});
  } else {
    bindLocalControls();
  }

  function initializeShinySession() {
    registerHandlers();
    applyUrlStartupMode();
  }

  if (window.Shiny) {
    registerHandlers();
    if (Shiny.initializedPromise && typeof Shiny.initializedPromise.then === "function") {
      Shiny.initializedPromise.then(initializeShinySession);
    }
  }

  // Shiny dispatches its lifecycle events through jQuery. Keep this fallback
  // for versions that do not expose initializedPromise.
  if (window.jQuery) {
    window.jQuery(document).one("shiny:sessioninitialized", initializeShinySession);
  }
})();
