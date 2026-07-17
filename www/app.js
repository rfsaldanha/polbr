(function () {
  "use strict";

  const overlays = new Map();
  const rasterPreloads = new Map();
  const jsonPreloads = new Map();

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
    return remember(rasterPreloads, url, promise, 24);
  }

  function loadJson(url) {
    if (jsonPreloads.has(url)) return jsonPreloads.get(url);
    const promise = fetch(url, {cache: "force-cache"}).then(response => {
      if (!response.ok) throw new Error("HTTP " + response.status);
      return response.json();
    });
    return remember(jsonPreloads, url, promise, 16);
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
    const state = {el, map, canvas, ctx, grid: null, active: false, frame: null, particles: [], token: 0};
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
      ctx.clearRect(0, 0, el.clientWidth, el.clientHeight);
    }

    function color(speed) {
      if (speed < 2) return "rgba(86,165,190,.42)";
      if (speed < 5) return "rgba(83,217,199,.58)";
      if (speed < 9) return "rgba(190,239,121,.72)";
      return "rgba(255,226,112,.82)";
    }

    function draw() {
      if (!state.active || !state.grid) return;
      const width = el.clientWidth, height = el.clientHeight;
      ctx.globalCompositeOperation = "destination-in";
      ctx.fillStyle = "rgba(0,0,0,.91)";
      ctx.fillRect(0, 0, width, height);
      ctx.globalCompositeOperation = "source-over";
      ctx.lineWidth = .8;

      for (let i = 0; i < state.particles.length; i++) {
        let p = state.particles[i];
        if (p.age++ > 105) p = state.particles[i] = randomParticle();
        const wind = interpolate(state.grid, p.lon, p.lat);
        if (!wind) { state.particles[i] = randomParticle(); continue; }
        const a = map.project([p.lon, p.lat]);
        const latFactor = Math.max(.25, Math.cos(p.lat * Math.PI / 180));
        const nextLon = p.lon + wind.u * particleAdvection / latFactor;
        const nextLat = p.lat + wind.v * particleAdvection;
        const b = map.project([nextLon, nextLat]);
        if (b.x < 0 || b.y < 0 || b.x > width || b.y > height || Math.abs(b.x-a.x) > 12 || Math.abs(b.y-a.y) > 12) {
          state.particles[i] = randomParticle(); continue;
        }
        ctx.strokeStyle = color(wind.speed);
        ctx.beginPath(); ctx.moveTo(a.x, a.y); ctx.lineTo(b.x, b.y); ctx.stroke();
        p.lon = nextLon; p.lat = nextLat;
      }
      state.frame = requestAnimationFrame(draw);
    }

    state.setActive = function(active) {
      state.active = active;
      if (!active) {
        if (state.frame) cancelAnimationFrame(state.frame);
        state.frame = null;
        ctx.clearRect(0, 0, el.clientWidth, el.clientHeight);
      } else if (!state.frame && state.grid) {
        seed(); draw();
      }
    };
    state.setGrid = function(grid) {
      state.grid = grid;
      if (state.active && !state.frame) { seed(); draw(); }
    };

    new ResizeObserver(resize).observe(el);
    map.on("movestart", () => ctx.clearRect(0, 0, el.clientWidth, el.clientHeight));
    map.on("moveend", seed);
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
    if (!overlay) { overlay = makeOverlay(el, el.map); overlays.set(message.mapId, overlay); }
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

  async function updateRaster(message) {
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    while ((!el.map || !el.map.isStyleLoaded()) && attempts++ < 40) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map || !el.map.isStyleLoaded()) return;

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

      const map = el.map;
      const source = map.getSource("forecast");
      map.once("idle", () => acknowledge(true));
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
          paint: {
            "raster-opacity": .82,
            "raster-fade-duration": 0,
            "raster-resampling": "linear"
          }
        });
      }
      map.triggerRepaint();
      window.setTimeout(() => acknowledge(true), 3000);
    } catch (error) {
      console.error("Falha ao atualizar raster:", error);
      acknowledge(false, String(error));
    }
  }

  async function localizeMap(message) {
    const el = getMapElement(message.mapId);
    if (!el) return;
    let attempts = 0;
    while ((!el.map || !el.map.isStyleLoaded()) && attempts++ < 40) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    if (!el.map || !el.map.isStyleLoaded()) return;

    const language = String(message.language || "pt").toLowerCase();
    const textField = [
      "coalesce",
      ["get", "name:" + language],
      ["get", "name"],
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

  function registerHandlers() {
    Shiny.addCustomMessageHandler("alertar:wind", updateWind);
    Shiny.addCustomMessageHandler("alertar:raster", updateRaster);
    Shiny.addCustomMessageHandler("alertar:preload", preloadResources);
    Shiny.addCustomMessageHandler("alertar:language", localizeMap);
    Shiny.addCustomMessageHandler("alertar:interface", updateInterface);
    Shiny.addCustomMessageHandler("alertar:toggle-details", toggleDetailsPanel);
  }

  if (window.Shiny) {
    registerHandlers();
  } else {
    document.addEventListener("shiny:connected", registerHandlers, {once: true});
  }
})();
