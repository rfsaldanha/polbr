(function () {
  "use strict";

  const overlays = new Map();
  const rasterPreloads = new Map();
  const jsonPreloads = new Map();
  const latestRasterTokens = new Map();
  const windPerformanceModes = new Map();
  let totemActive = false;
  let fullscreenEventsBound = false;

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
      performanceMode: false, lastDrawAt: 0,
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
      const baseCount = Math.round((el.clientWidth * el.clientHeight) / 1150);
      const count = state.performanceMode
        ? Math.max(140, Math.min(650, Math.round(baseCount * .5)))
        : Math.max(260, Math.min(1250, baseCount));
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

    function draw(timestamp) {
      state.frame = null;
      const mapIsMoving = typeof map.isMoving === "function" && map.isMoving();
      if (!state.active || !state.grid || state.moving || mapIsMoving) {
        clear();
        return;
      }
      if (state.performanceMode && timestamp - state.lastDrawAt < 1000 / 30) {
        start();
        return;
      }
      state.lastDrawAt = timestamp;
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
    state.setPerformanceMode = function(reduced) {
      const nextMode = Boolean(reduced);
      if (state.performanceMode === nextMode) return;
      state.performanceMode = nextMode;
      state.lastDrawAt = 0;
      if (state.active && state.grid) {
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
      overlay.setPerformanceMode(Boolean(windPerformanceModes.get(message.mapId)));
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

  function updateWindPerformance(message) {
    const reduced = Boolean(message.reduced);
    windPerformanceModes.set(message.mapId, reduced);
    const overlay = overlays.get(message.mapId);
    if (overlay) overlay.setPerformanceMode(reduced);
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
          paint: {
            "raster-opacity": .82,
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

  function bindLocalControls() {
    bindDetailsToggle();
    bindMapAttribution();
    bindStarField();
    bindTotemToggle();
  }

  function registerHandlers() {
    Shiny.addCustomMessageHandler("alertar:wind", updateWind);
    Shiny.addCustomMessageHandler("alertar:wind-performance", updateWindPerformance);
    Shiny.addCustomMessageHandler("alertar:raster", updateRaster);
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

  if (window.Shiny) {
    registerHandlers();
  } else {
    document.addEventListener("shiny:connected", registerHandlers, {once: true});
  }
})();
