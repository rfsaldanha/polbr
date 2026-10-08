(function () {
  "use strict";
  const states = new Map();
  let registered = false;
  const antenna = '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M9 20h6M12 10v10M8.5 6.5a5 5 0 0 0 0 7M15.5 6.5a5 5 0 0 1 0 7M5.5 3.5a9 9 0 0 0 0 13M18.5 3.5a9 9 0 0 1 0 13"/><circle cx="12" cy="10" r="1.5"/></svg>';

  async function update(message) {
    const el = document.getElementById(message.mapId);
    if (!el) return;
    let state = states.get(message.mapId);
    if (!state || state.el !== el) {
      if (state) for (const marker of state.markers.values()) marker.remove();
      state = {el, markers: new Map(), token: 0, map: null};
      states.set(message.mapId, state);
    }
    const token = ++state.token;
    for (let tries = 0; tries < 80 && (!el.map || !el.map.isStyleLoaded()); tries++) {
      await new Promise(resolve => setTimeout(resolve, 100));
      if (token !== state.token) return;
    }
    if (token !== state.token || !el.map || !window.maplibregl) return;
    if (state.map !== el.map) {
      for (const marker of state.markers.values()) marker.remove();
      state.markers.clear();
      state.map = el.map;
      el.map.once("remove", () => {
        for (const marker of state.markers.values()) marker.remove();
        state.markers.clear();
        if (states.get(message.mapId) === state) states.delete(message.mapId);
      });
    }
    const wanted = new Set();
    for (const station of message.active ? (message.stations || []) : []) {
      if (!Number.isFinite(station.lon) || !Number.isFinite(station.lat)) continue;
      wanted.add(station.id);
      let marker = state.markers.get(station.id);
      if (!marker) {
        const container = document.createElement("div");
        container.className = "fioares-marker";
        container.dataset.stationId = station.id;
        const button = document.createElement("button");
        button.type = "button";
        button.className = "fioares-marker-button";
        button.innerHTML = antenna;
        button.addEventListener("click", event => {
          event.stopPropagation();
          if (el.classList.contains("is-distance-measuring")) return;
          if (window.Shiny) Shiny.setInputValue(message.inputId, station.id, {priority: "event"});
        });
        button.addEventListener("dblclick", event => event.stopPropagation());
        container.appendChild(button);
        marker = new maplibregl.Marker({element: container, anchor: "center"}).setLngLat([station.lon, station.lat]).addTo(el.map);
        state.markers.set(station.id, marker);
      }
      marker.setLngLat([station.lon, station.lat]);
      const button = marker.getElement().querySelector("button");
      button.style.setProperty("--station-color", station.color);
      button.title = station.title;
      button.setAttribute("aria-label", station.label);
      button.dataset.state = station.state;
    }
    for (const [id, marker] of state.markers) {
      if (!wanted.has(id)) { marker.remove(); state.markers.delete(id); }
    }
  }

  function register() {
    if (registered || !window.Shiny) return;
    Shiny.addCustomMessageHandler("alertar:fioares", update);
    registered = true;
  }
  if (window.jQuery) window.jQuery(document).on("shiny:connected", register);
  document.addEventListener("DOMContentLoaded", register);
  register();
})();
