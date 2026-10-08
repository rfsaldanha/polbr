(function () {
  "use strict";
  const maps = new Map();
  let latest = {active: false, stations: []};
  let registered = false;
  const antenna = '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M9 20h6M12 10v10M8.5 6.5a5 5 0 0 0 0 7M15.5 6.5a5 5 0 0 1 0 7M5.5 3.5a9 9 0 0 0 0 13M18.5 3.5a9 9 0 0 1 0 13"/><circle cx="12" cy="10" r="1.5"/></svg>';

  function update(state) {
    if (!state.ready) return;
    const wanted = new Set();
    for (const station of latest.active ? (latest.stations || []) : []) {
      if (!Number.isFinite(station.lon) || !Number.isFinite(station.lat)) continue;
      wanted.add(station.id);
      let entry = state.markers.get(station.id);
      if (!entry) {
        const button = document.createElement("button");
        button.type = "button";
        button.className = "fioares-marker-button";
        button.dataset.stationId = station.id;
        button.innerHTML = antenna;
        L.DomEvent.disableClickPropagation(button);
        L.DomEvent.disableScrollPropagation(button);
        button.addEventListener("click", event => {
          event.stopPropagation();
          if (window.Shiny) Shiny.setInputValue(latest.inputId, station.id, {priority: "event"});
        });
        const marker = L.marker([station.lat, station.lon], {
          icon: L.divIcon({html: "", className: "fioares-marker", iconSize: [26, 26], iconAnchor: [13, 13]}),
          keyboard: false, bubblingMouseEvents: false, zIndexOffset: 500
        }).addTo(state.map);
        marker.getElement().dataset.stationId = station.id;
        marker.getElement().appendChild(button);
        entry = {marker, button};
        state.markers.set(station.id, entry);
      }
      entry.marker.setLatLng([station.lat, station.lon]);
      entry.button.style.setProperty("--station-color", station.color);
      entry.button.title = station.title;
      entry.button.setAttribute("aria-label", station.label);
      entry.button.dataset.state = station.state;
    }
    for (const [id, entry] of state.markers) {
      if (!wanted.has(id)) { entry.marker.remove(); state.markers.delete(id); }
    }
  }

  window.fioaresMaps = {
    register: function (id, map) {
      const existing = maps.get(id);
      if (existing && existing.map === map) { update(existing); return; }
      if (existing) for (const entry of existing.markers.values()) entry.marker.remove();
      const state = {map, markers: new Map(), ready: false};
      maps.set(id, state);
      map.once("unload", () => {
        state.markers.clear();
        if (maps.get(id) === state) maps.delete(id);
      });
      map.whenReady(() => { state.ready = true; update(state); });
    }
  };

  function register() {
    if (registered || !window.Shiny) return;
    Shiny.addCustomMessageHandler("alertar:fioares", message => {
      latest = message;
      for (const state of maps.values()) update(state);
    });
    registered = true;
  }
  if (window.jQuery) window.jQuery(document).on("shiny:connected", register);
  document.addEventListener("DOMContentLoaded", register);
  register();
})();
