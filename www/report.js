(function () {
  "use strict";

  function normalized(value) {
    return String(value == null ? "" : value)
      .normalize("NFD")
      .replace(/[\u0300-\u036f]/g, "")
      .toLocaleLowerCase();
  }

  function appendCell(row, text, className) {
    const cell = document.createElement("td");
    if (className) cell.className = className;
    cell.textContent = text == null ? "—" : String(text);
    row.appendChild(cell);
    return cell;
  }

  function buildRow(item) {
    const row = document.createElement("tr");
    if (item.selected) row.classList.add("is-selected");
    appendCell(row, "#" + item.rank);
    appendCell(row, item.territory);
    appendCell(row, item.maximumLabel, "report-value-cell");
    appendCell(row, item.maximumTime);
    appendCell(row, item.hoursLabel);
    const bandCell = document.createElement("td");
    const band = document.createElement("span");
    band.className = "report-band-chip";
    band.style.setProperty("--band-color", item.bandColor || "#7890a0");
    band.textContent = item.band;
    bandCell.appendChild(band);
    row.appendChild(bandCell);
    return row;
  }

  function bindReportTable(shell) {
    if (shell.dataset.reportTableBound === "true") return;
    shell.dataset.reportTableBound = "true";
    const source = shell.querySelector(".report-table-data");
    const body = shell.querySelector("tbody");
    if (!source || !body) return;

    let data;
    try {
      data = JSON.parse(source.textContent || "[]");
    } catch (error) {
      console.warn("Dados da tabela do relatório inválidos", error);
      return;
    }

    const search = shell.querySelector(".report-table-search");
    const pageSize = shell.querySelector(".report-page-size");
    const previous = shell.querySelector(".report-page-previous");
    const next = shell.querySelector(".report-page-next");
    const count = shell.querySelector(".report-table-count");
    const empty = shell.querySelector(".report-table-empty");
    const sortButtons = Array.from(shell.querySelectorAll(".report-sort-button"));
    let page = 0;
    let sortKey = "rank";
    let sortDirection = 1;

    function compare(left, right) {
      const a = left[sortKey];
      const b = right[sortKey];
      if (a == null && b == null) return 0;
      if (a == null) return 1;
      if (b == null) return -1;
      if (typeof a === "number" && typeof b === "number") return (a - b) * sortDirection;
      return String(a).localeCompare(String(b), undefined, {numeric: true, sensitivity: "base"}) * sortDirection;
    }

    function refresh() {
      const query = normalized(search ? search.value : "");
      const filtered = data
        .filter(item => !query || normalized(item.territory + " " + item.band).includes(query))
        .slice()
        .sort(compare);
      const size = Math.max(1, Number(pageSize ? pageSize.value : 10) || 10);
      const pages = Math.max(1, Math.ceil(filtered.length / size));
      page = Math.min(page, pages - 1);
      const start = page * size;
      const visible = filtered.slice(start, start + size);
      const fragment = document.createDocumentFragment();
      for (const item of visible) fragment.appendChild(buildRow(item));
      body.replaceChildren(fragment);

      if (empty) empty.hidden = filtered.length > 0;
      if (count) count.textContent = filtered.length ? (start + 1) + "–" + (start + visible.length) + " / " + filtered.length : "0 / 0";
      if (previous) previous.disabled = page <= 0;
      if (next) next.disabled = page >= pages - 1;
      for (const button of sortButtons) {
        const active = button.dataset.sortKey === sortKey;
        button.closest("th").setAttribute("aria-sort", active ? (sortDirection > 0 ? "ascending" : "descending") : "none");
        button.classList.toggle("is-active", active);
        const indicator = button.querySelector(".report-sort-indicator");
        if (indicator) indicator.textContent = active ? (sortDirection > 0 ? "↑" : "↓") : "";
      }
    }

    if (search) search.addEventListener("input", () => { page = 0; refresh(); });
    if (pageSize) pageSize.addEventListener("change", () => { page = 0; refresh(); });
    if (previous) previous.addEventListener("click", () => { page = Math.max(0, page - 1); refresh(); });
    if (next) next.addEventListener("click", () => { page += 1; refresh(); });
    for (const button of sortButtons) {
      button.addEventListener("click", () => {
        const key = button.dataset.sortKey;
        if (sortKey === key) sortDirection *= -1;
        else {
          sortKey = key;
          sortDirection = button.dataset.sortType === "number" && key !== "rank" ? -1 : 1;
        }
        page = 0;
        refresh();
      });
    }
    refresh();
  }

  function bindReport(report) {
    if (report.dataset.reportBound === "true") return;
    report.dataset.reportBound = "true";
    const tabs = Array.from(report.querySelectorAll(".report-scope-tab"));
    const panels = Array.from(report.querySelectorAll(".report-scope-panel"));
    for (const tab of tabs) {
      tab.addEventListener("click", () => {
        const target = tab.dataset.reportScopeTarget;
        for (const item of tabs) {
          const active = item === tab;
          item.classList.toggle("is-active", active);
          item.setAttribute("aria-selected", active ? "true" : "false");
        }
        for (const panel of panels) {
          const active = panel.dataset.reportScope === target;
          panel.classList.toggle("is-active", active);
          panel.hidden = !active;
        }
      });
    }
    for (const shell of report.querySelectorAll(".report-table-shell")) bindReportTable(shell);
  }

  function bindReports(root) {
    if (root.matches && root.matches(".territorial-report")) bindReport(root);
    if (root.querySelectorAll) {
      for (const report of root.querySelectorAll(".territorial-report")) bindReport(report);
    }
  }

  function initialize() {
    bindReports(document);
    if (!document.body || typeof MutationObserver === "undefined") return;
    new MutationObserver(mutations => {
      for (const mutation of mutations) {
        for (const node of mutation.addedNodes) {
          if (node.nodeType === 1) bindReports(node);
        }
      }
    }).observe(document.body, {childList: true, subtree: true});
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", initialize, {once: true});
  else initialize();
})();
