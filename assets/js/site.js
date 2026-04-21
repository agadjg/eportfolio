(function () {
  var MAX_CARD_CHARACTERS = 350;
  var yearNodes = document.querySelectorAll("[data-year]");
  var cardDescriptionNodes = document.querySelectorAll(".card.short p");
  var year = new Date().getFullYear();

  yearNodes.forEach(function (node) {
    node.textContent = String(year);
  });

  cardDescriptionNodes.forEach(function (node) {
    var fullText = node.textContent.replace(/\s+/g, " ").trim();

    if (fullText.length <= MAX_CARD_CHARACTERS) {
      node.textContent = fullText;
      return;
    }

    node.textContent = fullText.slice(0, MAX_CARD_CHARACTERS).trimEnd() + "...";
  });
  
  // Auto-apply variant classes to .md-btn elements based on their label text
  // This allows existing and future buttons to receive the correct colour
  // without editing HTML.
  var btnMap = {
    "key project": "btn-key-project",
    "reflection": "btn-reflection",
    "case study": "btn-case-study",
    "short analysis": "btn-short-analysis"
  };

  var mdBtns = document.querySelectorAll(".md-btn");
  mdBtns.forEach(function (btn) {
    try {
      // find the first span that is not the icon
      var labelSpan = btn.querySelector("span:not(.material-symbols-outlined)");
      if (!labelSpan) return;
      var label = labelSpan.textContent.replace(/\s+/g, " ").trim().toLowerCase();
      var cls = btnMap[label];
      if (cls && !btn.classList.contains(cls)) {
        btn.classList.add(cls);
      }
    } catch (e) {
      // fail silently — DOM may change or button contain different structure
      // but not critical for functionality
      console.error(e);
    }
  });
})();
