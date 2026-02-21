-- fix-readme-links.lua
-- Los README incluidos via {{< include >}} tienen links como "docs/GUIDE.md"
-- que son relativos a la fuente del README, no al HTML de salida.
-- Este filtro los reescribe a la página de guía correspondiente.

local guide_map = {
  ["application.qmd"]   = "application-guide.html",
  ["commons.qmd"]       = "commons-guide.html",
  ["configuration.qmd"] = "configuration-guide.html",
  ["di.qmd"]            = "di-guide.html",
  ["hosting.qmd"]       = "hosting-guide.html",
  ["logging.qmd"]       = "logging-guide.html",
  ["mediator.qmd"]      = "mediator-guide.html",
  ["minispec.qmd"]      = "minispec-guide.html",
  ["nnlog.qmd"]         = "nnlog-guide.html",
  ["web.qmd"]           = "web-guide.html",
}

function Link(el)
  if el.target:match("GUIDE[^/]*%.md") then
    local src = (PANDOC_STATE.input_files or {})[1] or ""
    local basename = src:match("([^/\\]+)$") or ""
    local guide = guide_map[basename]
    el.target = guide or "#guide"
    return el
  end
end
