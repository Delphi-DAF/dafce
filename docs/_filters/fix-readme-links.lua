-- fix-readme-links.lua
-- Cuando los README se incluyen via {{< include >}} en Quarto, los links
-- relativos como "docs/GUIDE.md" se resuelven mal (el path es relativo al
-- README original, no al HTML de salida).
-- La guía ya está incluida inline en la misma página, así que convertimos
-- esos links en anchors (#guide) en lugar de dejarlos rotos.

function Link(el)
  local t = el.target
  -- Cubre: docs/GUIDE.md, docs/GUIDE.es.md, ../Foo/docs/GUIDE.md, etc.
  if t:match("GUIDE[^/]*%.md$") then
    el.target = "#guide"
    return el
  end
end
