-- fix-readme-links.lua
-- Rewrites relative .md links in {{< include >}}d README/GUIDE files to valid Quarto HTML URLs.
-- Covers: license badges, CHANGELOG, cross-module READMEs, guide sub-pages, language toggles.

-- ── guide page map (source .qmd basename → guide html) ──────────────────────
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
  ["sqlcute.qmd"]       = "sqlcute-guide.html",
  ["sqlcute-guide.qmd"] = "sqlcute-guide.html",
}

-- ── module directory → module overview page ──────────────────────────────────
local module_readme_map = {
  ["Application"]                    = "application.html",
  ["Hosting"]                        = "hosting.html",
  ["DependencyInjection"]            = "di.html",
  ["DependencyInjection.Abstractions"] = "di.html",
  ["Configuration"]                  = "configuration.html",
  ["Logging"]                        = "logging.html",
  ["NNLog"]                          = "nnlog.html",
  ["MediatR"]                        = "mediator.html",
  ["MediatR.Abstractions"]           = "mediator.html",
  ["MiniSpec"]                       = "minispec.html",
  ["SQLCute.Abstractions"]           = "sqlcute.html",
  ["SQLCute.Compilers"]              = "sqlcute.html",
  ["SQLCute"]                        = "sqlcute.html",
}

-- ── SQLCute topic filename (with or without .es) → anchor ───────────────────
local sqlcute_anchors = {
  ["select"]      = "#select--from",
  ["where"]       = "#where",
  ["joins"]       = "#joins",
  ["groupby"]     = "#group-by--aggregates",
  ["setops"]      = "#set-operations",
  ["dml"]         = "#dml",
  ["subqueries"]  = "#subqueries",
  ["string-ops"]  = "#string-operations",
  ["date-ops"]    = "#date-operations",
  ["dialects"]    = "#dialect-reference",
}

local function sqlcute_anchor(filename)
  -- strip path, strip fragment (#...), strip .md or .es.md
  local base = filename:match("([^/\\]+)$") or filename
  base = base:gsub("#.*$", "")
  base = base:gsub("%.es%.md$", ""):gsub("%.md$", "")
  return sqlcute_anchors[base]
end

local function is_sqlcute_topic(filename)
  return sqlcute_anchor(filename) ~= nil
end

-- ── helper: current source file basename ────────────────────────────────────
local function src_basename()
  local src = (PANDOC_STATE.input_files or {})[1] or ""
  return src:match("([^/\\]+)$") or ""
end

-- ── main Link filter ─────────────────────────────────────────────────────────
function Link(el)
  local t = el.target

  -- 1. LICENSE badge links → GitHub
  if t:match("legal/LICENSE") then
    el.target = "https://github.com/Delphi-DAF/dafce/blob/main/legal/LICENSE.md"
    return el
  end

  -- 2. CHANGELOG → GitHub
  if t:match("CHANGELOG%.md$") then
    el.target = "https://github.com/Delphi-DAF/dafce/blob/main/CHANGELOG.md"
    return el
  end

  -- 3. GUIDE*.md → <module>-guide.html
  if t:match("GUIDE[^/]*%.md") then
    local guide = guide_map[src_basename()]
    el.target = guide or "#guide"
    return el
  end

  -- 4. Cross-module README links (e.g. ../MediatR.Abstractions/README.md)
  for mod, page in pairs(module_readme_map) do
    if t:match(mod .. "[/\\]README") then
      el.target = page
      return el
    end
  end

  -- 5. Language toggle in README files (README.es.md ↔ README.md)
  --    Rewrite to the corresponding module page in the other language
  if t:match("^README%.es%.md$") then
    local base = src_basename():gsub("%.qmd$", "")
    el.target = "../../es/modules/" .. base .. ".html"
    return el
  end
  if t:match("^README%.md$") then
    local base = src_basename():gsub("%.qmd$", "")
    el.target = "../../en/modules/" .. base .. ".html"
    return el
  end

  -- 6. SQLCute topic files: guide/select.md, guide/select.es.md,
  --    or bare select.es.md (language toggle), or SQLCute.Abstractions/docs/guide/…
  if t:match("[/\\]guide[/\\]") or t:match("^guide[/\\]") or
     t:match("SQLCute%.Abstractions[/\\]docs") then
    local anchor = sqlcute_anchor(t)
    if anchor then
      el.target = "sqlcute-guide.html" .. anchor
      return el
    end
  end
  -- bare topic filename (language toggle in individual topic files)
  if is_sqlcute_topic(t) then
    local anchor = sqlcute_anchor(t)
    el.target = "sqlcute-guide.html" .. anchor
    return el
  end

  -- 7. MiniSpec guide sub-pages (guide/assertions.md, guide/world.md, etc.)
  --    and TESTING-PATTERNS.md
  if t:match("TESTING%-PATTERNS") then
    el.target = "minispec-guide.html"
    return el
  end
  if t:match("[/\\]guide[/\\][^/\\]+%.md$") or t:match("^guide[/\\][^/\\]+%.md$") then
    el.target = "minispec-guide.html"
    return el
  end

  -- 8. Sample README links → #
  if t:match("samples?[/\\].*README") or t:match("Sample.*README") then
    el.target = "#"
    return el
  end

  -- 9. modules/README.md or README.es.md cross-language ref
  if t:match("modules?[/\\]README") then
    el.target = "index.html"
    return el
  end
end

-- Handle HTML <a href="..."> tags in raw inline/block HTML
-- (e.g. back-links in included .md files: <a href="../README.md">)
function RawInline(el)
  if el.format == "html" and el.text:match("<a ") then
    local text = el.text:gsub('href="([^"]*)"', function(href)
      -- Back-links to README → anchor (no target page in Quarto context)
      if href:match("README[^\"]*%.md") then
        return 'href="#"'
      end
      return 'href="' .. href .. '"'
    end)
    if text ~= el.text then
      return pandoc.RawInline("html", text)
    end
  end
end
