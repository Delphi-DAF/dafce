-- Reescribe los enlaces relativos de los documentos que el sitio publica por include.
--
-- El porque, el reparto de destinos y la medida que lo motivo estan en la cabecera de
-- docs/_gen/link-map.py, que genera link-map.lua. Aqui solo se aplica el mapa.
--
-- Se clava en quarto.doc.input_file porque es lo unico que sigue sabiendo cual es la entrada de
-- verdad: PANDOC_STATE.input_files da el temporal que Quarto escribe al resolver el include, y de
-- ahi salen los href a quarto-input<hash>.md.html.

local script = PANDOC_SCRIPT_FILE or ""
local dir = script:match("^(.*)[/\\][^/\\]*$") or "."
local MAP = dofile(dir .. "/link-map.lua")

local page, resolved = nil, false

local function current_page()
  local input = quarto and quarto.doc and quarto.doc.input_file
  if type(input) ~= "string" then return nil end
  -- La raiz del proyecto la da Quarto; el respaldo es la PRIMERA aparicion de /docs/, porque
  -- las paginas de docs/<lang>/docs/ traen dos y quedarse con la ultima da una clave que no
  -- existe en el mapa -- y entonces el filtro no reescribe nada sin decirlo.
  local proj = quarto and quarto.project and quarto.project.directory
  if type(proj) == "string" and input:sub(1, #proj) == proj then
    return (input:sub(#proj + 2))
  end
  local first = input:find("/docs/", 1, true)
  if not first then return nil end
  return input:sub(first + 6)
end

function Link(el)
  if not resolved then
    page = current_page()
    resolved = true
  end
  local path, frag = el.target:match("^([^#]*)(.*)$")
  local tbl = page and MAP.links[page]
  -- Primero el destino entero: asi entran las anclas ('#dafrtti') y los enlaces que nombran un
  -- ancla del documento de destino, cuyo slug tambien cambia al publicarse. Si no, solo la ruta.
  local repl = tbl and (tbl[el.target]
                        or (path ~= "" and tbl[path] and tbl[path] .. (frag or "")))
  if repl then
    el.target = repl
    return el
  end
  -- Un .md relativo que el mapa no conoce es el modo de fallo silencioso de este filtro: sin
  -- este aviso, dejar de reescribir y no tener nada que reescribir se leen igual. Las paginas
  -- sin include no llegan aqui, porque no tienen enlaces .md relativos.
  if path and path:match("%.md$") and not path:match("^%a[%w+.-]*:") then
    io.stderr:write("include-links.lua: " .. tostring(page) .. " -> '" .. el.target ..
                    "' no esta en el mapa; ejecuta docs/_gen/link-map.py\n")
  end
  return nil
end

-- Dos documentos de MiniSpec llevan el enlace de vuelta como <a href> crudo, que no es un Link
-- para pandoc sino HTML sin tocar. El mapa es el mismo.
local function rewrite_raw(el)
  if el.format ~= "html" or not el.text:find("<a", 1, true) then return nil end
  if not resolved then
    page = current_page()
    resolved = true
  end
  local tbl = page and MAP.links[page]
  if not tbl then return nil end
  local text = el.text:gsub('href="([^"]*)"', function(href)
    local path, frag = href:match("^([^#]*)(.*)$")
    local repl = tbl[href] or (path ~= "" and tbl[path] and tbl[path] .. (frag or ""))
    return 'href="' .. (repl or href) .. '"'
  end)
  if text == el.text then return nil end
  return el.format == "html" and (el.t == "RawBlock" and pandoc.RawBlock("html", text)
                                  or pandoc.RawInline("html", text)) or nil
end

function RawInline(el) return rewrite_raw(el) end
function RawBlock(el) return rewrite_raw(el) end
