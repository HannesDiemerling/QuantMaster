-- Verhindert Zeilenumbrüche zwischen einer Inline-Formel und direkt angrenzenden
-- Klammern, Anführungszeichen oder Satzzeichen, z. B. „($d$)“ oder „$b$.“
local OPEN = { "(", "[", "„" }
local CLOSE = { ".", ",", ";", ":", ")", "]", "!", "?", "“", "%" }

local function split_tail(s)
  for _, o in ipairs(OPEN) do
    if #s > #o and s:sub(-#o) == o then return s:sub(1, -#o - 1), o end
    if s == o then return "", o end
  end
end

local function split_head(s)
  local head = ""
  local changed = true
  while changed do
    changed = false
    for _, c in ipairs(CLOSE) do
      if s:sub(1, #c) == c then head = head .. c; s = s:sub(#c + 1); changed = true end
    end
  end
  if head ~= "" then return head, s end
end

-- Kurzes Präfix mit Bindestrich, z. B. „p-Werte“, „t-Test“, „BFI-K“, „α-Fehler“, „UTF-8“
local function short_hyphen(s)
  local pre = s:match("^([^%s%-]+)%-%S")
  if pre ~= nil and utf8.len(pre) ~= nil and utf8.len(pre) <= 4 then return true end
  -- kurzes Mittelstück, z. B. „Einstichproben-t-Test“
  if s:match("%-[^%s%-][^%s%-]?%-%S") then return true end
  -- Prozent mit Bindestrich („95 %-KI“) oder kurzer Rest („Bootstrap-KI.“)
  if s:match("%%%-") then return true end
  local post = s:match("%-([^%-]+)$")
  if post == nil then return false end
  -- schließende Satzzeichen und Anführungszeichen am Ende abziehen (byteweise sicher)
  local core = post:gsub("“$", ""):gsub("”$", ""):gsub("[%.,;:!%?%)]+$", "")
  local n = utf8.len(core)
  return n ~= nil and n >= 1 and n <= 2
end

-- Zahlbereich mit Bis-Strich, z. B. „195–206“
local function num_range(s)
  return s:match("%w–%w") ~= nil
end

-- nur schließende Satzzeichen, z. B. „…“ oder „…“.“
local function closing_punct(s)
  local n = utf8.len(s)
  if n == nil or n > 3 or s:match("%w") then return false end
  local first = s:sub(1, 1)
  return first ~= "(" and first ~= "[" and s:sub(1, 3) ~= "„"
end

-- Kurzes „Wort“, das nicht allein am Zeilenanfang stehen soll: ein Buchstabe oder eine
-- Ziffer, auch griechisch (α, ω), ggf. mit schließendem Satzzeichen („2“.“)
local function tiny_word(s)
  local n = utf8.len(s)
  if n == nil or n > 3 then return false end
  local first = s:sub(1, 1)
  local greek = s:byte(1) == 0xCE or s:byte(1) == 0xCF
  if not (first:match("%w") or greek) then return false end
  local rest = greek and s:sub(3) or s:sub(2)
  return rest == "" or not rest:match("%w")
end

local RELATION = { ["="] = true, ["≈"] = true, ["<"] = true, [">"] = true, ["≤"] = true, ["≥"] = true, ["~"] = true, ["×"] = true }

local function nowrap(list)
  return pandoc.Span(list, pandoc.Attr("", { "nowrap" }))
end

function Inlines(inl)
  -- erst: *t*-Test (Hervorhebung direkt vor „-Wort“) und p-Werte zusammenhalten
  local tmp = pandoc.List()
  local j = 1
  while j <= #inl do
    local el, nx = inl[j], inl[j + 1]
    if (el.t == "Emph" or el.t == "Strong" or el.t == "Math") and nx and nx.t == "Str" and nx.text:match("^%-%a") then
      tmp:insert(nowrap({ el, nx })); j = j + 2
    elseif el.t == "Str" and (short_hyphen(el.text) or num_range(el.text)) then
      tmp:insert(nowrap({ el })); j = j + 1
    elseif el.t == "Code" and utf8.len(el.text) and utf8.len(el.text) <= 30 and el.text:match("[%s%-]") then
      -- kurzer Code mit Leerzeichen oder Bindestrich (Dateiname, Formel) bleibt in einer Zeile
      tmp:insert(nowrap({ el })); j = j + 1
    elseif el.t == "Space" and nx and nx.t == "Str" and tiny_word(nx.text) then
      -- einzelnes Zeichen (α, ω, n, „2“) nicht allein an den Zeilenanfang
      tmp:insert(pandoc.Str("\u{00A0}")); j = j + 1
    elseif el.t == "Space" and nx and nx.t == "Str" and closing_punct(nx.text) then
      tmp:insert(pandoc.Str("\u{00A0}")); j = j + 1
    elseif el.t == "Space" and nx and nx.t == "Str" and RELATION[nx.text] then
      -- vor „=“ usw. nicht umbrechen
      tmp:insert(pandoc.Str("\u{00A0}")); j = j + 1
    elseif el.t == "Space" and tmp[#tmp] and tmp[#tmp].t == "Str" and RELATION[tmp[#tmp].text] then
      -- nach „=“ usw. nicht umbrechen
      tmp:insert(pandoc.Str("\u{00A0}")); j = j + 1
    elseif el.t == "Space" and tmp[#tmp] and tmp[#tmp].t == "Str" and tmp[#tmp].text:match("^%(?%d+%.$") then
      -- Ordinalzahl bleibt beim folgenden Wort („2. Aufl.“, „3. Variable“)
      tmp:insert(pandoc.Str("\u{00A0}")); j = j + 1
    else
      tmp:insert(el); j = j + 1
    end
  end
  inl = tmp
  local out = pandoc.List()
  local i = 1
  while i <= #inl do
    local el = inl[i]
    if el.t == "Math" and el.mathtype == "InlineMath" then
      local group = pandoc.List({ el })
      local prev = out[#out]
      if prev and prev.t == "Str" then
        local rest, o = split_tail(prev.text)
        if o then
          if rest == "" then out:remove(#out) else prev.text = rest end
          group:insert(1, pandoc.Str(o))
        end
      end
      local nxt = inl[i + 1]
      if nxt and nxt.t == "Str" then
        local head, rest = split_head(nxt.text)
        if head then
          group:insert(pandoc.Str(head))
          if rest ~= "" then inl[i + 1] = pandoc.Str(rest) else i = i + 1 end
        end
      end
      if #group > 1 then
        out:insert(pandoc.Span(group, pandoc.Attr("", { "nowrap" })))
      else
        out:insert(el)
      end
    else
      out:insert(el)
    end
    i = i + 1
  end
  return out
end
