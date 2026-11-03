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

function Inlines(inl)
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
