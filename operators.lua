function _S43(...)
  local t = table.pack(...)
  local r = 0
  for i = 1, t.n do
    r = r + t[i]
  end
  return r
end

function _S42(...)
  local t = table.pack(...)
  local r = 1
  for i = 1, t.n do
    r = r * t[i]
  end
  return r
end

function _S45(...)
  local t = table.pack(...)
  if t.n < 1 then
    error("not enough arguments for operator (-)")
  else
    local r = t[1]
    for i = 2, t.n do
      r = r - t[i]
    end
    return r
  end
end

function _S47(...)
  local t = table.pack(...)
  if t.n < 1 then
    error("not enough arguments for operator (/)")
  else
    local r = t[1]
    for i = 2, t.n do
      r = r / t[i]
    end
    return r
  end
end

function _callS47native(s, ...)
  if _symbolS63(s) then
    return _G[s[1]](...)
  else
    local o, path = _G, "_G"
    repeat
      o = o[s[1][1]]
      path = path .. "." .. s[1][1]
      s = s[2]
    until not _pairS63(s)
    if not o then
      error("No such procedure: " .. path)
    end
    return o(...)
  end
end

function _apply(f, t)
  assert(_pairS63(t) or t == scm_nil)
  local args, n, i = {}, 0, 1
  while _pairS63(t) do
    args[i] = t[1]
    i, n, t = i + 1, n + 1, t[2]
  end
  local ok, err = pcall(f, unpack(args, 1, n))
  if not ok then
    error(err, 2)
  else
    return err
  end
end

local gensym_counter = gensym_counter or 0
function _gensym()
  gensym_counter = gensym_counter + 1
  return symbol('#.' .. gensym_counter)
end

function _error(...)
  local t = table.pack(...)
  for i = 1, t.n do
    _write(t[i])
    io.write ' '
  end
  return error(list(...))
end

function _catch(thunk, handler)
  local ok, err = pcall(thunk)
  if not ok then
    return handler(err)
  else
    return err
  end
end

function _pairS63(p)
  return type(p) == 'table' and #p == 2
end

function _S62(...)
  local t = table.pack(...)
  if t.n < 2 then
    error("not enough arguments for operator (-)")
  else
    for i = 1, t.n do
      if t[i + 1] and t[i] <= t[i + 1] then
        return false
      end
    end
    return true
  end
end

function _S60(...)
  local t = table.pack(...)
  if t.n < 2 then
    error("not enough arguments for operator (-)")
  else
    for i = 1, t.n do
      if t[i + 1] and t[i] >= t[i + 1] then
        return false
      end
    end
    return true
  end
end

