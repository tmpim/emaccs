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
    local o = _G
    repeat
      o = o[s[1][1]]
      s = s[2]
    until not _pairS63(s)
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
  return f(table.unpack(args, 1, n))
end

do
  local counter = 0
  function _gensym()
    counter = counter + 1
    return symbol('#:' .. counter)
  end
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
