local symbol = mksymbol
_G.add_rat = add_rat
_G.over_rat = over_rat
_G.times_rat = times_rat
_G.minus_rat = minus_rat
_G._read      = read_sexpr
_G.scm_nil    = scm_nil
_G.scm_eof    = scm_eof
_G.symbol     = mksymbol
function _G.keyword(s)
  return {[0]=symbol,kw=true,s}
end
_G._symbolS63 = symbolp
_G._eqS63     = scm_eq
function _G._write(...)
  local t = table.pack(...)
  for i = 1, t.n do
    scm_print(t[i])
  end
  return true
end
function _G._display(...)
  local t = table.pack(...)
  for i = 1, t.n do
    scm_print(t[i], true)
  end
  return true
end

function _S43(...)
  local t = table.pack(...)
  local r = 0
  for i = 1, t.n do
    r = add_rat(r, t[i])
  end
  return r
end

function _S42(...)
  local t = table.pack(...)
  local r = 1
  for i = 1, t.n do
    r = times_rat(r, t[i])
  end
  return r
end

function _S45(...)
  local t = table.pack(...)
  if t.n == 0 then
    error("not enough arguments for operator (-)")
  elseif t.n == 1 then
    return minus_rat(0, t[1])
  else
    local r = t[1]
    for i = 2, t.n do
      r = minus_rat(r, t[i])
    end
    return r
  end
end

function _S47(...)
  local t = table.pack(...)
  if t.n < 1 then
    error("not enough arguments for operator (/)")
  elseif t.n == 1 then
    return over_rat(rational(1, 1), t[1])
  else
    local r = t[1]
    for i = 2, t.n do
      r = over_rat(r, t[i])
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
  assert(_pairS63(t) or t == scm_nil, "values to be applied must be a list")
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
  return type(p) == 'table' and #p == 2 and p[0] ~= rational
end

local function rat_lte(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x <= y
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return x[1]*y[2] <= y[1]*x[2]
end

function _S62(...)
  local t = table.pack(...)
  if t.n < 2 then
    error("not enough arguments for operator (>)")
  else
    for i = 1, t.n do
      if t[i + 1] and rat_lte(t[i], t[i + 1]) then
        return false
      end
    end
    return true
  end
end

local function rat_gte(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x >= y
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return x[1]*y[2] >= y[1]*x[2]
end

function _S60(...)
  local t = table.pack(...)
  if t.n < 2 then
    error("not enough arguments for operator (<)")
  else
    for i = 1, t.n do
      if t[i + 1] and rat_gte(t[i], t[i + 1]) then
        return false
      end
    end
    return true
  end
end

local function rat_lt(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x < y
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return x[1]*y[2] < y[1]*x[2]
end

function _S62S61(...)
  local t = table.pack(...)
  if t.n < 2 then
    error("not enough arguments for operator (>=)")
  else
    for i = 1, t.n do
      if t[i + 1] and rat_lt(t[i], t[i + 1]) then
        return false
      end
    end
    return true
  end
end

local function rat_gt(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x > y
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return x[1]*y[2] > y[1]*x[2]
end


function _S60S61(...)
  local t = table.pack(...)
  if t.n < 2 then
    error("not enough arguments for operator (<=)")
  else
    for i = 1, t.n do
      if t[i + 1] and rat_gt(t[i], t[i + 1]) then
        return false
      end
    end
    return true
  end
end

