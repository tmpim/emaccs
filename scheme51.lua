local input, input_file = {}

local write = write

if not table.pack then
  table.pack = function(...)
    return { n = select('#', ...), ... }
  end
end

if not table.unpack then
  table.unpack = unpack
end

if not math.type then
  function math.type(x)
    if type(x) == 'number' then
      return select(2, math.modf(x)) ~= 0 and 'float' or 'integer'
    end
    return nil
  end
end

if not load then
  function _G.load(string, source, mode, env)
    return setfenv(loadstring(string), env)
  end
end

if not write then
  function write(x)
    return io.write(tostring(x))
  end
end

function getchar()
  if #input >= 1 then
    local c = table.remove(input, 1)
    return c
  elseif input_file then
    return input_file:read(1)
  elseif not _CC_DEFAULT_SETTINGS then
    return io.read(1)
  else
    while true do
      local ev, c = coroutine.yield()
      if ev == 'key' and c == keys.enter then
        write('\n')
        return '\n'
      elseif ev == 'char' then
        write(c)
        return c
      end
    end
  end
end

local function ungetchar(ch)
  table.insert(input, 1, ch)
end

local function peek()
  local ch = getchar()
  ungetchar(ch)
  return ch
end

local function peekp(c)
  return peek() == c
end

local function eat(chs, i)
  local i = i or 1
  if i > #chs then
    return
  elseif peekp(chs:sub(i, i)) and i <= #chs then
    getchar()
    return eat(chs, i + 1)
  else
    return error("unexpected character " .. getchar())
  end
end

local function skip_spaces()
  local ch = getchar()
  if ch == ' ' or ch == '\t' or ch == '\n' then
    return skip_spaces()
  elseif ch == ';' then
    while ch ~= '\n' do
      ch = getchar()
    end
    return skip_spaces()
  end
  return ch
end

local function gcd(a, b)
  if b == 0 then
    return math.abs(a)
  else
    return gcd(b, a % b)
  end
end

function rational(a, b)
  assert(type(a) == 'number' and type(b) == 'number')
  local c = gcd(a, b)
  local d = math.floor(b/c)
  if d == 0 then
    return error("division by zero")
  else
    return {[0]=rational, math.floor(a/c), math.floor(b/c)}
  end
end

local function insist_delim(x)
  local ch = peek()
  if ch == ' ' or ch == '\t' or ch == '\n' or ch == ')' or ch == ']' or ch == '}' then
    return x
  else
    return error("expected delimiter, got " .. getchar())
  end
end

local function is_char_base(radix)
  if radix <= 10 then
    local upper = string.char(string.byte('9') - (10 - radix))
    return function(ch)
      return '0' <= ch and ch <= upper
    end
  elseif radix <= 26 then
    local upper = string.char(string.byte('z') - (26 - radix))
    return function(ch)
      return ('0' <= ch and ch <= '9') or ('a' <= ch and ch <= upper)
    end
  elseif radix <= 52 then
    local upper = string.char(string.byte('Z') - (52 - radix))
    return function(ch)
      return ('0' <= ch and ch <= '9')
          or ('a' <= ch and ch <= 'z')
          or ('A' <= ch and ch <= upper)
    end
  end
end

local function read_number(acc, radix, allow_dot)
  local allow_dot = allow_dot == nil and true or false
  local p = is_char_base(radix or 10)
  local ch = getchar()
  if p(ch) then
    return insist_delim(read_number(acc .. ch, radix))
  elseif ch == '.' and allow_dot then
    local dec = read_number('', radix, false)
    return insist_delim(read_number(acc .. ch .. tostring(dec)))
  elseif ch == '/' and allow_dot then
    local denom = read_number('', radix, false)
    return insist_delim(rational(tonumber(acc), tonumber(denom)))
  else
    ungetchar(ch)
    return insist_delim(tonumber(acc or '0', radix))
  end
end

local symbol_chars = {
  ['+'] = true, ['-'] = true, ['?'] = true, ['!'] = true, ['='] = true,
  ['>'] = true, ['<'] = true, ['*'] = true, ['/'] = true
}

local function symbol_carp(ch)
  return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or symbol_chars[ch]
end

local symbol_table, symbol = {}, {}

local function mksymbol(x, iskeyword)
  if symbol_table[x] then
    return symbol_table[x]
  else
    symbol_table[x] = {kw=iskeyword,[0]=symbol,x}
    return symbol_table[x]
  end
end

_G.symbol_table = symbol_table

local _lambda = mksymbol('lambda')
local _if = mksymbol('if')
local _quote = mksymbol('quote')
local _quasiquote = mksymbol('quasiquote')
local _unquote = mksymbol('unquote')
local _unquotes = mksymbol('unquote-splicing')
local _define = mksymbol('define')
local _set = mksymbol('set!')

local function read_symbol(acc, k)
  local ch = getchar()
  if symbol_carp(ch) or (ch <= '9' and ch >= '0') or ch == '/' then
    return read_symbol(acc .. ch, k)
  else
    ungetchar(ch)
    return (k or mksymbol)(acc)
  end
end

local read_sexpr

local function read_character()
  local ch = getchar()
  if ch == 's' then
    if peekp('p') then
      eat('pace')
      return ' '
    end
    return insist_delim(s)
  elseif ch == 'n' then
    if peekp('e') then
      eat('ewline')
      return '\n'
    elseif peekp('u') then
      eat('ull')
      return '\0'
    end
    return insist_delim('n')
  elseif ch == 't' then
    if peekp('a') then
      eat('ab')
      return '\t'
    end
    return insist_delim('t')
  elseif ch == 'b' then
    if peekp('a') then
      eat('ackspace')
      return '\08'
    end
    return insist_delim('b')
  elseif ch == 'e' then
    if peekp('s') then
      eat('scape')
      return '\x1b'
    end
    return insist_delim('e')
  elseif ch == 'd' then
    if peekp('e') then
      eat('elete')
      return '\x7f'
    end
    return insist_delim('d')
  elseif ch == 'x' then
    local acc = '0x'
    while tonumber(peek(), 16) do
      acc = acc .. getchar()
    end
    return insist_delim(utf8.char(tonumber(acc)))
  else
    return ch
  end
end

local scm_nil, scm_eof, eval, callproc = {}, {}, {}, {}

local function read_special_atom()
  local ch = getchar()
  if ch == 't' then
    return insist_delim(true)
  elseif ch == 'f' then
    return insist_delim(false)
  elseif ch == '\\' then
    return read_character()
  elseif ch == ';' then
    read_sexpr()
    return true
  elseif ch == ':' then
    return insist_delim(read_symbol('', function(s)
      return {[0] = symbol, kw = true, s}
    end))
  elseif ch == 'e' then
    eat('of')
    return insist_delim(scm_eof)
  elseif ch == 'o' then
    return read_number('', 8)
  elseif ch == 'x' then
    return read_number('', 16)
  elseif ch == 'b' then
    return read_number('', 2)
  else
    return error('unexpected special atom ' .. ch)
  end
end

local function cons(a,b)
  return {a,b}
end

local delims = { [']'] = true, ['}'] = true, [')'] = true, ['['] = ']', ['('] = ')', ['{'] = '}' }

local function read_sexpr_list(delim, can_dot)
  local ch = skip_spaces()
  if ch == delim then
    return scm_nil
  elseif type(delims[ch]) == 'boolean' then
    return error("delimiter mismatch: expected " .. delim .. " but got " .. ch)
  elseif ch == '.' and can_dot then -- improper pair
    local e = read_sexpr()
    if not peekp(delim) then
      return error("expected delimiter after cdr of improper list")
    end
    getchar()
    return e
  elseif not ch then
    return error("expected expression in list, but got #eof")
  else
    ungetchar(ch)
    return {read_sexpr(), read_sexpr_list(delim, true)}
  end
end

local function read_string(acc)
  local ch = getchar()
  if ch == '\\' then
    local ch = getchar()
    if ch == 'n' then
      return read_string(acc .. '\n')
    elseif ch == 't' then
      return read_string(acc .. '\t')
    elseif ch == '"' then
      return read_string(acc .. '"')
    elseif ch == '\\' then
      return read_string(acc .. '\\')
    else
      error("unknown escape sequence " .. ch)
    end
  elseif ch == '"' then
    return acc
  elseif not ch then
    error(("unterminated string literal: %q"):format(acc))
  else
    return read_string(acc .. ch)
  end
end

function read_sexpr()
  local ch = skip_spaces()
  if not ch then
    return scm_eof
  elseif ch == '(' or ch == '[' or ch == '{' then
    return read_sexpr_list(delims[ch], false)
  elseif ch == '\'' then
    return {_quote, {read_sexpr(), scm_nil}}
  elseif ch == '`' then
    return {_quasiquote, {read_sexpr(), scm_nil}}
  elseif ch == ',' then
    if peekp('@') then
      getchar()
      return {_unquotes, {read_sexpr(), scm_nil}}
    end
    return {_unquote, {read_sexpr(), scm_nil}}
  elseif ch <= '9' and ch >= '0' then
    return read_number(ch)
  elseif ch == '-' then
    local ch2 = peek()
    if ch2 <= '9' and ch2 >= '0' then
      getchar()
      local num = read_number(ch2)
      if type(num) == 'table' then
        return rational(-num[1], num[2])
      else
        return -num
      end
    else
      return read_symbol(ch)
    end
  elseif ch == '#' then
    return read_special_atom()
  elseif symbol_carp(ch) then
    return read_symbol(ch)
  elseif ch == '"' then
    return read_string("")
  else
    return error("lexical error at character: " .. ch)
  end
end

local function symbolp(e)
  return type(e) == 'table' and e[0] == symbol
end

local function consp(e)
  return type(e) == 'table' and #e == 2 and e[0] ~= rational
end

local function scm_print(e, display, seen)
  seen = seen or {}
  if type(e) == 'table' then
    if seen[e] then
      write("#<cycle>")
      return
    end
    seen[e] = true
    if symbolp(e) then
      write(e[1])
    elseif e[0] == eval then
      write '(lambda '
      scm_print(e[1], display, seen)
      write ' '
      scm_print(e[3], display, seen)
      write ')'
    elseif consp(e) then
      write '('
      repeat
        scm_print(e[1], display, seen)
        if consp(e[2]) then
          write ' '
        end
        e = e[2]
      until not consp(e)
      if e ~= scm_nil then
        write ' . '
        scm_print(e, display, seen)
      end
      write ')'
    elseif e == scm_nil then
      write ('\'()')
    elseif e[0] == callproc then
      write '<procedure>'
    elseif e[0] == rational then
      write (tostring(e[1]))
      write '/'
      write (tostring(e[2]))
    elseif e == scm_eof then
      write '#eof'
    elseif e.kw then
      write '#:'
      write(e[1])
    elseif getmetatable(e) and getmetatable(e).__call then
      write(tostring(getmetatable(e).__call))
    else
      write '(make-hash-table '
      for k, v in pairs(e) do
        write '('
        scm_print(k, display, seen)
        write ' . '
        scm_print(v, display, seen)
        write ')'
        if next(e, k) ~= nil then
          write ' '
        end
      end
      write ')'
    end
  elseif type(e) == 'string' then
    if display then
      write(e)
    else
      write(('%q'):format(e))
    end
  elseif e == true then
    write('#t')
  elseif e == false then
    write('#f')
  else
    write(tostring(e))
  end
end

local function is_self_eval(e)
  return type(e) == 'number'
      or type(e) == 'string'
      or type(e) == 'function'
      or type(e) == 'boolean'
      or (type(e) == 'table' and (e[0] == eval or e[0] == rational))
      or e == scm_nil
      or e == scm_eof
      or e == nil
end

local function num2rat(x)
  if type(x) == 'table' then
    return x
  else
    return rational(x, 1)
  end
end

local function bothnums(a, b)
  return (type(a) == 'number' or (type(a) == 'table' and a[0] == rational))
     and (type(b) == 'number' or (type(b) == 'table' and b[0] == rational))
end

local function scm_eq(a, b)
  if a == b then
    return true
  elseif consp(a) and consp(b) then
    return scm_eq(a[1]) and scm_eq(a[2])
  elseif type(a) == 'table' and (a[0] == eval or a[0] == callproc) then
    return false
  elseif type(a) == 'table' and a.kw and type(b) == 'table' and b.kw then
    return a[1] == b[1]
  elseif bothnums(a, b) then
    local a = num2rat(a)
    local b = num2rat(b)
    return a[1] * b[2] == b[1] * a[2]
  end
  return false
end

local function add_rat(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x + y
  elseif type(x) == 'table' and math.type(y) == 'float' then
    return (y * x[2] + x[1]) / x[2]
  elseif type(y) == 'table' and math.type(x) == 'float' then
    return (x * y[2] + y[1]) / y[2]
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return rational(x[1] * y[2] + y[1] * x[2], x[2] * y[2])
end

local function minus_rat(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x - y
  elseif type(x) == 'table' and math.type(y) == 'float' then
    return (y * x[2] - x[1]) / x[2]
  elseif type(y) == 'table' and math.type(x) == 'float' then
    return (x * y[2] - y[1]) / y[2]
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return rational(x[1] * y[2] - y[1] * x[2], x[2] * y[2])
end

local function times_rat(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    return x * y
  elseif type(x) == 'table' and math.type(y) == 'float' then
    return (y * x[1]) / x[2]
  elseif type(y) == 'table' and math.type(x) == 'float' then
    return (x * y[1]) / y[2]
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return rational(x[1] * y[1], x[2] * y[2])
end

local function over_rat(x, y)
  if type(x) == 'number' and type(y) == 'number' then
    if math.type(x) == 'integer' and math.type(x) == 'integer' then
      return rational(x, y)
    end
    return x / y
  elseif type(x) == 'table' and math.type(y) == 'float' then
    return x[1] / (x[2] * y)
  elseif type(y) == 'table' and math.type(x) == 'float' then
    return x / (y[1] * y[2])
  end
  local x = num2rat(x)
  local y = num2rat(y)
  return rational(x[1] * y[2], x[2] * y[1])
end


local gensym_counter = 0

local scm_env = {
}

scm_env.read  = read_sexpr

local handles = {}

function _G.input_from_file(p, t)
  if not p then
    error('not p', 2)
  end
  if type(p) == 'string' then
    local h = io.open(p, 'r')
    if not h then
      return error('failed to open ' .. p .. ' for reading')
    end
    p = h
  end
  local i = input_file
  input_file = p
  local r = t()
  input_file = i
  return r
end

function _G.output_to_file(p, t)
  if not p then
    error('not p', 2)
  end
  if type(p) == 'string' then
    local h = io.open(p, 'w')
    if not h then
      return error('failed to open ' .. p .. ' for write')
    end
    local old_write = write
    function write(x)
      h:write(x)
    end
    local r = t()
    write = old_write
    h:close()
    return r
  else
    error("argument must be a path", 2)
  end
end
-- return repl()

if select(1, ...) == 'boot' then
  scm_env.booting = true
else
  scm_env.booting = false
end

function scm_env.environment()
  return _ENV
end

function _G.scm_set_helper(x, v, n)
  if not x then
    error("can't set! " .. n)
  end
  return v
end

function _G.var(x, n)
  if x ~= nil then
    return x
  end
  return error("no binding for symbol " .. n, 2)
end

function ignore(x)
  return true
end

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
  local r = table.pack(pcall(f, table.unpack(args, 1, n)))
  if not r[1] then
    error(r[2], 2)
  else
    return table.unpack(r, 2, r.n)
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

function var(x, n)
      if x ~= nil then return x end
      return error('no binding for symbol ' .. n, 2)
    end

function list(car, ...)
     if car ~= nil then
       return { car, list(...) }
     else
       return scm_nil
     end
   end

function scm_set_helper(var, val, name)
     assert(var ~= nil, 'no previous binding for symbol ' .. name);
     return val
   end

if not _ENV then _ENV = _G end

function _environment() return _ENV end

function _setS45carS33(_cell ,  _val  ) _cell[1] = _val; return true end

function _car(_cell  ) return _cell[1] end

function _setS45cdrS33(_cell ,  _val  ) _cell[2] = _val; return true end

function _cdr(_cell  ) return _cell[2] end

function _nullS63(_p  ) return _p == scm_nil or _p == nil end

function _numberS63(_p  ) return type(_p) == 'number' or (type(_p) == 'table' and _p[0] == rational) end

function _stringS63(_p  ) return type(_p) == 'string' end

function _keywordS63(_p  ) return _symbolS63(_p) and _p.kw ~= nil end

function _procedureS63(_p  ) return type(_p) == 'function' or (type(_p) == 'table' and getmetatable(_p) and type(getmetatable(_p).__call) == 'function') end

function _charS63(_p  ) return type(_p) == 'string' and #_p == 1 end

function _cons(_a ,  _b  ) return {_a,_b} end

function _hashS45ref(_t ,  _k ,  _def  ) if _t[_k] ~= nil then return _t[_k] else return _def or false end end

function _callS45withS45values(_pro ,  _con  ) return _con(_pro()) end

function _values(...)
     return ...
   end

function _hashS45forS45each(_hash ,  _func  ) local n = 0
   for k, v in pairs(_hash) do
     _func(k, v)
     n = n + 1
   end
   return n end

function _rationalS63(_p  ) return type(_p) == 'table' and _p[0] == rational end

_platform = 'Scheme 51'

function _withS45inputS45fromS45file(_path ,  _thunk  ) return input_from_file(_path, _thunk) end

function _withS45outputS45toS45file(_path ,  _thunk  ) return output_to_file(_path, _thunk) end

_booting = false
local function ignore(x) end
ignore((function(_S35S46892)
 if _S35S46892 then
 return (function(_S35S46893)
 if _S35S46893 then
 return (function()
 var(_display, "display")("loading pre-built Scheme...",  "\
" );
 var(_callS47native, "call/native")(symbol('dofile'),  "scheme51.lua" );
 _display("loading startup file...",  "\
" );
 return _callS47native(symbol('load'),  "return _load('emaccs/startup.ss')" )()
 end)() else
 return false
 end
 end)(var(_callS47native, "call/native")({symbol('fs'),{symbol('exists'),scm_nil}},  "scheme51.lua" )) else
 return false
 end
 end)(var(_eqS63, "eq?")(var(_platform, "platform"),  symbol('computercraft') )))_not = setmetatable({args={symbol('b'),scm_nil},doc="Negate the boolean ,b."}, { __call = function(_S35S46894 ,  _b  )
 if var(_b, "b") then return false else return true end
 end });
 ignore(_not)_S61 = _eqS63;
 ignore(_S61)_charS61S63 = _S61;
 ignore(_charS61S63)_caar = (function(_x  )
 return ((var(_x, "x"))[1])[1]
 end);
 ignore(_caar)_cadr = (function(_x  )
 return ((var(_x, "x"))[2])[1]
 end);
 ignore(_cadr)_cdar = (function(_x  )
 return ((var(_x, "x"))[1])[2]
 end);
 ignore(_cdar)_cddr = (function(_x  )
 return ((var(_x, "x"))[2])[2]
 end);
 ignore(_cddr)_list = (function(...)
 local _x = list(...);
 return var(_x, "x")
 end);
 ignore(_list)_makeS45hashS45table = setmetatable({args=scm_nil,doc="Create a new hash table. This procedure is internal: Shouldn't this\
  have arguments?"}, { __call = function(_S35S46896  )
 return _callS47native(symbol('load'),  "return {}" )()
 end });
 ignore(_makeS45hashS45table)_hashS45setS33 = setmetatable({args={symbol('table'),{symbol('key'),{symbol('value'),scm_nil}}},doc="Associate the ,key in ,table with the given ,value."}, { __call = function(_S35S46897 ,  _table ,  _key ,  _value  )
 return _callS47native(symbol('rawset'),  var(_table, "table"),  var(_key, "key"),  var(_value, "value") )
 end });
 ignore(_hashS45setS33)_copyS45hashS45table = setmetatable({args={symbol('table'),scm_nil},doc="Copy the hash table ,table. Since hash tables are mutable, this is\
  important if you want to maintain persistence."}, { __call = function(_S35S46898 ,  _table  )
 local _x;
 _x = _makeS45hashS45table();
 var(_hashS45forS45each, "hash-for-each")(var(_table, "table"),  (function(_k ,  _v  )
 return _hashS45setS33(var(_x, "x"),  var(_k, "k"),  var(_v, "v") )
 end) );
 return var(_x, "x")
 end });
 ignore(_copyS45hashS45table)_macros = _makeS45hashS45table();
 ignore(_macros)_pushS45macroS33 = setmetatable({args={symbol('name'),{symbol('expander'),scm_nil}},doc="Associate the macro ,name with the expansion function ,expander.\
   Prefer 'define-syntax instead."}, { __call = function(_S35S46899 ,  _name ,  _expander  )
 _hashS45setS33(_macros,  (var(_name, "name"))[1],  var(_expander, "expander") );
 return true
 end });
 ignore(_pushS45macroS33)_lookupS45macroS45in = setmetatable({args={symbol('symbol'),{symbol('table'),scm_nil}},doc="Look up the macro called ,symbol in the table ,table."}, { __call = function(_S35S46900 ,  _symbol ,  _table  )
 return var(_hashS45ref, "hash-ref")(var(_table, "table"),  (var(_symbol, "symbol"))[1] )
 end });
 ignore(_lookupS45macroS45in)_makeS45lambda = setmetatable({args={symbol('args'),{symbol('body'),scm_nil}},doc="Make a 'lambda expression with the given ,args and ,body."}, { __call = function(_S35S46901 ,  _args ,  _body  )
 return {symbol('lambda'), {var(_args, "args"), var(_body, "body")}}
 end });
 ignore(_makeS45lambda)_map = setmetatable({args={symbol('f'),{symbol('x'),scm_nil}},doc="Map the procedure ,f over the list ,x, collecting the results in a\
   new list that doesn't share any structure with ,x."}, { __call = function(_S35S46902 ,  _f ,  _x  )
 if var(_nullS63, "null?")(var(_x, "x") ) then return scm_nil else return {var(_f, "f")((_x)[1] ), _map(_f,  (_x)[2] )} end
 end });
 ignore(_map)ignore(_pushS45macroS33(symbol('let'),  (function(_macroS45arguments  )
 return (function(_args ,  _vals ,  _body  )
 return {_makeS45lambda(var(_args, "args"),  var(_body, "body") ), var(_vals, "vals")}
 end)(_map(var(_car, "car"),  (var(_macroS45arguments, "macro-arguments"))[1] ),  _map(_cadr,  (_macroS45arguments)[1] ),  (_macroS45arguments)[2] )
 end) ))_member = setmetatable({args={symbol('x'),{symbol('xs'),scm_nil}},doc="Test whether ,x is a member of ,xs, using '= as an equality\
  predicate."}, { __call = function(_S35S46904 ,  _x ,  _xs  )
 if var(_nullS63, "null?")(var(_xs, "xs") ) then return false else if _S61((_xs)[1],  var(_x, "x") ) then return true else return _member(_x,  (_xs)[2] ) end end
 end });
 ignore(_member)_append = setmetatable({args={symbol('xs'),{symbol('ys'),scm_nil}},doc="Append the list ,xs with the list ,ys, such that the result shares\
  structure with ,ys."}, { __call = function(_S35S46907 ,  _xs ,  _ys  )
 if (function(_S35S46909)
 if _S35S46909 then
 return true else
 return _not(var(_pairS63, "pair?")(var(_xs, "xs") ) )
 end
 end)(var(_nullS63, "null?")(_xs )) then return var(_ys, "ys") else return {(_xs)[1], _append((_xs)[2],  _ys )} end
 end });
 ignore(_append)_expandS47helper = setmetatable({args={symbol('shadow'),{symbol('s'),scm_nil}},doc="Expand the expression ,s, without considering the symbols in ,shadow\
  as possibly macros."}, { __call = function(_S35S46910 ,  _shadow ,  _s  )
 if (function(_S35S46912)
 if _S35S46912 then
 return (function(_S35S46913)
 if _S35S46913 then
 return _not(_member((var(_s, "s"))[1],  var(_shadow, "shadow") ) ) else
 return false
 end
 end)(var(_symbolS63, "symbol?")((_s)[1] )) else
 return false
 end
 end)(var(_pairS63, "pair?")(_s )) then if _eqS63((_s)[1],  symbol('quote') ) then return _s else if (function(_S35S46916)
 if _S35S46916 then
 return _symbolS63(_cadr(_s ) ) else
 return false
 end
 end)(_eqS63((_s)[1],  keyword('prim') )) then return _cadr(_s ) else if _eqS63((_s)[1],  symbol('unquote') ) then return var(_eval, "eval")(_cadr(_s ) ) else if _eqS63((_s)[1],  symbol('lambda') ) then return _makeS45lambda(_cadr(_s ),  _expandS47helper(_append((function(_S35S46919)
 if _S35S46919 then
 return _list(_cadr(_s ) ) else
 return _cadr(_s )
 end
 end)(_symbolS63(_cadr(_s ) )),  _shadow ),  _cddr(_s ) ) ) else return (function(_mS45entry  )
 if var(_mS45entry, "m-entry") then return _expandS47helper(_shadow,  _mS45entry((_s)[2] ) ) else return {_expandS47helper(_shadow,  (_s)[1] ), _expandS47helper(_shadow,  (_s)[2] )} end
 end)(_lookupS45macroS45in((_s)[1],  _macros ) ) end end end end else if _pairS63(_s ) then return {_expandS47helper(_shadow,  (_s)[1] ), _expandS47helper(_shadow,  (_s)[2] )} else return _s end end
 end });
 ignore(_expandS47helper)_expand = setmetatable({args={symbol('s'),scm_nil},doc="Macro-expand the expression ,s"}, { __call = function(_S35S46922 ,  _s  )
 return _expandS47helper(scm_nil,  var(_s, "s") )
 end });
 ignore(_expand)_andS45expander = setmetatable({args={symbol('macro-arguments'),scm_nil},doc="Macro expander for 'and expressions."}, { __call = function(_S35S46923 ,  _macroS45arguments  )
 if var(_nullS63, "null?")(var(_macroS45arguments, "macro-arguments") ) then return true else if _nullS63((_macroS45arguments)[2] ) then return (_macroS45arguments)[1] else return _list(symbol('if'),  (_macroS45arguments)[1],  _andS45expander((_macroS45arguments)[2] ),  false ) end end
 end });
 ignore(_andS45expander)_orS45expander = setmetatable({args={symbol('macro-arguments'),scm_nil},doc="Macro expander for 'or expressions."}, { __call = function(_S35S46926 ,  _macroS45arguments  )
 return (function(_name  )
 if var(_nullS63, "null?")(var(_macroS45arguments, "macro-arguments") ) then return false else return _list(_list(symbol('lambda'),  _list(var(_name, "name") ),  _list(symbol('if'),  _name,  _name,  _orS45expander((_macroS45arguments)[2] ) ) ),  (_macroS45arguments)[1] ) end
 end)(var(_gensym, "gensym")() )
 end });
 ignore(_orS45expander)ignore(_pushS45macroS33(symbol('and'),  _andS45expander ))ignore(_pushS45macroS33(symbol('or'),  _orS45expander ))_quasiquoteS47helper = setmetatable({args={symbol('do-quote'),{symbol('expr'),scm_nil}},doc="Macro expander for quasiquote expressions."}, { __call = function(_S35S46928 ,  _doS45quote ,  _expr  )
 if (function(_S35S46930)
 if _S35S46930 then
 return _S61(symbol('unquote'),  (var(_expr, "expr"))[1] ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_expr )) then return _cadr(_expr ) else if _pairS63(_expr ) then if (function(_S35S46933)
 if _S35S46933 then
 return _S61(symbol('unquote-splicing'),  _caar(_expr ) ) else
 return false
 end
 end)(_pairS63((_expr)[1] )) then return _list(symbol('append'),  (_cdar(_expr ))[1],  _quasiquoteS47helper(true,  (_expr)[2] ) ) else if _S61((_expr)[1],  symbol('unquote-splicing') ) then return _cadr(_expr ) else return _list(symbol('cons'),  _quasiquoteS47helper(true,  (_expr)[1] ),  _quasiquoteS47helper(true,  (_expr)[2] ) ) end end else if var(_doS45quote, "do-quote") then return _list(symbol('quote'),  _expr ) else return _expr end end end
 end });
 ignore(_quasiquoteS47helper)ignore(_pushS45macroS33(symbol('quasiquote'),  (function(_args  )
 if (function(_S35S46937)
 if _S35S46937 then
 return var(_nullS63, "null?")((var(_args, "args"))[2] ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_args )) then return _quasiquoteS47helper(true,  (_args)[1] ) else return var(_error, "error")("bad quasiquote" ) end
 end) ))ignore(_pushS45macroS33(symbol('define-syntax'),  (function(_macroS45arguments  )
 if (function(_S35S46939)
 if _S35S46939 then
 return var(_pairS63, "pair?")((var(_macroS45arguments, "macro-arguments"))[2] ) else
 return false
 end
 end)(_pairS63((_macroS45arguments)[1] )) then return (function(_name ,  _args ,  _body ,  _macroS45args  )
 local _push;
 _push = {symbol('push-macro!'), {{symbol('quote'), {var(_name, "name"), scm_nil}}, {_makeS45lambda(_list(var(_macroS45args, "macro-args") ),  {{symbol('apply'), {_makeS45lambda(var(_args, "args"),  var(_body, "body") ), {_macroS45args, scm_nil}}}, scm_nil} ), scm_nil}}};
 var(_eval, "eval")(var(_push, "push") );
 return _push
 end)(_caar(_macroS45arguments ),  _cdar(_macroS45arguments ),  (_macroS45arguments)[2],  var(_gensym, "gensym")() ) else if var(_symbolS63, "symbol?")((_macroS45arguments)[1] ) then return (function(_arg  )
 local _push;
 _push = {symbol('push-macro!'), {{symbol('quote'), {(_macroS45arguments)[1], scm_nil}}, {{symbol('lambda'), {{var(_arg, "arg"), scm_nil}, {{symbol('apply'), {_cadr(_macroS45arguments ), {_arg, scm_nil}}}, scm_nil}}}, scm_nil}}};
 var(_eval, "eval")(var(_push, "push") );
 return _push
 end)(_gensym() ) else return var(_error, "error")("bad define-syntax" ) end end
 end) ))ignore(_pushS45macroS33(symbol('begin'),  (function(_macroS45arguments  )
 return {_makeS45lambda(scm_nil,  var(_macroS45arguments, "macro-arguments") ), scm_nil}
 end) ))ignore(_pushS45macroS33(symbol('cond'),  (function(_S35S46941  )
 return var(_apply, "apply")((function(...)
 local _cases = list(...);
 local _expand;
 _expand = (function(_cases  )
 if var(_nullS63, "null?")(var(_cases, "cases") ) then return false else return {symbol('if'), {_caar(_cases ), {{symbol('begin'), _cdar(_cases )}, {_expand((_cases)[2] ), scm_nil}}}} end
 end);
 return _expand(var(_cases, "cases") )
 end),  var(_S35S46941, "#.941") )
 end) ))_makeS45parameter = setmetatable({args=symbol('initval'),doc="Make a parameter, or dynamic binding, with the initial value given by\
  ,initval.\
  A parameter is a binding that, while lexically bound (as everything in\
  Scheme), has its value controlled dynamically. To change a parameter,\
  call it a single argument: this will be the new value.\
  If no parameters are given in the call, the stored value is returned."}, { __call = function(_S35S46947 ,  ...)
 local _initval = list(...);
 return (function(_cell  )
 return (function(...)
 local _args = list(...);
 if var(_nullS63, "null?")(var(_args, "args") ) then return var(_cell, "cell") else return (function()
 local _old;
 _old = _cell;
 (function()
 _cell = scm_set_helper(_cell, (_args)[1], "cell");
 return _cell
 end)();
 return var(_old, "old")
 end)() end
 end)
 end)((function(_S35S46944  )
 if var(_S35S46944, "#.944") then return _S35S46944 else return (function(_S35S46945  )
 if var(_S35S46945, "#.945") then return _S35S46945 else return false end
 end)(false ) end
 end)((var(_initval, "initval"))[1] ) )
 end });
 ignore(_makeS45parameter)ignore(_pushS45macroS33(symbol('parameterise'),  (function(_S35S46951  )
 return var(_apply, "apply")((function(_vars ,  ...)
 local _body = list(...);
 if var(_nullS63, "null?")(var(_vars, "vars") ) then return {symbol('begin'), var(_body, "body")} else return (function()
 local _saved;
 _saved = var(_gensym, "gensym")();
 local _rv;
 _rv = _gensym();
 return {symbol('let'), {{{var(_saved, "saved"), {{_caar(_vars ), scm_nil}, scm_nil}}, scm_nil}, {{_caar(_vars ), {{symbol('begin'), _cdar(_vars )}, scm_nil}}, {{symbol('let'), {{{var(_rv, "rv"), {{symbol('parameterise'), {(_vars)[2], _body}}, scm_nil}}, scm_nil}, {{_caar(_vars ), {_saved, scm_nil}}, {_rv, scm_nil}}}}, scm_nil}}}}
 end)() end
 end),  var(_S35S46951, "#.951") )
 end) ))_makeS45hashS45table = (function(_emptyS45hash  )
 local _go;
 _go = (function(_table ,  _args  )
 if var(_nullS63, "null?")(var(_args, "args") ) then return var(_table, "table") else return (function()
 _hashS45setS33(_table,  _caar(_args ),  _cdar(_args ) );
 return var(_go, "go")(_table,  (_args)[2] )
 end)() end
 end);
 return setmetatable({args=symbol('args'),doc="Make a hash table with the associations given as pairs in ,args."}, { __call = function(_S35S46955 ,  ...)
 local _args = list(...);
 local _table;
 _table = var(_emptyS45hash, "empty-hash")();
 return var(_go, "go")(var(_table, "table"),  var(_args, "args") )
 end })
 end)(_makeS45hashS45table );
 ignore(_makeS45hashS45table)_length = setmetatable({args={symbol('l'),symbol('a')},doc="Compute the length of the list ,l. Optionally, use ('car ,a) as the\
  length of the empty list. (Leave ,a empty for 0)."}, { __call = function(_S35S46956 ,  _l ,  ...)
 local _a = list(...);
 if var(_nullS63, "null?")(var(_a, "a") ) then return (function()
 return _length(var(_l, "l"),  0 )
 end)() else if _nullS63(var(_l, "l") ) then return (function()
 return (_a)[1]
 end)() else if var(_pairS63, "pair?")(_l ) then return (function()
 return _length((_l)[2],  var(_S43, "+")(1,  (_a)[1] ) )
 end)() else return false end end end
 end });
 ignore(_length)_else = true;
 ignore(_else)_safe = _makeS45parameter(true );
 ignore(_safe)ignore(_pushS45macroS33(symbol('check-parameter'),  (function(_S35S46960  )
 return var(_apply, "apply")((function(_val ,  _predS63 ,  _func  )
 if _safe() then return {symbol('if'), {{symbol('not'), {{var(_predS63, "pred?"), {var(_val, "val"), scm_nil}}, scm_nil}}, {{symbol('error'), {"In function ", {{symbol('quote'), {var(_func, "func"), scm_nil}}, {": the argument ", {{symbol('quote'), {_val, scm_nil}}, {"was expected to be a ", {{symbol('quote'), {_predS63, scm_nil}}, scm_nil}}}}}}}, scm_nil}}} else return false end
 end),  var(_S35S46960, "#.960") )
 end) ))ignore(_pushS45macroS33(symbol('when'),  (function(_S35S46963  )
 return var(_apply, "apply")((function(_c ,  ...)
 local _b = list(...);
 return {symbol('if'), {var(_c, "c"), {{symbol('begin'), var(_b, "b")}, {false, scm_nil}}}}
 end),  var(_S35S46963, "#.963") )
 end) ))ignore(_pushS45macroS33(symbol('unless'),  (function(_S35S46964  )
 return var(_apply, "apply")((function(_c ,  ...)
 local _b = list(...);
 return {symbol('if'), {var(_c, "c"), {false, {{symbol('begin'), var(_b, "b")}, scm_nil}}}}
 end),  var(_S35S46964, "#.964") )
 end) ))_documentationS45forS45procedure = setmetatable({args={symbol('f'),scm_nil},doc="Return the documentation for ,f, if any exists.\
   Documentation exists for procedures defined with a string as the\
   first element of their bodies, given that the string is not the only\
   expression."}, { __call = function(_S35S46965 ,  _f  )
 if var(_procedureS63, "procedure?")(var(_f, "f") ) then return (function()
 return var(_catch, "catch")((function()
 return var(_hashS45ref, "hash-ref")(_f,  "doc" )
 end),  (function(_e  )
 return false
 end) )
 end)() else return false end
 end });
 ignore(_documentationS45forS45procedure)_call = setmetatable({args={symbol('obj'),{symbol('meth'),symbol('args')}},doc="Call the method ,meth on the object ,obj, giving the ,args as\
  variadic arguments."}, { __call = function(_S35S46967 ,  _obj ,  _meth ,  ...)
 local _args = list(...);
 return var(_apply, "apply")(var(_hashS45ref, "hash-ref")(var(_obj, "obj"),  var(_meth, "meth") ),  {_obj, var(_args, "args")} )
 end });
 ignore(_call)_callS42 = setmetatable({args={symbol('obj'),{symbol('meth'),symbol('args')}},doc="Call the method ,meth on the object ,obj, giving the ,args as\
  variadic arguments."}, { __call = function(_S35S46968 ,  _obj ,  _meth ,  ...)
 local _args = list(...);
 return var(_apply, "apply")(var(_hashS45ref, "hash-ref")(var(_obj, "obj"),  var(_meth, "meth") ),  var(_args, "args") )
 end });
 ignore(_callS42)ignore((function(_S35S46969)
 if _S35S46969 then
 return (function()
 _environment = scm_set_helper(_environment, (function()
 return var(_ENV, "ENV")
 end), "environment");
 return _environment
 end)() else
 return false
 end
 end)(_S61(_platform,  "Scheme 51" )))_exitS45error = _makeS45hashS45table();
 ignore(_exitS45error)_exitS45errorS63 = (function(_e  )
 return _S61(var(_e, "e"),  _exitS45error )
 end);
 ignore(_exitS45errorS63)_exit = (function()
 return var(_error, "error")(_exitS45error )
 end);
 ignore(_exit)_runS45withS45exit = (function(_thunk  )
 return var(_catch, "catch")(var(_thunk, "thunk"),  (function(_e  )
 if (function(_S35S46971)
 if _S35S46971 then
 return _exitS45errorS63((var(_e, "e"))[1] ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_e )) then return true else return var(_error, "error")(_e ) end
 end) )
 end);
 ignore(_runS45withS45exit)_caseS45pairS63 = (function(_p  )
 return var(_pairS63, "pair?")(var(_p, "p") )
 end);
 ignore(_caseS45pairS63)_S42caseS45void = _makeS45hashS45table();
 ignore(_S42caseS45void)ignore(_pushS45macroS33(symbol('*case'),  (function(_S35S46972  )
 return var(_apply, "apply")((function(_empty ,  _expr ,  _head ,  ...)
 local _cases = list(...);
 local _matchS45sym;
 _matchS45sym = var(_gensym, "gensym")();
 local _compileS45match;
 _compileS45match = (function(_pattern ,  _matchS45sym ,  _body ,  _rest  )
 if _eqS63(symbol('else'),  var(_pattern, "pattern") ) then return (function()
 return var(_body, "body")
 end)() else if (function(_S35S46996  )
 if var(_S35S46996, "#.996") then return _S35S46996 else return (function(_S35S46997  )
 if var(_S35S46997, "#.997") then return _S35S46997 else return (function(_S35S46998  )
 if var(_S35S46998, "#.998") then return _S35S46998 else return (function(_S35S46999  )
 if var(_S35S46999, "#.999") then return _S35S46999 else return (function(_S35S461000  )
 if var(_S35S461000, "#.1000") then return _S35S461000 else return (function(_S35S461001  )
 if var(_S35S461001, "#.1001") then return _S35S461001 else return false end
 end)((function(_S35S461011)
 if _S35S461011 then
 return _S61((_pattern)[1],  symbol('quote') ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_pattern )) ) end
 end)(var(_nullS63, "null?")(_pattern ) ) end
 end)(var(_numberS63, "number?")(_pattern ) ) end
 end)(var(_stringS63, "string?")(_pattern ) ) end
 end)(_eqS63(_pattern,  false ) ) end
 end)(_eqS63(_pattern,  true ) ) then return (function()
 return {symbol('if'), {{symbol('='), {_pattern, {var(_matchS45sym, "match-sym"), scm_nil}}}, {var(_body, "body"), {var(_rest, "rest")(), scm_nil}}}}
 end)() else if var(_symbolS63, "symbol?")(_pattern ) then return (function()
 return {symbol('let'), {{{_pattern, {var(_matchS45sym, "match-sym"), scm_nil}}, scm_nil}, {var(_body, "body"), scm_nil}}}
 end)() else if (function(_S35S461014)
 if _S35S461014 then
 return (function(_S35S461015)
 if _S35S461015 then
 return (function(_S35S461016)
 if _S35S461016 then
 return _eqS63(_cadr(_pattern ),  keyword('when') ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_cddr(_pattern ) )) else
 return false
 end
 end)(_pairS63((_pattern)[2] )) else
 return false
 end
 end)(_pairS63(_pattern )) then return (function()
 local _join;
 _join = _gensym();
 return {symbol('let'), {{{var(_join, "join"), {{symbol('lambda'), {scm_nil, {var(_rest, "rest")(), scm_nil}}}, scm_nil}}, scm_nil}, {var(_compileS45match, "compile-match")((_pattern)[1],  var(_matchS45sym, "match-sym"),  {symbol('if'), {(_cddr(_pattern ))[1], {var(_body, "body"), {{_join, scm_nil}, scm_nil}}}},  (function()
 return {_join, scm_nil}
 end) ), scm_nil}}}
 end)() else if _pairS63(_pattern ) then return (function()
 local _join;
 _join = _gensym();
 return {symbol('let'), {{{var(_join, "join"), {{symbol('lambda'), {scm_nil, {var(_rest, "rest")(), scm_nil}}}, scm_nil}}, scm_nil}, {{symbol('if'), {{symbol('case-pair?'), {var(_matchS45sym, "match-sym"), scm_nil}}, {var(_compileS45match, "compile-match")((_pattern)[1],  {symbol('car'), {_matchS45sym, scm_nil}},  _compileS45match((_pattern)[2],  {symbol('cdr'), {_matchS45sym, scm_nil}},  var(_body, "body"),  (function()
 return {_join, scm_nil}
 end) ),  (function()
 return {_join, scm_nil}
 end) ), {{_join, scm_nil}, scm_nil}}}}, scm_nil}}}
 end)() else return false end end end end end
 end);
 local _expand;
 _expand = (function(_cases  )
 if var(_nullS63, "null?")(var(_cases, "cases") ) then return var(_empty, "empty") else return var(_compileS45match, "compile-match")(_caar(_cases ),  var(_matchS45sym, "match-sym"),  {symbol('begin'), _cdar(_cases )},  (function()
 return _expand((_cases)[2] )
 end) ) end
 end);
 return {symbol('let'), {{{var(_matchS45sym, "match-sym"), {var(_expr, "expr"), scm_nil}}, scm_nil}, {_expand({var(_head, "head"), var(_cases, "cases")} ), scm_nil}}}
 end),  var(_S35S46972, "#.972") )
 end) ))ignore(_pushS45macroS33(symbol('case'),  (function(_S35S461019  )
 return var(_apply, "apply")((function(_expr ,  _case1 ,  ...)
 local _cases = list(...);
 return {symbol('*case'), {false, {var(_expr, "expr"), {var(_case1, "case1"), var(_cases, "cases")}}}}
 end),  var(_S35S461019, "#.1019") )
 end) ))ignore(_pushS45macroS33(symbol('case!'),  (function(_S35S461020  )
 return var(_apply, "apply")((function(_expr ,  _case1 ,  ...)
 local _cases = list(...);
 return {symbol('*case'), {{symbol('error'), {"no matching case for ", {var(_expr, "expr"), {" in ", {{symbol('quote'), {{var(_case1, "case1"), var(_cases, "cases")}, scm_nil}}, scm_nil}}}}}, {_expr, {_case1, _cases}}}}
 end),  var(_S35S461020, "#.1020") )
 end) ))ignore(_pushS45macroS33(symbol('case-lambda'),  (function(_S35S461021  )
 return var(_apply, "apply")((function(...)
 local _args = list(...);
 local _name;
 _name = var(_gensym, "gensym")();
 return (function(_S35S461031  )
 return (function(_S35S461032  )
 return (function(_S35S461034  )
 if _caseS45pairS63(var(_S35S461031, "#.1031") ) then return (function(_docs  )
 return (function(_S35S461035  )
 if _caseS45pairS63((_S35S461031)[2] ) then return (function(_case1  )
 return (function(_cases  )
 if var(_stringS63, "string?")(var(_docs, "docs") ) then return (function()
 return {symbol('lambda'), {var(_name, "name"), {_docs, {{symbol('case!'), {_name, {var(_case1, "case1"), var(_cases, "cases")}}}, scm_nil}}}}
 end)() else return var(_S35S461032, "#.1032")() end
 end)(((_S35S461031)[2])[2] )
 end)(((_S35S461031)[2])[1] ) else return var(_S35S461035, "#.1035")() end
 end)((function()
 return var(_S35S461034, "#.1034")()
 end) )
 end)((_S35S461031)[1] ) else return var(_S35S461034, "#.1034")() end
 end)((function()
 return var(_S35S461032, "#.1032")()
 end) )
 end)((function()
 return (function(_S35S461033  )
 if _caseS45pairS63(var(_S35S461031, "#.1031") ) then return (function(_case1  )
 return (function(_cases  )
 return (function()
 return {symbol('lambda'), {var(_name, "name"), {{symbol('case!'), {_name, {var(_case1, "case1"), var(_cases, "cases")}}}, scm_nil}}}
 end)()
 end)((_S35S461031)[2] )
 end)((_S35S461031)[1] ) else return var(_S35S461033, "#.1033")() end
 end)((function()
 return false
 end) )
 end) )
 end)(var(_args, "args") )
 end),  var(_S35S461021, "#.1021") )
 end) ))ignore(_pushS45macroS33(symbol('let-values'),  (function(_S35S461040  )
 return var(_apply, "apply")((function(...)
 local _S35S461055 = list(...);
 return (function(_S35S461056  )
 return (function(_S35S461057  )
 if _caseS45pairS63(var(_S35S461056, "#.1056") ) then if _S61(scm_nil,  (_S35S461056)[1] ) then return (function(_body  )
 return (function()
 return {symbol('begin'), var(_body, "body")}
 end)()
 end)((_S35S461056)[2] ) else return var(_S35S461057, "#.1057")() end else return _S35S461057() end
 end)((function()
 return (function(_S35S461058  )
 if _caseS45pairS63(var(_S35S461056, "#.1056") ) then return (function(_S35S461059  )
 if _caseS45pairS63((_S35S461056)[1] ) then return (function(_S35S461060  )
 if _caseS45pairS63(((_S35S461056)[1])[1] ) then return (function(_names  )
 return (function(_S35S461061  )
 if _caseS45pairS63((((_S35S461056)[1])[1])[2] ) then return (function(_expr  )
 if _S61(scm_nil,  ((((_S35S461056)[1])[1])[2])[2] ) then return (function(_rest  )
 return (function(_body  )
 return (function()
 return {symbol('call-with-values'), {{symbol('lambda'), {scm_nil, {var(_expr, "expr"), scm_nil}}}, {{symbol('lambda'), {var(_names, "names"), {{symbol('let-values'), {var(_rest, "rest"), var(_body, "body")}}, scm_nil}}}, scm_nil}}}
 end)()
 end)((_S35S461056)[2] )
 end)(((_S35S461056)[1])[2] ) else return var(_S35S461061, "#.1061")() end
 end)(((((_S35S461056)[1])[1])[2])[1] ) else return var(_S35S461061, "#.1061")() end
 end)((function()
 return var(_S35S461060, "#.1060")()
 end) )
 end)((((_S35S461056)[1])[1])[1] ) else return var(_S35S461060, "#.1060")() end
 end)((function()
 return var(_S35S461059, "#.1059")()
 end) ) else return var(_S35S461059, "#.1059")() end
 end)((function()
 return var(_S35S461058, "#.1058")()
 end) ) else return var(_S35S461058, "#.1058")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461055, "#.1055"),  " in ",  {{{scm_nil,symbol('body')},{{symbol('quasiquote'),{{symbol('begin'),{symbol('unquote'),{symbol('body'),scm_nil}}},scm_nil}},scm_nil}},{{{{{symbol('names'),{symbol('expr'),scm_nil}},symbol('rest')},symbol('body')},{{symbol('quasiquote'),{{symbol('call-with-values'),{{symbol('lambda'),{scm_nil,{{symbol('unquote'),{symbol('expr'),scm_nil}},scm_nil}}},{{symbol('lambda'),{{symbol('unquote'),{symbol('names'),scm_nil}},{{symbol('let-values'),{{symbol('unquote'),{symbol('rest'),scm_nil}},{symbol('unquote'),{symbol('body'),scm_nil}}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}},scm_nil}} )
 end) )
 end) )
 end)(var(_S35S461055, "#.1055") )
 end),  var(_S35S461040, "#.1040") )
 end) ))ignore(_pushS45macroS33(symbol('let'),  (function(_S35S461069  )
 return var(_apply, "apply")((function(...)
 local _S35S461097 = list(...);
 return (function(_S35S461098  )
 return (function(_S35S461099  )
 return (function(_S35S461105  )
 if _caseS45pairS63(var(_S35S461098, "#.1098") ) then return (function(_name  )
 return (function(_S35S461106  )
 if _caseS45pairS63((_S35S461098)[2] ) then return (function(_S35S461107  )
 if _caseS45pairS63(((_S35S461098)[2])[1] ) then return (function(_S35S461108  )
 if _caseS45pairS63((((_S35S461098)[2])[1])[1] ) then return (function(_name1  )
 return (function(_S35S461109  )
 if _caseS45pairS63(((((_S35S461098)[2])[1])[1])[2] ) then return (function(_init1  )
 if _S61(scm_nil,  (((((_S35S461098)[2])[1])[1])[2])[2] ) then return (function(_vars  )
 return (function(_body  )
 if var(_symbolS63, "symbol?")(var(_name, "name") ) then return (function()
 return {symbol('begin'), {{symbol('define'), {_name, {{symbol('lambda'), {{var(_name1, "name1"), _map(var(_car, "car"),  var(_vars, "vars") )}, var(_body, "body")}}, scm_nil}}}, {{_name, {var(_init1, "init1"), _map(_cadr,  _vars )}}, scm_nil}}}
 end)() else return var(_S35S461099, "#.1099")() end
 end)(((_S35S461098)[2])[2] )
 end)((((_S35S461098)[2])[1])[2] ) else return var(_S35S461109, "#.1109")() end
 end)((((((_S35S461098)[2])[1])[1])[2])[1] ) else return var(_S35S461109, "#.1109")() end
 end)((function()
 return var(_S35S461108, "#.1108")()
 end) )
 end)(((((_S35S461098)[2])[1])[1])[1] ) else return var(_S35S461108, "#.1108")() end
 end)((function()
 return var(_S35S461107, "#.1107")()
 end) ) else return var(_S35S461107, "#.1107")() end
 end)((function()
 return var(_S35S461106, "#.1106")()
 end) ) else return var(_S35S461106, "#.1106")() end
 end)((function()
 return var(_S35S461105, "#.1105")()
 end) )
 end)((_S35S461098)[1] ) else return var(_S35S461105, "#.1105")() end
 end)((function()
 return var(_S35S461099, "#.1099")()
 end) )
 end)((function()
 return (function(_S35S461100  )
 if _caseS45pairS63(var(_S35S461098, "#.1098") ) then return (function(_S35S461102  )
 if _caseS45pairS63((_S35S461098)[1] ) then return (function(_S35S461103  )
 if _caseS45pairS63(((_S35S461098)[1])[1] ) then return (function(_name1  )
 return (function(_S35S461104  )
 if _caseS45pairS63((((_S35S461098)[1])[1])[2] ) then return (function(_init1  )
 if _S61(scm_nil,  ((((_S35S461098)[1])[1])[2])[2] ) then return (function(_vars  )
 return (function(_body  )
 return (function()
 return {{symbol('lambda'), {{var(_name1, "name1"), _map(var(_car, "car"),  var(_vars, "vars") )}, var(_body, "body")}}, {var(_init1, "init1"), _map(_cadr,  _vars )}}
 end)()
 end)((_S35S461098)[2] )
 end)(((_S35S461098)[1])[2] ) else return var(_S35S461104, "#.1104")() end
 end)(((((_S35S461098)[1])[1])[2])[1] ) else return var(_S35S461104, "#.1104")() end
 end)((function()
 return var(_S35S461103, "#.1103")()
 end) )
 end)((((_S35S461098)[1])[1])[1] ) else return var(_S35S461103, "#.1103")() end
 end)((function()
 return var(_S35S461102, "#.1102")()
 end) ) else return var(_S35S461102, "#.1102")() end
 end)((function()
 return var(_S35S461100, "#.1100")()
 end) ) else return var(_S35S461100, "#.1100")() end
 end)((function()
 return (function(_S35S461101  )
 if _caseS45pairS63(var(_S35S461098, "#.1098") ) then if _S61(scm_nil,  (_S35S461098)[1] ) then return (function(_body  )
 return (function()
 return {symbol('begin'), var(_body, "body")}
 end)()
 end)((_S35S461098)[2] ) else return var(_S35S461101, "#.1101")() end else return _S35S461101() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461097, "#.1097"),  " in ",  {{{{symbol('name'),{{{symbol('name1'),{symbol('init1'),scm_nil}},symbol('vars')},symbol('body')}},{keyword('when'),{{symbol('symbol?'),{symbol('name'),scm_nil}},scm_nil}}},{{symbol('quasiquote'),{{symbol('begin'),{{symbol('define'),{{symbol('unquote'),{symbol('name'),scm_nil}},{{symbol('lambda'),{{{symbol('unquote'),{symbol('name1'),scm_nil}},{symbol('unquote'),{{symbol('map'),{symbol('car'),{symbol('vars'),scm_nil}}},scm_nil}}},{symbol('unquote'),{symbol('body'),scm_nil}}}},scm_nil}}},{{{symbol('unquote'),{symbol('name'),scm_nil}},{{symbol('unquote'),{symbol('init1'),scm_nil}},{symbol('unquote'),{{symbol('map'),{symbol('cadr'),{symbol('vars'),scm_nil}}},scm_nil}}}},scm_nil}}},scm_nil}},scm_nil}},{{{{{symbol('name1'),{symbol('init1'),scm_nil}},symbol('vars')},symbol('body')},{{symbol('quasiquote'),{{{symbol('lambda'),{{{symbol('unquote'),{symbol('name1'),scm_nil}},{symbol('unquote'),{{symbol('map'),{symbol('car'),{symbol('vars'),scm_nil}}},scm_nil}}},{symbol('unquote'),{symbol('body'),scm_nil}}}},{{symbol('unquote'),{symbol('init1'),scm_nil}},{symbol('unquote'),{{symbol('map'),{symbol('cadr'),{symbol('vars'),scm_nil}}},scm_nil}}}},scm_nil}},scm_nil}},{{{scm_nil,symbol('body')},{{symbol('quasiquote'),{{symbol('begin'),{symbol('unquote'),{symbol('body'),scm_nil}}},scm_nil}},scm_nil}},scm_nil}}} )
 end) )
 end) )
 end) )
 end)(var(_S35S461097, "#.1097") )
 end),  var(_S35S461069, "#.1069") )
 end) ))ignore(_pushS45macroS33(symbol('letrec'),  (function(_S35S461124  )
 return var(_apply, "apply")((function(_vars ,  ...)
 local _body = list(...);
 return {{symbol('lambda'), {_map(var(_car, "car"),  var(_vars, "vars") ), {{symbol('begin'), _map((function(_x  )
 return {symbol('set!'), {(var(_x, "x"))[1], {_cadr(_x ), scm_nil}}}
 end),  _vars )}, var(_body, "body")}}}, _map((function(...)
 local _a = list(...);
 return false
 end),  _vars )}
 end),  var(_S35S461124, "#.1124") )
 end) ))ignore(_pushS45macroS33(symbol('let-syntax'),  (function(_S35S461125  )
 return var(_apply, "apply")((function()
 _go = (function(...)
 local _S35S461140 = list(...);
 return (function(_S35S461141  )
 return (function(_S35S461142  )
 if _caseS45pairS63(var(_S35S461141, "#.1141") ) then return (function(_S35S461144  )
 if _caseS45pairS63((_S35S461141)[1] ) then return (function(_S35S461145  )
 if _caseS45pairS63(((_S35S461141)[1])[1] ) then return (function(_var  )
 return (function(_S35S461146  )
 if _caseS45pairS63((((_S35S461141)[1])[1])[2] ) then return (function(_macro  )
 if _S61(scm_nil,  ((((_S35S461141)[1])[1])[2])[2] ) then return (function(_vars  )
 return (function(_body  )
 return (function()
 local _saved;
 _saved = _lookupS45macroS45in(var(_var, "var"),  _macros );
 _pushS45macroS33(_var,  var(_eval, "eval")(var(_macro, "macro") ) );
 return (function(_b  )
 _pushS45macroS33(_var,  var(_saved, "saved") );
 return var(_b, "b")
 end)(_apply(_go,  {var(_vars, "vars"), var(_body, "body")} ) )
 end)()
 end)((_S35S461141)[2] )
 end)(((_S35S461141)[1])[2] ) else return var(_S35S461146, "#.1146")() end
 end)(((((_S35S461141)[1])[1])[2])[1] ) else return var(_S35S461146, "#.1146")() end
 end)((function()
 return var(_S35S461145, "#.1145")()
 end) )
 end)((((_S35S461141)[1])[1])[1] ) else return var(_S35S461145, "#.1145")() end
 end)((function()
 return var(_S35S461144, "#.1144")()
 end) ) else return var(_S35S461144, "#.1144")() end
 end)((function()
 return var(_S35S461142, "#.1142")()
 end) ) else return var(_S35S461142, "#.1142")() end
 end)((function()
 return (function(_S35S461143  )
 if _caseS45pairS63(var(_S35S461141, "#.1141") ) then if _S61(scm_nil,  (_S35S461141)[1] ) then return (function(_body  )
 return (function()
 return {symbol('begin'), _expand(var(_body, "body") )}
 end)()
 end)((_S35S461141)[2] ) else return var(_S35S461143, "#.1143")() end else return _S35S461143() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461140, "#.1140"),  " in ",  {{{{{symbol('var'),{symbol('macro'),scm_nil}},symbol('vars')},symbol('body')},{{symbol('define'),{symbol('saved'),{{symbol('lookup-macro-in'),{symbol('var'),{symbol('macros'),scm_nil}}},scm_nil}}},{{symbol('push-macro!'),{symbol('var'),{{symbol('eval'),{symbol('macro'),scm_nil}},scm_nil}}},{{symbol('let'),{{{symbol('b'),{{symbol('apply'),{symbol('go'),{{symbol('cons'),{symbol('vars'),{symbol('body'),scm_nil}}},scm_nil}}},scm_nil}},scm_nil},{{symbol('push-macro!'),{symbol('var'),{symbol('saved'),scm_nil}}},{symbol('b'),scm_nil}}}},scm_nil}}}},{{{scm_nil,symbol('body')},{{symbol('quasiquote'),{{symbol('begin'),{symbol('unquote'),{{symbol('expand'),{symbol('body'),scm_nil}},scm_nil}}},scm_nil}},scm_nil}},scm_nil}} )
 end) )
 end) )
 end)(var(_S35S461140, "#.1140") )
 end);
 return _go
 end)(),  var(_S35S461125, "#.1125") )
 end) ))ignore(_pushS45macroS33(symbol('let*'),  (function(_S35S461154  )
 return var(_apply, "apply")((function(...)
 local _S35S461169 = list(...);
 return (function(_S35S461170  )
 return (function(_S35S461171  )
 if _caseS45pairS63(var(_S35S461170, "#.1170") ) then if _S61(scm_nil,  (_S35S461170)[1] ) then return (function(_body  )
 return (function()
 return {symbol('begin'), var(_body, "body")}
 end)()
 end)((_S35S461170)[2] ) else return var(_S35S461171, "#.1171")() end else return _S35S461171() end
 end)((function()
 return (function(_S35S461172  )
 if _caseS45pairS63(var(_S35S461170, "#.1170") ) then return (function(_S35S461173  )
 if _caseS45pairS63((_S35S461170)[1] ) then return (function(_S35S461174  )
 if _caseS45pairS63(((_S35S461170)[1])[1] ) then return (function(_var1  )
 return (function(_S35S461175  )
 if _caseS45pairS63((((_S35S461170)[1])[1])[2] ) then return (function(_exp1  )
 if _S61(scm_nil,  ((((_S35S461170)[1])[1])[2])[2] ) then return (function(_vars  )
 return (function(_body  )
 return (function()
 return {{symbol('lambda'), {{var(_var1, "var1"), scm_nil}, {{symbol('letrec'), {var(_vars, "vars"), var(_body, "body")}}, scm_nil}}}, {var(_exp1, "exp1"), scm_nil}}
 end)()
 end)((_S35S461170)[2] )
 end)(((_S35S461170)[1])[2] ) else return var(_S35S461175, "#.1175")() end
 end)(((((_S35S461170)[1])[1])[2])[1] ) else return var(_S35S461175, "#.1175")() end
 end)((function()
 return var(_S35S461174, "#.1174")()
 end) )
 end)((((_S35S461170)[1])[1])[1] ) else return var(_S35S461174, "#.1174")() end
 end)((function()
 return var(_S35S461173, "#.1173")()
 end) ) else return var(_S35S461173, "#.1173")() end
 end)((function()
 return var(_S35S461172, "#.1172")()
 end) ) else return var(_S35S461172, "#.1172")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461169, "#.1169"),  " in ",  {{{scm_nil,symbol('body')},{{symbol('quasiquote'),{{symbol('begin'),{symbol('unquote'),{symbol('body'),scm_nil}}},scm_nil}},scm_nil}},{{{{{symbol('var1'),{symbol('exp1'),scm_nil}},symbol('vars')},symbol('body')},{{symbol('quasiquote'),{{{symbol('lambda'),{{{symbol('unquote'),{symbol('var1'),scm_nil}},scm_nil},{{symbol('letrec'),{{symbol('unquote'),{symbol('vars'),scm_nil}},{symbol('unquote'),{symbol('body'),scm_nil}}}},scm_nil}}},{{symbol('unquote'),{symbol('exp1'),scm_nil}},scm_nil}},scm_nil}},scm_nil}},scm_nil}} )
 end) )
 end) )
 end)(var(_S35S461169, "#.1169") )
 end),  var(_S35S461154, "#.1154") )
 end) ))_max = setmetatable({args=symbol('#.1183'),doc="Return the maximum of the given arguments."}, { __call = function(_S35S461190 ,  ...)
 local _S35S461183 = list(...);
 return (function(_S35S461184  )
 return (function(_S35S461185  )
 if _caseS45pairS63(var(_S35S461184, "#.1184") ) then return (function(_x  )
 if _S61(scm_nil,  (_S35S461184)[2] ) then return (function()
 return var(_x, "x")
 end)() else return var(_S35S461185, "#.1185")() end
 end)((_S35S461184)[1] ) else return var(_S35S461185, "#.1185")() end
 end)((function()
 return (function(_S35S461186  )
 if _caseS45pairS63(var(_S35S461184, "#.1184") ) then return (function(_x  )
 return (function(_S35S461189  )
 if _caseS45pairS63((_S35S461184)[2] ) then return (function(_y  )
 if _S61(scm_nil,  ((_S35S461184)[2])[2] ) then return (function()
 if var(_S62, ">")(var(_x, "x"),  var(_y, "y") ) then return _x else return _y end
 end)() else return var(_S35S461189, "#.1189")() end
 end)(((_S35S461184)[2])[1] ) else return var(_S35S461189, "#.1189")() end
 end)((function()
 return var(_S35S461186, "#.1186")()
 end) )
 end)((_S35S461184)[1] ) else return var(_S35S461186, "#.1186")() end
 end)((function()
 return (function(_S35S461187  )
 if _caseS45pairS63(var(_S35S461184, "#.1184") ) then return (function(_x  )
 return (function(_S35S461188  )
 if _caseS45pairS63((_S35S461184)[2] ) then return (function(_y  )
 return (function(_z  )
 return (function()
 if var(_S62, ">")(var(_x, "x"),  var(_y, "y") ) then return var(_apply, "apply")(_max,  {_x, var(_z, "z")} ) else return _apply(_max,  {_y, _z} ) end
 end)()
 end)(((_S35S461184)[2])[2] )
 end)(((_S35S461184)[2])[1] ) else return var(_S35S461188, "#.1188")() end
 end)((function()
 return var(_S35S461187, "#.1187")()
 end) )
 end)((_S35S461184)[1] ) else return var(_S35S461187, "#.1187")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461183, "#.1183"),  " in ",  {{{symbol('x'),scm_nil},{symbol('x'),scm_nil}},{{{symbol('x'),{symbol('y'),scm_nil}},{{symbol('if'),{{symbol('>'),{symbol('x'),{symbol('y'),scm_nil}}},{symbol('x'),{symbol('y'),scm_nil}}}},scm_nil}},{{{symbol('x'),{symbol('y'),symbol('z')}},{{symbol('if'),{{symbol('>'),{symbol('x'),{symbol('y'),scm_nil}}},{{symbol('apply'),{symbol('max'),{{symbol('cons'),{symbol('x'),{symbol('z'),scm_nil}}},scm_nil}}},{{symbol('apply'),{symbol('max'),{{symbol('cons'),{symbol('y'),{symbol('z'),scm_nil}}},scm_nil}}},scm_nil}}}},scm_nil}},scm_nil}}} )
 end) )
 end) )
 end) )
 end)(var(_S35S461183, "#.1183") )
 end });
 ignore(_max)_min = setmetatable({args=symbol('#.1200'),doc="Return the minimum of the given arguments."}, { __call = function(_S35S461207 ,  ...)
 local _S35S461200 = list(...);
 return (function(_S35S461201  )
 return (function(_S35S461202  )
 if _caseS45pairS63(var(_S35S461201, "#.1201") ) then return (function(_x  )
 if _S61(scm_nil,  (_S35S461201)[2] ) then return (function()
 return var(_x, "x")
 end)() else return var(_S35S461202, "#.1202")() end
 end)((_S35S461201)[1] ) else return var(_S35S461202, "#.1202")() end
 end)((function()
 return (function(_S35S461203  )
 if _caseS45pairS63(var(_S35S461201, "#.1201") ) then return (function(_x  )
 return (function(_S35S461206  )
 if _caseS45pairS63((_S35S461201)[2] ) then return (function(_y  )
 if _S61(scm_nil,  ((_S35S461201)[2])[2] ) then return (function()
 if var(_S62, ">")(var(_x, "x"),  var(_y, "y") ) then return _y else return _x end
 end)() else return var(_S35S461206, "#.1206")() end
 end)(((_S35S461201)[2])[1] ) else return var(_S35S461206, "#.1206")() end
 end)((function()
 return var(_S35S461203, "#.1203")()
 end) )
 end)((_S35S461201)[1] ) else return var(_S35S461203, "#.1203")() end
 end)((function()
 return (function(_S35S461204  )
 if _caseS45pairS63(var(_S35S461201, "#.1201") ) then return (function(_x  )
 return (function(_S35S461205  )
 if _caseS45pairS63((_S35S461201)[2] ) then return (function(_y  )
 return (function(_z  )
 return (function()
 if var(_S62, ">")(var(_x, "x"),  var(_y, "y") ) then return var(_apply, "apply")(_min,  {_y, var(_z, "z")} ) else return _apply(_min,  {_x, _z} ) end
 end)()
 end)(((_S35S461201)[2])[2] )
 end)(((_S35S461201)[2])[1] ) else return var(_S35S461205, "#.1205")() end
 end)((function()
 return var(_S35S461204, "#.1204")()
 end) )
 end)((_S35S461201)[1] ) else return var(_S35S461204, "#.1204")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461200, "#.1200"),  " in ",  {{{symbol('x'),scm_nil},{symbol('x'),scm_nil}},{{{symbol('x'),{symbol('y'),scm_nil}},{{symbol('if'),{{symbol('>'),{symbol('x'),{symbol('y'),scm_nil}}},{symbol('y'),{symbol('x'),scm_nil}}}},scm_nil}},{{{symbol('x'),{symbol('y'),symbol('z')}},{{symbol('if'),{{symbol('>'),{symbol('x'),{symbol('y'),scm_nil}}},{{symbol('apply'),{symbol('min'),{{symbol('cons'),{symbol('y'),{symbol('z'),scm_nil}}},scm_nil}}},{{symbol('apply'),{symbol('min'),{{symbol('cons'),{symbol('x'),{symbol('z'),scm_nil}}},scm_nil}}},scm_nil}}}},scm_nil}},scm_nil}}} )
 end) )
 end) )
 end) )
 end)(var(_S35S461200, "#.1200") )
 end });
 ignore(_min)_caaar = (function(_x  )
 return (((var(_x, "x"))[1])[1])[1]
 end);
 ignore(_caaar)_caadr = (function(_x  )
 return (((var(_x, "x"))[2])[1])[1]
 end);
 ignore(_caadr)_cadar = (function(_x  )
 return (((var(_x, "x"))[1])[2])[1]
 end);
 ignore(_cadar)_caddr = (function(_x  )
 return (((var(_x, "x"))[2])[2])[1]
 end);
 ignore(_caddr)_cdaar = (function(_x  )
 return (((var(_x, "x"))[1])[1])[2]
 end);
 ignore(_cdaar)_cdadr = (function(_x  )
 return (((var(_x, "x"))[2])[1])[2]
 end);
 ignore(_cdadr)_cddar = (function(_x  )
 return (((var(_x, "x"))[1])[2])[2]
 end);
 ignore(_cddar)_cdddr = (function(_x  )
 return (((var(_x, "x"))[2])[2])[2]
 end);
 ignore(_cdddr)_caaaar = (function(_x  )
 return ((((var(_x, "x"))[1])[1])[1])[1]
 end);
 ignore(_caaaar)_caaadr = (function(_x  )
 return ((((var(_x, "x"))[2])[1])[1])[1]
 end);
 ignore(_caaadr)_caadar = (function(_x  )
 return ((((var(_x, "x"))[1])[2])[1])[1]
 end);
 ignore(_caadar)_caaddr = (function(_x  )
 return ((((var(_x, "x"))[2])[2])[1])[1]
 end);
 ignore(_caaddr)_cadaar = (function(_x  )
 return ((((var(_x, "x"))[1])[1])[2])[1]
 end);
 ignore(_cadaar)_cadadr = (function(_x  )
 return ((((var(_x, "x"))[2])[1])[2])[1]
 end);
 ignore(_cadadr)_caddar = (function(_x  )
 return ((((var(_x, "x"))[1])[2])[2])[1]
 end);
 ignore(_caddar)_cadddr = (function(_x  )
 return ((((var(_x, "x"))[2])[2])[2])[1]
 end);
 ignore(_cadddr)_cdaaar = (function(_x  )
 return ((((var(_x, "x"))[1])[1])[1])[2]
 end);
 ignore(_cdaaar)_cdaadr = (function(_x  )
 return ((((var(_x, "x"))[2])[1])[1])[2]
 end);
 ignore(_cdaadr)_cdadar = (function(_x  )
 return ((((var(_x, "x"))[1])[2])[1])[2]
 end);
 ignore(_cdadar)_cdaddr = (function(_x  )
 return ((((var(_x, "x"))[2])[2])[1])[2]
 end);
 ignore(_cdaddr)_cddaar = (function(_x  )
 return ((((var(_x, "x"))[1])[1])[2])[2]
 end);
 ignore(_cddaar)_cddadr = (function(_x  )
 return ((((var(_x, "x"))[2])[1])[2])[2]
 end);
 ignore(_cddadr)_cdddar = (function(_x  )
 return ((((var(_x, "x"))[1])[2])[2])[2]
 end);
 ignore(_cdddar)_cddddr = (function(_x  )
 return ((((var(_x, "x"))[1])[2])[2])[2]
 end);
 ignore(_cddddr)_first = var(_car, "car");
 ignore(_first)_second = _cadr;
 ignore(_second)_third = _caddr;
 ignore(_third)_fourth = _cadddr;
 ignore(_fourth)_fifth = (function(_x  )
 return (_cddddr(var(_x, "x") ))[1]
 end);
 ignore(_fifth)_sixth = (function(_x  )
 return _cadr(_cddddr(var(_x, "x") ) )
 end);
 ignore(_sixth)_seventh = (function(_x  )
 return _caddr(_cddddr(var(_x, "x") ) )
 end);
 ignore(_seventh)_eighth = (function(_x  )
 return _cadddr(_cddddr(var(_x, "x") ) )
 end);
 ignore(_eighth)_ninth = (function(_x  )
 return (_cddddr(_cddddr(var(_x, "x") ) ))[1]
 end);
 ignore(_ninth)_tenth = (function(_x  )
 return _cadr(_cddddr(_cddddr(var(_x, "x") ) ) )
 end);
 ignore(_tenth)_integerS63 = setmetatable({args={symbol('x'),scm_nil},doc="Return #t if ,x is an exact number that is not a rational."}, { __call = function(_S35S461217 ,  _x  )
 if var(_exactS63, "exact?")(var(_x, "x") ) then return _not(var(_rationalS63, "rational?")(_x ) ) else return false end
 end });
 ignore(_integerS63)_map = (function()
 local _fold1;
 _fold1 = (function(_f ,  _z ,  _xs  )
 if var(_nullS63, "null?")(var(_xs, "xs") ) then return var(_z, "z") else return var(_f, "f")((_xs)[1],  var(_fold1, "fold1")(_f,  _z,  (_xs)[2] ) ) end
 end);
 return setmetatable({args=symbol('#.1219'),doc="(map proc list ...) applies proc element-wise to the elements of\
       list, in order, producing a list of the results. If more than one\
       list is given, the result has the same length as the smallest of the\
       arguments."}, { __call = function(_S35S461229 ,  ...)
 local _S35S461219 = list(...);
 return (function(_S35S461220  )
 return (function(_S35S461221  )
 if _caseS45pairS63(var(_S35S461220, "#.1220") ) then return (function(_f  )
 return (function(_S35S461227  )
 if _caseS45pairS63((_S35S461220)[2] ) then return (function(_l  )
 if _S61(scm_nil,  ((_S35S461220)[2])[2] ) then return (function()
 (function(_S35S461233)
 if _S35S461233 then
 return var(_error, "error")("In function ",  symbol('map'),  ": the argument ",  symbol('f'),  "was expected to be a ",  symbol('procedure?') ) else
 return false
 end
 end)(_not(var(_procedureS63, "procedure?")(var(_f, "f") ) ));
 return (function()
 local _go;
 _go = (function(_l  )
 if var(_nullS63, "null?")(var(_l, "l") ) then return scm_nil else return {_f((_l)[1] ), _map(_f,  (_l)[2] )} end
 end);
 return var(_go, "go")(var(_l, "l") )
 end)()
 end)() else return var(_S35S461227, "#.1227")() end
 end)(((_S35S461220)[2])[1] ) else return var(_S35S461227, "#.1227")() end
 end)((function()
 return var(_S35S461221, "#.1221")()
 end) )
 end)((_S35S461220)[1] ) else return var(_S35S461221, "#.1221")() end
 end)((function()
 return (function(_S35S461222  )
 if _caseS45pairS63(var(_S35S461220, "#.1220") ) then return (function(_f  )
 return (function(_S35S461225  )
 if _caseS45pairS63((_S35S461220)[2] ) then return (function(_l1  )
 return (function(_S35S461226  )
 if _caseS45pairS63(((_S35S461220)[2])[2] ) then return (function(_l2  )
 if _S61(scm_nil,  (((_S35S461220)[2])[2])[2] ) then return (function()
 (function(_S35S461239)
 if _S35S461239 then
 return var(_error, "error")("In function ",  symbol('map'),  ": the argument ",  symbol('f'),  "was expected to be a ",  symbol('procedure?') ) else
 return false
 end
 end)(_not(var(_procedureS63, "procedure?")(var(_f, "f") ) ));
 return (function()
 local _go;
 _go = (function(_l1 ,  _l2  )
 if var(_nullS63, "null?")(var(_l1, "l1") ) then return (function()
 return scm_nil
 end)() else if _nullS63(var(_l2, "l2") ) then return (function()
 return scm_nil
 end)() else if _else then return (function()
 return {_f((_l1)[1],  (_l2)[1] ), var(_go, "go")((_l2)[2],  (_l2)[2] )}
 end)() else return false end end end
 end);
 return var(_go, "go")(var(_l1, "l1"),  var(_l2, "l2") )
 end)()
 end)() else return var(_S35S461226, "#.1226")() end
 end)((((_S35S461220)[2])[2])[1] ) else return var(_S35S461226, "#.1226")() end
 end)((function()
 return var(_S35S461225, "#.1225")()
 end) )
 end)(((_S35S461220)[2])[1] ) else return var(_S35S461225, "#.1225")() end
 end)((function()
 return var(_S35S461222, "#.1222")()
 end) )
 end)((_S35S461220)[1] ) else return var(_S35S461222, "#.1222")() end
 end)((function()
 return (function(_S35S461223  )
 if _caseS45pairS63(var(_S35S461220, "#.1220") ) then return (function(_f  )
 return (function(_S35S461224  )
 if _caseS45pairS63((_S35S461220)[2] ) then return (function(_l1  )
 return (function(_ls  )
 return (function()
 (function(_S35S461245)
 if _S35S461245 then
 return var(_error, "error")("In function ",  symbol('map'),  ": the argument ",  symbol('f'),  "was expected to be a ",  symbol('procedure?') ) else
 return false
 end
 end)(_not(var(_procedureS63, "procedure?")(var(_f, "f") ) ));
 local _len;
 _len = var(_fold1, "fold1")((function(_ls ,  _len  )
 return _min(_length(var(_ls, "ls") ),  var(_len, "len") )
 end),  _length(var(_l1, "l1") ),  var(_ls, "ls") );
 return (function()
 local _go;
 _go = (function(_l1 ,  _ls ,  _len  )
 if _S61(var(_len, "len"),  0 ) then return scm_nil else return {var(_apply, "apply")(_f,  {(var(_l1, "l1"))[1], _map(_car,  var(_ls, "ls") )} ), var(_go, "go")((_l1)[2],  _map(var(_cdr, "cdr"),  _ls ),  var(_S45, "-")(_len,  1 ) )} end
 end);
 return var(_go, "go")(_l1,  _ls,  var(_len, "len") )
 end)()
 end)()
 end)(((_S35S461220)[2])[2] )
 end)(((_S35S461220)[2])[1] ) else return var(_S35S461224, "#.1224")() end
 end)((function()
 return var(_S35S461223, "#.1223")()
 end) )
 end)((_S35S461220)[1] ) else return var(_S35S461223, "#.1223")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461219, "#.1219"),  " in ",  {{{symbol('f'),{symbol('l'),scm_nil}},{{symbol('check-parameter'),{symbol('f'),{symbol('procedure?'),{symbol('map'),scm_nil}}}},{{symbol('let'),{symbol('go'),{{{symbol('l'),{symbol('l'),scm_nil}},scm_nil},{{symbol('if'),{{symbol('null?'),{symbol('l'),scm_nil}},{{symbol('quote'),{scm_nil,scm_nil}},{{symbol('cons'),{{symbol('f'),{{symbol('car'),{symbol('l'),scm_nil}},scm_nil}},{{symbol('map'),{symbol('f'),{{symbol('cdr'),{symbol('l'),scm_nil}},scm_nil}}},scm_nil}}},scm_nil}}}},scm_nil}}}},scm_nil}}},{{{symbol('f'),{symbol('l1'),{symbol('l2'),scm_nil}}},{{symbol('check-parameter'),{symbol('f'),{symbol('procedure?'),{symbol('map'),scm_nil}}}},{{symbol('let'),{symbol('go'),{{{symbol('l1'),{symbol('l1'),scm_nil}},{{symbol('l2'),{symbol('l2'),scm_nil}},scm_nil}},{{symbol('cond'),{{{symbol('null?'),{symbol('l1'),scm_nil}},{{symbol('quote'),{scm_nil,scm_nil}},scm_nil}},{{{symbol('null?'),{symbol('l2'),scm_nil}},{{symbol('quote'),{scm_nil,scm_nil}},scm_nil}},{{symbol('else'),{{symbol('cons'),{{symbol('f'),{{symbol('car'),{symbol('l1'),scm_nil}},{{symbol('car'),{symbol('l2'),scm_nil}},scm_nil}}},{{symbol('go'),{{symbol('cdr'),{symbol('l2'),scm_nil}},{{symbol('cdr'),{symbol('l2'),scm_nil}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}},scm_nil}}}},scm_nil}}},{{{symbol('f'),{symbol('l1'),symbol('ls')}},{{symbol('check-parameter'),{symbol('f'),{symbol('procedure?'),{symbol('map'),scm_nil}}}},{{symbol('define'),{symbol('len'),{{symbol('fold1'),{{symbol('lambda'),{{symbol('ls'),{symbol('len'),scm_nil}},{{symbol('min'),{{symbol('length'),{symbol('ls'),scm_nil}},{symbol('len'),scm_nil}}},scm_nil}}},{{symbol('length'),{symbol('l1'),scm_nil}},{symbol('ls'),scm_nil}}}},scm_nil}}},{{symbol('let'),{symbol('go'),{{{symbol('l1'),{symbol('l1'),scm_nil}},{{symbol('ls'),{symbol('ls'),scm_nil}},{{symbol('len'),{symbol('len'),scm_nil}},scm_nil}}},{{symbol('if'),{{symbol('='),{symbol('len'),{0,scm_nil}}},{{symbol('quote'),{scm_nil,scm_nil}},{{symbol('cons'),{{symbol('apply'),{symbol('f'),{{symbol('cons'),{{symbol('car'),{symbol('l1'),scm_nil}},{{symbol('map'),{symbol('car'),{symbol('ls'),scm_nil}}},scm_nil}}},scm_nil}}},{{symbol('go'),{{symbol('cdr'),{symbol('l1'),scm_nil}},{{symbol('map'),{symbol('cdr'),{symbol('ls'),scm_nil}}},{{symbol('-'),{symbol('len'),{1,scm_nil}}},scm_nil}}}},scm_nil}}},scm_nil}}}},scm_nil}}}},scm_nil}}}},scm_nil}}} )
 end) )
 end) )
 end) )
 end)(var(_S35S461219, "#.1219") )
 end })
 end)();
 ignore(_map)_forS45each = (function()
 local _fold1;
 _fold1 = (function(_f ,  _z ,  _xs  )
 if var(_nullS63, "null?")(var(_xs, "xs") ) then return var(_z, "z") else return var(_f, "f")((_xs)[1],  var(_fold1, "fold1")(_f,  _z,  (_xs)[2] ) ) end
 end);
 return setmetatable({args=symbol('#.1247'),doc="(for-each proc list ...) applies proc element-wise to the\
       elements of list, in order, discarding the results. If more than\
       one list is given, the result has the same length as the smallest\
       of the arguments."}, { __call = function(_S35S461257 ,  ...)
 local _S35S461247 = list(...);
 return (function(_S35S461248  )
 return (function(_S35S461249  )
 if _caseS45pairS63(var(_S35S461248, "#.1248") ) then return (function(_f  )
 return (function(_S35S461255  )
 if _caseS45pairS63((_S35S461248)[2] ) then return (function(_l  )
 if _S61(scm_nil,  ((_S35S461248)[2])[2] ) then return (function()
 (function(_S35S461261)
 if _S35S461261 then
 return var(_error, "error")("In function ",  symbol('map'),  ": the argument ",  symbol('f'),  "was expected to be a ",  symbol('procedure?') ) else
 return false
 end
 end)(_not(var(_procedureS63, "procedure?")(var(_f, "f") ) ));
 return (function()
 local _go;
 _go = (function(_l  )
 if var(_nullS63, "null?")(var(_l, "l") ) then return scm_nil else return (function()
 _f((_l)[1] );
 return _map(_f,  (_l)[2] )
 end)() end
 end);
 return var(_go, "go")(var(_l, "l") )
 end)()
 end)() else return var(_S35S461255, "#.1255")() end
 end)(((_S35S461248)[2])[1] ) else return var(_S35S461255, "#.1255")() end
 end)((function()
 return var(_S35S461249, "#.1249")()
 end) )
 end)((_S35S461248)[1] ) else return var(_S35S461249, "#.1249")() end
 end)((function()
 return (function(_S35S461250  )
 if _caseS45pairS63(var(_S35S461248, "#.1248") ) then return (function(_f  )
 return (function(_S35S461253  )
 if _caseS45pairS63((_S35S461248)[2] ) then return (function(_l1  )
 return (function(_S35S461254  )
 if _caseS45pairS63(((_S35S461248)[2])[2] ) then return (function(_l2  )
 if _S61(scm_nil,  (((_S35S461248)[2])[2])[2] ) then return (function()
 (function(_S35S461267)
 if _S35S461267 then
 return var(_error, "error")("In function ",  symbol('map'),  ": the argument ",  symbol('f'),  "was expected to be a ",  symbol('procedure?') ) else
 return false
 end
 end)(_not(var(_procedureS63, "procedure?")(var(_f, "f") ) ));
 return (function()
 local _go;
 _go = (function(_l1 ,  _l2  )
 if var(_nullS63, "null?")(var(_l1, "l1") ) then return (function()
 return scm_nil
 end)() else if _nullS63(var(_l2, "l2") ) then return (function()
 return scm_nil
 end)() else if _else then return (function()
 _f((_l1)[1],  (_l2)[1] );
 return var(_go, "go")((_l2)[2],  (_l2)[2] )
 end)() else return false end end end
 end);
 return var(_go, "go")(var(_l1, "l1"),  var(_l2, "l2") )
 end)()
 end)() else return var(_S35S461254, "#.1254")() end
 end)((((_S35S461248)[2])[2])[1] ) else return var(_S35S461254, "#.1254")() end
 end)((function()
 return var(_S35S461253, "#.1253")()
 end) )
 end)(((_S35S461248)[2])[1] ) else return var(_S35S461253, "#.1253")() end
 end)((function()
 return var(_S35S461250, "#.1250")()
 end) )
 end)((_S35S461248)[1] ) else return var(_S35S461250, "#.1250")() end
 end)((function()
 return (function(_S35S461251  )
 if _caseS45pairS63(var(_S35S461248, "#.1248") ) then return (function(_f  )
 return (function(_S35S461252  )
 if _caseS45pairS63((_S35S461248)[2] ) then return (function(_l1  )
 return (function(_ls  )
 return (function()
 (function(_S35S461273)
 if _S35S461273 then
 return var(_error, "error")("In function ",  symbol('map'),  ": the argument ",  symbol('f'),  "was expected to be a ",  symbol('procedure?') ) else
 return false
 end
 end)(_not(var(_procedureS63, "procedure?")(var(_f, "f") ) ));
 local _len;
 _len = var(_fold1, "fold1")((function(_ls ,  _len  )
 return _min(_length(var(_ls, "ls") ),  var(_len, "len") )
 end),  _length(var(_l1, "l1") ),  var(_ls, "ls") );
 return (function()
 local _go;
 _go = (function(_l1 ,  _ls ,  _len  )
 if _S61(var(_len, "len"),  0 ) then return scm_nil else return (function()
 var(_apply, "apply")(_f,  {(var(_l1, "l1"))[1], _map(_car,  var(_ls, "ls") )} );
 return var(_go, "go")((_l1)[2],  _map(var(_cdr, "cdr"),  _ls ),  var(_S45, "-")(_len,  1 ) )
 end)() end
 end);
 return var(_go, "go")(_l1,  _ls,  var(_len, "len") )
 end)()
 end)()
 end)(((_S35S461248)[2])[2] )
 end)(((_S35S461248)[2])[1] ) else return var(_S35S461252, "#.1252")() end
 end)((function()
 return var(_S35S461251, "#.1251")()
 end) )
 end)((_S35S461248)[1] ) else return var(_S35S461251, "#.1251")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461247, "#.1247"),  " in ",  {{{symbol('f'),{symbol('l'),scm_nil}},{{symbol('check-parameter'),{symbol('f'),{symbol('procedure?'),{symbol('map'),scm_nil}}}},{{symbol('let'),{symbol('go'),{{{symbol('l'),{symbol('l'),scm_nil}},scm_nil},{{symbol('if'),{{symbol('null?'),{symbol('l'),scm_nil}},{{symbol('quote'),{scm_nil,scm_nil}},{{symbol('begin'),{{symbol('f'),{{symbol('car'),{symbol('l'),scm_nil}},scm_nil}},{{symbol('map'),{symbol('f'),{{symbol('cdr'),{symbol('l'),scm_nil}},scm_nil}}},scm_nil}}},scm_nil}}}},scm_nil}}}},scm_nil}}},{{{symbol('f'),{symbol('l1'),{symbol('l2'),scm_nil}}},{{symbol('check-parameter'),{symbol('f'),{symbol('procedure?'),{symbol('map'),scm_nil}}}},{{symbol('let'),{symbol('go'),{{{symbol('l1'),{symbol('l1'),scm_nil}},{{symbol('l2'),{symbol('l2'),scm_nil}},scm_nil}},{{symbol('cond'),{{{symbol('null?'),{symbol('l1'),scm_nil}},{{symbol('quote'),{scm_nil,scm_nil}},scm_nil}},{{{symbol('null?'),{symbol('l2'),scm_nil}},{{symbol('quote'),{scm_nil,scm_nil}},scm_nil}},{{symbol('else'),{{symbol('f'),{{symbol('car'),{symbol('l1'),scm_nil}},{{symbol('car'),{symbol('l2'),scm_nil}},scm_nil}}},{{symbol('go'),{{symbol('cdr'),{symbol('l2'),scm_nil}},{{symbol('cdr'),{symbol('l2'),scm_nil}},scm_nil}}},scm_nil}}},scm_nil}}}},scm_nil}}}},scm_nil}}},{{{symbol('f'),{symbol('l1'),symbol('ls')}},{{symbol('check-parameter'),{symbol('f'),{symbol('procedure?'),{symbol('map'),scm_nil}}}},{{symbol('define'),{symbol('len'),{{symbol('fold1'),{{symbol('lambda'),{{symbol('ls'),{symbol('len'),scm_nil}},{{symbol('min'),{{symbol('length'),{symbol('ls'),scm_nil}},{symbol('len'),scm_nil}}},scm_nil}}},{{symbol('length'),{symbol('l1'),scm_nil}},{symbol('ls'),scm_nil}}}},scm_nil}}},{{symbol('let'),{symbol('go'),{{{symbol('l1'),{symbol('l1'),scm_nil}},{{symbol('ls'),{symbol('ls'),scm_nil}},{{symbol('len'),{symbol('len'),scm_nil}},scm_nil}}},{{symbol('if'),{{symbol('='),{symbol('len'),{0,scm_nil}}},{{symbol('quote'),{scm_nil,scm_nil}},{{symbol('begin'),{{symbol('apply'),{symbol('f'),{{symbol('cons'),{{symbol('car'),{symbol('l1'),scm_nil}},{{symbol('map'),{symbol('car'),{symbol('ls'),scm_nil}}},scm_nil}}},scm_nil}}},{{symbol('go'),{{symbol('cdr'),{symbol('l1'),scm_nil}},{{symbol('map'),{symbol('cdr'),{symbol('ls'),scm_nil}}},{{symbol('-'),{symbol('len'),{1,scm_nil}}},scm_nil}}}},scm_nil}}},scm_nil}}}},scm_nil}}}},scm_nil}}}},scm_nil}}} )
 end) )
 end) )
 end) )
 end)(var(_S35S461247, "#.1247") )
 end })
 end)();
 ignore(_forS45each)_reverse = setmetatable({args={symbol('l'),scm_nil},doc="Reverse the list ,l."}, { __call = function(_S35S461275 ,  _l  )
 local _go;
 _go = (function(_acc ,  _l  )
 if var(_nullS63, "null?")(var(_l, "l") ) then return var(_acc, "acc") else return var(_go, "go")({(_l)[1], _acc},  (_l)[2] ) end
 end);
 return var(_go, "go")(scm_nil,  var(_l, "l") )
 end });
 ignore(_reverse)_numberS45S62string = setmetatable({args={symbol('x'),scm_nil},doc="Convert the number ,x to a string representation that can be read\
  back."}, { __call = function(_S35S461277 ,  _x  )
 return _callS47native(symbol('tostring'),  var(_x, "x") )
 end });
 ignore(_numberS45S62string)_stringS45S62number = setmetatable({args={symbol('x'),scm_nil},doc="Convert the string ,x to a number, returning #f on failure."}, { __call = function(_S35S461278 ,  _x  )
 if _callS47native(symbol('tonumber'),  var(_x, "x") ) then return _callS47native(symbol('tonumber'),  _x ) else return false end
 end });
 ignore(_stringS45S62number)_exactS45S62inexact = setmetatable({args={symbol('x'),scm_nil},doc="Convert an exact number (integer or rational) to an inexact number\
  (floating point). Note: Scheme 51 does *not* support inexact rational\
  numbers."}, { __call = function(_S35S461280 ,  _x  )
 (function()
 (function(_S35S461281)
 if _S35S461281 then
 return var(_display, "display")("\
",  "function _over(_x ,  _y  ) return _x / _y end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _over(_x ,  _y  ) return _x / _y end" )()
 end)();
 if var(_rationalS63, "rational?")(var(_x, "x") ) then return var(_over, "over")((_x)[1],  (_x)[2] ) else return _x end
 end });
 ignore(_exactS45S62inexact)_symbolS45S62string = setmetatable({args={symbol('x'),scm_nil},doc="Return the string contents of the symbol ,x."}, { __call = function(_S35S461283 ,  _x  )
 return (var(_x, "x"))[1]
 end });
 ignore(_symbolS45S62string)_stringS45S62symbol = setmetatable({args={symbol('x'),scm_nil},doc="Return a symbol with string contents ,x. This symbol is guaranteed to\
  be 'eq? to any other symbol with the same string contents. In\
  addition, they are guaranteed to be the same object in memory."}, { __call = function(_S35S461284 ,  _x  )
 return _callS47native(symbol('symbol'),  var(_x, "x") )
 end });
 ignore(_stringS45S62symbol)_hashS45tableS45size = setmetatable({args={symbol('table'),scm_nil},doc="Return the number of entries in the hash-table ,table."}, { __call = function(_S35S461285 ,  _table  )
 local _x;
 _x = 1;
 var(_hashS45forS45each, "hash-for-each")(var(_table, "table"),  (function(_k ,  _v  )
 return (function()
 _x = scm_set_helper(_x, var(_S43, "+")(1,  var(_x, "x") ), "x");
 return _x
 end)()
 end) );
 return var(_x, "x")
 end });
 ignore(_hashS45tableS45size)_remainder = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Return the remainder of the division of ,x by ,y."}, { __call = function(_S35S461286 ,  _x ,  _y  )
 return var(_S45, "-")(var(_x, "x"),  var(_S42, "*")(var(_y, "y"),  var(_floorS47, "floor/")(_x,  _y ) ) )
 end });
 ignore(_remainder)_floorS47 = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Number-theoretic division (integer) division of ,x by ,y. This\
  procedure returns two integers, ,nq and ,nr, such that ,nq is the\
  floor of the result of the division and ,nr is the remainder."}, { __call = function(_S35S461287 ,  _x ,  _y  )
 return (function(_nq  )
 return var(_values, "values")(var(_nq, "nq"),  var(_S45, "-")(var(_x, "x"),  var(_S42, "*")(var(_y, "y"),  _nq ) ) )
 end)(var(_floor, "floor")(var(_S47, "/")(var(_x, "x"),  var(_y, "y") ) ) )
 end });
 ignore(_floorS47)_floorS45quotient = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Return the quotient of ,x by ,y in the sense of 'floor/."}, { __call = function(_S35S461288 ,  _x ,  _y  )
 return var(_callS45withS45values, "call-with-values")((function()
 return _floorS47(var(_x, "x"),  var(_y, "y") )
 end),  (function(_nq ,  _nr  )
 return var(_nq, "nq")
 end) )
 end });
 ignore(_floorS45quotient)_floorS45remainder = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Return the remainder of ,x by ,y in the sense of 'floor/."}, { __call = function(_S35S461289 ,  _x ,  _y  )
 return var(_callS45withS45values, "call-with-values")((function()
 return _floorS47(var(_x, "x"),  var(_y, "y") )
 end),  (function(_nq ,  _nr  )
 return var(_nqr, "nqr")
 end) )
 end });
 ignore(_floorS45remainder)_truncateS47 = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Number-theoretic division (integer) division of ,x by ,y. This\
  procedure returns two integers, ,nq and ,nr, such that ,nq is the\
  truncation of the result of the division and ,nr is the remainder."}, { __call = function(_S35S461290 ,  _x ,  _y  )
 return (function(_nq  )
 return var(_values, "values")(var(_nq, "nq"),  var(_S45, "-")(var(_x, "x"),  var(_S42, "*")(var(_y, "y"),  _nq ) ) )
 end)(var(_truncate, "truncate")(var(_S47, "/")(var(_x, "x"),  var(_y, "y") ) ) )
 end });
 ignore(_truncateS47)_truncateS45quotient = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Return the quotient of ,x by ,y in the sense of 'truncate/."}, { __call = function(_S35S461291 ,  _x ,  _y  )
 return var(_callS45withS45values, "call-with-values")((function()
 return _truncateS47(var(_x, "x"),  var(_y, "y") )
 end),  (function(_nq ,  _nr  )
 return var(_nq, "nq")
 end) )
 end });
 ignore(_truncateS45quotient)_truncateS45remainder = setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Return the remainder of ,x by ,y in the sense of 'truncate/."}, { __call = function(_S35S461292 ,  _x ,  _y  )
 return var(_callS45withS45values, "call-with-values")((function()
 return _truncateS47(var(_x, "x"),  var(_y, "y") )
 end),  (function(_nq ,  _nr  )
 return var(_nqr, "nqr")
 end) )
 end });
 ignore(_truncateS45remainder)_inexactS45S62exact = setmetatable({args={symbol('q'),scm_nil},doc="If ,q is an inexact number, return a rational approximation ,r of ,q\
  such that ,q and ,r differ by at most 10^-x, where -x is the number of\
  decimal digits reported by (round (/ (log (floating-part ,r)) (log 10)))."}, { __call = function(_S35S461293 ,  _q  )
 if var(_exactS63, "exact?")(var(_q, "q") ) then return (function()
 return _q
 end)() else if var(_inexactS63, "inexact?")(_q ) then return (function()
 return var(_callS45withS45values, "call-with-values")((function()
 return _callS47native({symbol('math'),{symbol('modf'),scm_nil}},  _q )
 end),  (function(_int ,  _frac  )
 local _digits;
 _digits = var(_S43, "+")(1,  var(_round, "round")(var(_S47, "/")(var(_log, "log")(var(_frac, "frac") ),  _log(10 ) ) ) );
 return _callS47native(symbol('rational'),  _S43(var(_int, "int"),  var(_S42, "*")(_frac,  var(_expt, "expt")(10,  var(_digits, "digits") ) ) ),  _expt(10,  _digits ) )
 end) )
 end)() else return false end end
 end });
 ignore(_inexactS45S62exact)_exact = setmetatable({args={symbol('q'),scm_nil},doc="R7RS name for 'inexact->exact."}, { __call = function(_S35S461296 ,  _q  )
 return _inexactS45S62exact(var(_q, "q") )
 end });
 ignore(_exact)_inexact = setmetatable({args={symbol('q'),scm_nil},doc="R7RS name for 'exact->inexact"}, { __call = function(_S35S461297 ,  _q  )
 return _exactS45S62inexact(var(_q, "q") )
 end });
 ignore(_inexact)_fractionalS45part = setmetatable({args={symbol('r'),scm_nil},doc="Return the fractional part of ,r."}, { __call = function(_S35S461298 ,  _r  )
 return var(_callS45withS45values, "call-with-values")((function()
 return _callS47native({symbol('math'),{symbol('modf'),scm_nil}},  var(_r, "r") )
 end),  (function(_x ,  _y  )
 return var(_y, "y")
 end) )
 end });
 ignore(_fractionalS45part)_numerator = setmetatable({args={symbol('q'),scm_nil},doc="Return the numerator of ,q, with ,q expressed as a fraction in\
  simplest terms if it is an inexact number. If ,q is an exact integer,\
  return ,q. For details of the conversion, see 'inexact->exact."}, { __call = function(_S35S461299 ,  _q  )
 if var(_rationalS63, "rational?")(var(_q, "q") ) then return (function()
 return (_q)[2]
 end)() else if var(_exactS63, "exact?")(_q ) then return (function()
 return _q
 end)() else if _else then return (function()
 return _numerator(_inexactS45S62exact(_q ) )
 end)() else return false end end end
 end });
 ignore(_numerator)_denominator = setmetatable({args={symbol('q'),scm_nil},doc="Return the denominator of ,q, with ,q expressed as a fraction in\
  simplest terms if it is an inexact number. If ,q is an exact integer,\
  return 1. For details of the conversion, see 'inexact->exact."}, { __call = function(_S35S461303 ,  _q  )
 if var(_rationalS63, "rational?")(var(_q, "q") ) then return (function()
 return (_q)[2]
 end)() else if var(_exactS63, "exact?")(_q ) then return (function()
 return 1
 end)() else if _else then return (function()
 return _denominator(_inexactS45S62exact(_q ) )
 end)() else return false end end end
 end });
 ignore(_denominator)_floor = setmetatable({args={symbol('x'),scm_nil},doc="Return ,x rounded down. If x is an exact rational, interpret it as an\
  inexact float first."}, { __call = function(_S35S461307 ,  _x  )
 return _callS47native({symbol('math'),{symbol('floor'),scm_nil}},  _exactS45S62inexact(var(_x, "x") ) )
 end });
 ignore(_floor)_ceiling = setmetatable({args={symbol('x'),scm_nil},doc="Return ,x rounded up. If x is an exact rational, interpret it as an\
  inexact float first."}, { __call = function(_S35S461308 ,  _x  )
 return _callS47native({symbol('math'),{symbol('ceil'),scm_nil}},  _exactS45S62inexact(var(_x, "x") ) )
 end });
 ignore(_ceiling)_round = setmetatable({args={symbol('x'),scm_nil},doc="Return ,x rounded towards the nearest integer. If x is an exact\
  rational, interpret it as an inexact float first."}, { __call = function(_S35S461309 ,  _x  )
 (function()
 _x = scm_set_helper(_x, _exactS45S62inexact(var(_x, "x") ), "x");
 return _x
 end)();
 if var(_S62S61, ">=")(_x,  0 ) then return _callS47native({symbol('math'),{symbol('floor'),scm_nil}},  var(_S43, "+")(_x,  0.5 ) ) else return _callS47native({symbol('math'),{symbol('ceil'),scm_nil}},  var(_S45, "-")(_x,  0.5 ) ) end
 end });
 ignore(_round)_truncate = setmetatable({args={symbol('x'),scm_nil},doc="Returns the integer closest to ,x whose absolute value is not greater\
  than |,x|."}, { __call = function(_S35S461311 ,  _x  )
 return var(_callS45withS45values, "call-with-values")((function()
 return _callS47native({symbol('math'),{symbol('modf'),scm_nil}},  var(_x, "x") )
 end),  (function(_int ,  _frac  )
 return var(_int, "int")
 end) )
 end });
 ignore(_truncate)_nanS63 = setmetatable({args={symbol('x'),scm_nil},doc="Returns #t if x is the floating point value NaN."}, { __call = function(_S35S461312 ,  _x  )
 return _not(_S61(var(_x, "x"),  _x ) )
 end });
 ignore(_nanS63)ignore(_pushS45macroS33(symbol('define-iterated-comparison'),  (function(_S35S461313  )
 return var(_apply, "apply")((function(_operator ,  _defn ,  _pred ,  _docstring  )
 local _op;
 _op = var(_gensym, "gensym")();
 local _go;
 _go = _gensym();
 local _a;
 _a = _gensym();
 local _b;
 _b = _gensym();
 local _c;
 _c = _gensym();
 return {symbol('define'), {var(_operator, "operator"), {{symbol('begin'), {{symbol('define'), {{var(_op, "op"), {var(_a, "a"), {var(_b, "b"), scm_nil}}}, {(function(_S35S461315)
 if _S35S461315 then
 return false else
 return {symbol('begin'), {{symbol('check-parameter'), {_a, {var(_pred, "pred"), {{symbol('quote'), {_operator, scm_nil}}, scm_nil}}}}, {{symbol('check-parameter'), {_b, {_pred, {{symbol('quote'), {_operator, scm_nil}}, scm_nil}}}}, scm_nil}}}
 end
 end)(_S61(_pred,  false )), {{{symbol('call/native'), {{symbol('quote'), {symbol('load'), scm_nil}}, {var(_defn, "defn"), scm_nil}}}, {_a, {_b, scm_nil}}}, scm_nil}}}}, {{symbol('define'), {var(_go, "go"), {{symbol('case-lambda'), {var(_docstring, "docstring"), {{scm_nil, {true, scm_nil}}, {{{_a, scm_nil}, {true, scm_nil}}, {{{_a, {_b, scm_nil}}, {{_op, {_a, {_b, scm_nil}}}, scm_nil}}, {{{_a, {_b, var(_c, "c")}}, {{symbol('and'), {{_op, {_a, {_b, scm_nil}}}, {{symbol('apply'), {_go, {{symbol('cons'), {_b, {_c, scm_nil}}}, scm_nil}}}, scm_nil}}}, scm_nil}}, scm_nil}}}}}}, scm_nil}}}, {_go, scm_nil}}}}, scm_nil}}}
 end),  var(_S35S461313, "#.1313") )
 end) ))_stringS60S63 = (function()
 local _S35S461316;
 _S35S461316 = (function(_S35S461318 ,  _S35S461319  )
 (function()
 (function(_S35S461328)
 if _S35S461328 then
 return var(_error, "error")("In function ",  {symbol('quote'),{symbol('string<?'),scm_nil}},  ": the argument ",  symbol('#.1318'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_S35S461318, "#.1318") ) ));
 if _not(_stringS63(var(_S35S461319, "#.1319") ) ) then return _error("In function ",  {symbol('quote'),{symbol('string<?'),scm_nil}},  ": the argument ",  symbol('#.1319'),  "was expected to be a ",  symbol('string?') ) else return false end
 end)();
 return _callS47native(symbol('load'),  "local x, y = ...; return x < y" )(var(_S35S461318, "#.1318"),  var(_S35S461319, "#.1319") )
 end);
 local _S35S461317;
 _S35S461317 = setmetatable({args=symbol('#.1321'),doc="Return #t if the sequence of strings given by the arguments is\
   monotonically increasing."}, { __call = function(_S35S461330 ,  ...)
 local _S35S461321 = list(...);
 return (function(_S35S461322  )
 if _S61(scm_nil,  var(_S35S461322, "#.1322") ) then return (function()
 return true
 end)() else return (function(_S35S461323  )
 if _caseS45pairS63(_S35S461322 ) then return (function(_S35S461318  )
 if _S61(scm_nil,  (_S35S461322)[2] ) then return (function()
 return true
 end)() else return var(_S35S461323, "#.1323")() end
 end)((_S35S461322)[1] ) else return var(_S35S461323, "#.1323")() end
 end)((function()
 return (function(_S35S461324  )
 if _caseS45pairS63(_S35S461322 ) then return (function(_S35S461318  )
 return (function(_S35S461327  )
 if _caseS45pairS63((_S35S461322)[2] ) then return (function(_S35S461319  )
 if _S61(scm_nil,  ((_S35S461322)[2])[2] ) then return (function()
 return var(_S35S461316, "#.1316")(var(_S35S461318, "#.1318"),  var(_S35S461319, "#.1319") )
 end)() else return var(_S35S461327, "#.1327")() end
 end)(((_S35S461322)[2])[1] ) else return var(_S35S461327, "#.1327")() end
 end)((function()
 return var(_S35S461324, "#.1324")()
 end) )
 end)((_S35S461322)[1] ) else return var(_S35S461324, "#.1324")() end
 end)((function()
 return (function(_S35S461325  )
 if _caseS45pairS63(_S35S461322 ) then return (function(_S35S461318  )
 return (function(_S35S461326  )
 if _caseS45pairS63((_S35S461322)[2] ) then return (function(_S35S461319  )
 return (function(_S35S461320  )
 return (function()
 if var(_S35S461316, "#.1316")(var(_S35S461318, "#.1318"),  var(_S35S461319, "#.1319") ) then return var(_apply, "apply")(var(_S35S461317, "#.1317"),  {_S35S461319, var(_S35S461320, "#.1320")} ) else return false end
 end)()
 end)(((_S35S461322)[2])[2] )
 end)(((_S35S461322)[2])[1] ) else return var(_S35S461326, "#.1326")() end
 end)((function()
 return var(_S35S461325, "#.1325")()
 end) )
 end)((_S35S461322)[1] ) else return var(_S35S461325, "#.1325")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461321, "#.1321"),  " in ",  {{scm_nil,{true,scm_nil}},{{{symbol('#.1318'),scm_nil},{true,scm_nil}},{{{symbol('#.1318'),{symbol('#.1319'),scm_nil}},{{symbol('#.1316'),{symbol('#.1318'),{symbol('#.1319'),scm_nil}}},scm_nil}},{{{symbol('#.1318'),{symbol('#.1319'),symbol('#.1320')}},{{symbol('and'),{{symbol('#.1316'),{symbol('#.1318'),{symbol('#.1319'),scm_nil}}},{{symbol('apply'),{symbol('#.1317'),{{symbol('cons'),{symbol('#.1319'),{symbol('#.1320'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461321, "#.1321") )
 end });
 return var(_S35S461317, "#.1317")
 end)();
 ignore(_stringS60S63)_stringS60S61S63 = (function()
 local _S35S461340;
 _S35S461340 = (function(_S35S461342 ,  _S35S461343  )
 (function()
 (function(_S35S461352)
 if _S35S461352 then
 return var(_error, "error")("In function ",  {symbol('quote'),{symbol('string<=?'),scm_nil}},  ": the argument ",  symbol('#.1342'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_S35S461342, "#.1342") ) ));
 if _not(_stringS63(var(_S35S461343, "#.1343") ) ) then return _error("In function ",  {symbol('quote'),{symbol('string<=?'),scm_nil}},  ": the argument ",  symbol('#.1343'),  "was expected to be a ",  symbol('string?') ) else return false end
 end)();
 return _callS47native(symbol('load'),  "local x, y = ...; return x <= y" )(var(_S35S461342, "#.1342"),  var(_S35S461343, "#.1343") )
 end);
 local _S35S461341;
 _S35S461341 = setmetatable({args=symbol('#.1345'),doc="Return #t if the sequence of strings given by the arguments is\
   monotonically nondecreasing."}, { __call = function(_S35S461354 ,  ...)
 local _S35S461345 = list(...);
 return (function(_S35S461346  )
 if _S61(scm_nil,  var(_S35S461346, "#.1346") ) then return (function()
 return true
 end)() else return (function(_S35S461347  )
 if _caseS45pairS63(_S35S461346 ) then return (function(_S35S461342  )
 if _S61(scm_nil,  (_S35S461346)[2] ) then return (function()
 return true
 end)() else return var(_S35S461347, "#.1347")() end
 end)((_S35S461346)[1] ) else return var(_S35S461347, "#.1347")() end
 end)((function()
 return (function(_S35S461348  )
 if _caseS45pairS63(_S35S461346 ) then return (function(_S35S461342  )
 return (function(_S35S461351  )
 if _caseS45pairS63((_S35S461346)[2] ) then return (function(_S35S461343  )
 if _S61(scm_nil,  ((_S35S461346)[2])[2] ) then return (function()
 return var(_S35S461340, "#.1340")(var(_S35S461342, "#.1342"),  var(_S35S461343, "#.1343") )
 end)() else return var(_S35S461351, "#.1351")() end
 end)(((_S35S461346)[2])[1] ) else return var(_S35S461351, "#.1351")() end
 end)((function()
 return var(_S35S461348, "#.1348")()
 end) )
 end)((_S35S461346)[1] ) else return var(_S35S461348, "#.1348")() end
 end)((function()
 return (function(_S35S461349  )
 if _caseS45pairS63(_S35S461346 ) then return (function(_S35S461342  )
 return (function(_S35S461350  )
 if _caseS45pairS63((_S35S461346)[2] ) then return (function(_S35S461343  )
 return (function(_S35S461344  )
 return (function()
 if var(_S35S461340, "#.1340")(var(_S35S461342, "#.1342"),  var(_S35S461343, "#.1343") ) then return var(_apply, "apply")(var(_S35S461341, "#.1341"),  {_S35S461343, var(_S35S461344, "#.1344")} ) else return false end
 end)()
 end)(((_S35S461346)[2])[2] )
 end)(((_S35S461346)[2])[1] ) else return var(_S35S461350, "#.1350")() end
 end)((function()
 return var(_S35S461349, "#.1349")()
 end) )
 end)((_S35S461346)[1] ) else return var(_S35S461349, "#.1349")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461345, "#.1345"),  " in ",  {{scm_nil,{true,scm_nil}},{{{symbol('#.1342'),scm_nil},{true,scm_nil}},{{{symbol('#.1342'),{symbol('#.1343'),scm_nil}},{{symbol('#.1340'),{symbol('#.1342'),{symbol('#.1343'),scm_nil}}},scm_nil}},{{{symbol('#.1342'),{symbol('#.1343'),symbol('#.1344')}},{{symbol('and'),{{symbol('#.1340'),{symbol('#.1342'),{symbol('#.1343'),scm_nil}}},{{symbol('apply'),{symbol('#.1341'),{{symbol('cons'),{symbol('#.1343'),{symbol('#.1344'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461345, "#.1345") )
 end });
 return var(_S35S461341, "#.1341")
 end)();
 ignore(_stringS60S61S63)_stringS62S63 = (function()
 local _S35S461364;
 _S35S461364 = (function(_S35S461366 ,  _S35S461367  )
 (function()
 (function(_S35S461376)
 if _S35S461376 then
 return var(_error, "error")("In function ",  {symbol('quote'),{symbol('string>?'),scm_nil}},  ": the argument ",  symbol('#.1366'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_S35S461366, "#.1366") ) ));
 if _not(_stringS63(var(_S35S461367, "#.1367") ) ) then return _error("In function ",  {symbol('quote'),{symbol('string>?'),scm_nil}},  ": the argument ",  symbol('#.1367'),  "was expected to be a ",  symbol('string?') ) else return false end
 end)();
 return _callS47native(symbol('load'),  "local x, y = ...; return x > y" )(var(_S35S461366, "#.1366"),  var(_S35S461367, "#.1367") )
 end);
 local _S35S461365;
 _S35S461365 = setmetatable({args=symbol('#.1369'),doc="Return #t if the sequence of strings given by the arguments is\
   monotonically decreasing."}, { __call = function(_S35S461378 ,  ...)
 local _S35S461369 = list(...);
 return (function(_S35S461370  )
 if _S61(scm_nil,  var(_S35S461370, "#.1370") ) then return (function()
 return true
 end)() else return (function(_S35S461371  )
 if _caseS45pairS63(_S35S461370 ) then return (function(_S35S461366  )
 if _S61(scm_nil,  (_S35S461370)[2] ) then return (function()
 return true
 end)() else return var(_S35S461371, "#.1371")() end
 end)((_S35S461370)[1] ) else return var(_S35S461371, "#.1371")() end
 end)((function()
 return (function(_S35S461372  )
 if _caseS45pairS63(_S35S461370 ) then return (function(_S35S461366  )
 return (function(_S35S461375  )
 if _caseS45pairS63((_S35S461370)[2] ) then return (function(_S35S461367  )
 if _S61(scm_nil,  ((_S35S461370)[2])[2] ) then return (function()
 return var(_S35S461364, "#.1364")(var(_S35S461366, "#.1366"),  var(_S35S461367, "#.1367") )
 end)() else return var(_S35S461375, "#.1375")() end
 end)(((_S35S461370)[2])[1] ) else return var(_S35S461375, "#.1375")() end
 end)((function()
 return var(_S35S461372, "#.1372")()
 end) )
 end)((_S35S461370)[1] ) else return var(_S35S461372, "#.1372")() end
 end)((function()
 return (function(_S35S461373  )
 if _caseS45pairS63(_S35S461370 ) then return (function(_S35S461366  )
 return (function(_S35S461374  )
 if _caseS45pairS63((_S35S461370)[2] ) then return (function(_S35S461367  )
 return (function(_S35S461368  )
 return (function()
 if var(_S35S461364, "#.1364")(var(_S35S461366, "#.1366"),  var(_S35S461367, "#.1367") ) then return var(_apply, "apply")(var(_S35S461365, "#.1365"),  {_S35S461367, var(_S35S461368, "#.1368")} ) else return false end
 end)()
 end)(((_S35S461370)[2])[2] )
 end)(((_S35S461370)[2])[1] ) else return var(_S35S461374, "#.1374")() end
 end)((function()
 return var(_S35S461373, "#.1373")()
 end) )
 end)((_S35S461370)[1] ) else return var(_S35S461373, "#.1373")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461369, "#.1369"),  " in ",  {{scm_nil,{true,scm_nil}},{{{symbol('#.1366'),scm_nil},{true,scm_nil}},{{{symbol('#.1366'),{symbol('#.1367'),scm_nil}},{{symbol('#.1364'),{symbol('#.1366'),{symbol('#.1367'),scm_nil}}},scm_nil}},{{{symbol('#.1366'),{symbol('#.1367'),symbol('#.1368')}},{{symbol('and'),{{symbol('#.1364'),{symbol('#.1366'),{symbol('#.1367'),scm_nil}}},{{symbol('apply'),{symbol('#.1365'),{{symbol('cons'),{symbol('#.1367'),{symbol('#.1368'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461369, "#.1369") )
 end });
 return var(_S35S461365, "#.1365")
 end)();
 ignore(_stringS62S63)_stringS62S61S63 = (function()
 local _S35S461388;
 _S35S461388 = (function(_S35S461390 ,  _S35S461391  )
 (function()
 (function(_S35S461400)
 if _S35S461400 then
 return var(_error, "error")("In function ",  {symbol('quote'),{symbol('string>=?'),scm_nil}},  ": the argument ",  symbol('#.1390'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_S35S461390, "#.1390") ) ));
 if _not(_stringS63(var(_S35S461391, "#.1391") ) ) then return _error("In function ",  {symbol('quote'),{symbol('string>=?'),scm_nil}},  ": the argument ",  symbol('#.1391'),  "was expected to be a ",  symbol('string?') ) else return false end
 end)();
 return _callS47native(symbol('load'),  "local x, y = ...; return x >= y" )(var(_S35S461390, "#.1390"),  var(_S35S461391, "#.1391") )
 end);
 local _S35S461389;
 _S35S461389 = setmetatable({args=symbol('#.1393'),doc="Return #t if the sequence of strings given by the arguments is\
   monotonically nonincreasing."}, { __call = function(_S35S461402 ,  ...)
 local _S35S461393 = list(...);
 return (function(_S35S461394  )
 if _S61(scm_nil,  var(_S35S461394, "#.1394") ) then return (function()
 return true
 end)() else return (function(_S35S461395  )
 if _caseS45pairS63(_S35S461394 ) then return (function(_S35S461390  )
 if _S61(scm_nil,  (_S35S461394)[2] ) then return (function()
 return true
 end)() else return var(_S35S461395, "#.1395")() end
 end)((_S35S461394)[1] ) else return var(_S35S461395, "#.1395")() end
 end)((function()
 return (function(_S35S461396  )
 if _caseS45pairS63(_S35S461394 ) then return (function(_S35S461390  )
 return (function(_S35S461399  )
 if _caseS45pairS63((_S35S461394)[2] ) then return (function(_S35S461391  )
 if _S61(scm_nil,  ((_S35S461394)[2])[2] ) then return (function()
 return var(_S35S461388, "#.1388")(var(_S35S461390, "#.1390"),  var(_S35S461391, "#.1391") )
 end)() else return var(_S35S461399, "#.1399")() end
 end)(((_S35S461394)[2])[1] ) else return var(_S35S461399, "#.1399")() end
 end)((function()
 return var(_S35S461396, "#.1396")()
 end) )
 end)((_S35S461394)[1] ) else return var(_S35S461396, "#.1396")() end
 end)((function()
 return (function(_S35S461397  )
 if _caseS45pairS63(_S35S461394 ) then return (function(_S35S461390  )
 return (function(_S35S461398  )
 if _caseS45pairS63((_S35S461394)[2] ) then return (function(_S35S461391  )
 return (function(_S35S461392  )
 return (function()
 if var(_S35S461388, "#.1388")(var(_S35S461390, "#.1390"),  var(_S35S461391, "#.1391") ) then return var(_apply, "apply")(var(_S35S461389, "#.1389"),  {_S35S461391, var(_S35S461392, "#.1392")} ) else return false end
 end)()
 end)(((_S35S461394)[2])[2] )
 end)(((_S35S461394)[2])[1] ) else return var(_S35S461398, "#.1398")() end
 end)((function()
 return var(_S35S461397, "#.1397")()
 end) )
 end)((_S35S461394)[1] ) else return var(_S35S461397, "#.1397")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461393, "#.1393"),  " in ",  {{scm_nil,{true,scm_nil}},{{{symbol('#.1390'),scm_nil},{true,scm_nil}},{{{symbol('#.1390'),{symbol('#.1391'),scm_nil}},{{symbol('#.1388'),{symbol('#.1390'),{symbol('#.1391'),scm_nil}}},scm_nil}},{{{symbol('#.1390'),{symbol('#.1391'),symbol('#.1392')}},{{symbol('and'),{{symbol('#.1388'),{symbol('#.1390'),{symbol('#.1391'),scm_nil}}},{{symbol('apply'),{symbol('#.1389'),{{symbol('cons'),{symbol('#.1391'),{symbol('#.1392'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461393, "#.1393") )
 end });
 return var(_S35S461389, "#.1389")
 end)();
 ignore(_stringS62S61S63)_stringS61S63 = (function()
 local _S35S461412;
 _S35S461412 = (function(_S35S461414 ,  _S35S461415  )
 (function()
 (function(_S35S461424)
 if _S35S461424 then
 return var(_error, "error")("In function ",  {symbol('quote'),{symbol('string=?'),scm_nil}},  ": the argument ",  symbol('#.1414'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_S35S461414, "#.1414") ) ));
 if _not(_stringS63(var(_S35S461415, "#.1415") ) ) then return _error("In function ",  {symbol('quote'),{symbol('string=?'),scm_nil}},  ": the argument ",  symbol('#.1415'),  "was expected to be a ",  symbol('string?') ) else return false end
 end)();
 return _callS47native(symbol('load'),  "local x, y = ...; return x == y" )(var(_S35S461414, "#.1414"),  var(_S35S461415, "#.1415") )
 end);
 local _S35S461413;
 _S35S461413 = setmetatable({args=symbol('#.1417'),doc="Return #t if all the strings given are equal."}, { __call = function(_S35S461426 ,  ...)
 local _S35S461417 = list(...);
 return (function(_S35S461418  )
 if _S61(scm_nil,  var(_S35S461418, "#.1418") ) then return (function()
 return true
 end)() else return (function(_S35S461419  )
 if _caseS45pairS63(_S35S461418 ) then return (function(_S35S461414  )
 if _S61(scm_nil,  (_S35S461418)[2] ) then return (function()
 return true
 end)() else return var(_S35S461419, "#.1419")() end
 end)((_S35S461418)[1] ) else return var(_S35S461419, "#.1419")() end
 end)((function()
 return (function(_S35S461420  )
 if _caseS45pairS63(_S35S461418 ) then return (function(_S35S461414  )
 return (function(_S35S461423  )
 if _caseS45pairS63((_S35S461418)[2] ) then return (function(_S35S461415  )
 if _S61(scm_nil,  ((_S35S461418)[2])[2] ) then return (function()
 return var(_S35S461412, "#.1412")(var(_S35S461414, "#.1414"),  var(_S35S461415, "#.1415") )
 end)() else return var(_S35S461423, "#.1423")() end
 end)(((_S35S461418)[2])[1] ) else return var(_S35S461423, "#.1423")() end
 end)((function()
 return var(_S35S461420, "#.1420")()
 end) )
 end)((_S35S461418)[1] ) else return var(_S35S461420, "#.1420")() end
 end)((function()
 return (function(_S35S461421  )
 if _caseS45pairS63(_S35S461418 ) then return (function(_S35S461414  )
 return (function(_S35S461422  )
 if _caseS45pairS63((_S35S461418)[2] ) then return (function(_S35S461415  )
 return (function(_S35S461416  )
 return (function()
 if var(_S35S461412, "#.1412")(var(_S35S461414, "#.1414"),  var(_S35S461415, "#.1415") ) then return var(_apply, "apply")(var(_S35S461413, "#.1413"),  {_S35S461415, var(_S35S461416, "#.1416")} ) else return false end
 end)()
 end)(((_S35S461418)[2])[2] )
 end)(((_S35S461418)[2])[1] ) else return var(_S35S461422, "#.1422")() end
 end)((function()
 return var(_S35S461421, "#.1421")()
 end) )
 end)((_S35S461418)[1] ) else return var(_S35S461421, "#.1421")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461417, "#.1417"),  " in ",  {{scm_nil,{true,scm_nil}},{{{symbol('#.1414'),scm_nil},{true,scm_nil}},{{{symbol('#.1414'),{symbol('#.1415'),scm_nil}},{{symbol('#.1412'),{symbol('#.1414'),{symbol('#.1415'),scm_nil}}},scm_nil}},{{{symbol('#.1414'),{symbol('#.1415'),symbol('#.1416')}},{{symbol('and'),{{symbol('#.1412'),{symbol('#.1414'),{symbol('#.1415'),scm_nil}}},{{symbol('apply'),{symbol('#.1413'),{{symbol('cons'),{symbol('#.1415'),{symbol('#.1416'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461417, "#.1417") )
 end });
 return var(_S35S461413, "#.1413")
 end)();
 ignore(_stringS61S63)_stringS45append = (function()
 local _append2;
 _append2 = _callS47native(symbol('load'),  "local x, y = ...; return x .. y" );
 return setmetatable({args=symbol('#.1436'),doc="Concatenate any number of strings."}, { __call = function(_S35S461443 ,  ...)
 local _S35S461436 = list(...);
 return (function(_S35S461437  )
 if _S61(scm_nil,  var(_S35S461437, "#.1437") ) then return (function()
 return ""
 end)() else return (function(_S35S461438  )
 if _caseS45pairS63(_S35S461437 ) then return (function(_x  )
 if _S61(scm_nil,  (_S35S461437)[2] ) then return (function()
 return var(_x, "x")
 end)() else return var(_S35S461438, "#.1438")() end
 end)((_S35S461437)[1] ) else return var(_S35S461438, "#.1438")() end
 end)((function()
 return (function(_S35S461439  )
 if _caseS45pairS63(_S35S461437 ) then return (function(_x  )
 return (function(_S35S461442  )
 if _caseS45pairS63((_S35S461437)[2] ) then return (function(_y  )
 if _S61(scm_nil,  ((_S35S461437)[2])[2] ) then return (function()
 return var(_append2, "append2")(var(_x, "x"),  var(_y, "y") )
 end)() else return var(_S35S461442, "#.1442")() end
 end)(((_S35S461437)[2])[1] ) else return var(_S35S461442, "#.1442")() end
 end)((function()
 return var(_S35S461439, "#.1439")()
 end) )
 end)((_S35S461437)[1] ) else return var(_S35S461439, "#.1439")() end
 end)((function()
 return (function(_S35S461440  )
 if _caseS45pairS63(_S35S461437 ) then return (function(_x  )
 return (function(_S35S461441  )
 if _caseS45pairS63((_S35S461437)[2] ) then return (function(_y  )
 return (function(_z  )
 return (function()
 return var(_apply, "apply")(_stringS45append,  {var(_append2, "append2")(var(_x, "x"),  var(_y, "y") ), var(_z, "z")} )
 end)()
 end)(((_S35S461437)[2])[2] )
 end)(((_S35S461437)[2])[1] ) else return var(_S35S461441, "#.1441")() end
 end)((function()
 return var(_S35S461440, "#.1440")()
 end) )
 end)((_S35S461437)[1] ) else return var(_S35S461440, "#.1440")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461436, "#.1436"),  " in ",  {{scm_nil,{"",scm_nil}},{{{symbol('x'),scm_nil},{symbol('x'),scm_nil}},{{{symbol('x'),{symbol('y'),scm_nil}},{{symbol('append2'),{symbol('x'),{symbol('y'),scm_nil}}},scm_nil}},{{{symbol('x'),{symbol('y'),symbol('z')}},{{symbol('apply'),{symbol('string-append'),{{symbol('cons'),{{symbol('append2'),{symbol('x'),{symbol('y'),scm_nil}}},{symbol('z'),scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461436, "#.1436") )
 end })
 end)();
 ignore(_stringS45append)_stringS45length = (function(_s  )
 (function(_S35S461452)
 if _S35S461452 then
 return var(_error, "error")("In function ",  symbol('string-ref'),  ": the argument ",  symbol('s'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_s, "s") ) ));
 return _callS47native(symbol('load'),  "return #(...)" )(_s )
 end);
 ignore(_stringS45length)_stringS45ref = setmetatable({args={symbol('s'),{symbol('i'),scm_nil}},doc="Return the ,i-th byte in the string ,s."}, { __call = function(_S35S461453 ,  _s ,  _i  )
 (function(_S35S461454)
 if _S35S461454 then
 return var(_error, "error")("In function ",  symbol('string-ref'),  ": the argument ",  symbol('s'),  "was expected to be a ",  symbol('string?') ) else
 return false
 end
 end)(_not(var(_stringS63, "string?")(var(_s, "s") ) ));
 (function(_S35S461455)
 if _S35S461455 then
 return _error("In function ",  symbol('string-ref'),  ": the argument ",  symbol('i'),  "was expected to be a ",  symbol('integer?') ) else
 return false
 end
 end)(_not(_integerS63(var(_i, "i") ) ));
 return _callS47native(symbol('load'),  "local s, i = ...; return s:sub(i, i)" )(_s,  _i )
 end });
 ignore(_stringS45ref)_exp = setmetatable({args={symbol('c'),scm_nil},doc="Return e^c."}, { __call = function(_S35S461456 ,  _c  )
 return _callS47native({symbol('math'),{symbol('exp'),scm_nil}},  _exactS45S62inexact(var(_c, "c") ) )
 end });
 ignore(_exp)_log = setmetatable({args={symbol('c'),scm_nil},doc="Return ln(c)."}, { __call = function(_S35S461457 ,  _c  )
 return _callS47native({symbol('math'),{symbol('log'),scm_nil}},  _exactS45S62inexact(var(_c, "c") ) )
 end });
 ignore(_log)_sin = setmetatable({args={symbol('c'),scm_nil},doc="Return sin(c)."}, { __call = function(_S35S461458 ,  _c  )
 return _callS47native({symbol('math'),{symbol('sin'),scm_nil}},  _exactS45S62inexact(var(_c, "c") ) )
 end });
 ignore(_sin)_cos = setmetatable({args={symbol('c'),scm_nil},doc="Return cos(c)."}, { __call = function(_S35S461459 ,  _c  )
 return _callS47native({symbol('math'),{symbol('cos'),scm_nil}},  _exactS45S62inexact(var(_c, "c") ) )
 end });
 ignore(_cos)_tan = setmetatable({args={symbol('c'),scm_nil},doc="Return tan(c)."}, { __call = function(_S35S461460 ,  _c  )
 return _callS47native({symbol('math'),{symbol('tan'),scm_nil}},  _exactS45S62inexact(var(_c, "c") ) )
 end });
 ignore(_tan)_expt = setmetatable({args={symbol('z1'),{symbol('z2'),scm_nil}},doc="Return z1^z2. More specifically, return e^z2 ln(z1)."}, { __call = function(_S35S461461 ,  _z1 ,  _z2  )
 return _exp(var(_S42, "*")(var(_z2, "z2"),  _log(var(_z1, "z1") ) ) )
 end });
 ignore(_expt)_abs = setmetatable({args={symbol('x'),scm_nil},doc="Return the absolute value of ,x."}, { __call = function(_S35S461462 ,  _x  )
 if var(_S60, "<")(var(_x, "x"),  0 ) then return var(_S45, "-")(_x ) else return _x end
 end });
 ignore(_abs)_gcd = (function()
 local _gcd2;
 _gcd2 = (function(_a ,  _b  )
 if _S61(var(_b, "b"),  0 ) then return var(_a, "a") else return var(_gcd2, "gcd2")(_b,  _remainder(_a,  _b ) ) end
 end);
 return setmetatable({args=symbol('#.1464'),doc="Return the greatest common divisor of the given numbers."}, { __call = function(_S35S461472 ,  ...)
 local _S35S461464 = list(...);
 return (function(_S35S461465  )
 if _S61(scm_nil,  var(_S35S461465, "#.1465") ) then return (function()
 return 0
 end)() else return (function(_S35S461466  )
 if _caseS45pairS63(_S35S461465 ) then return (function(_x  )
 if _S61(scm_nil,  (_S35S461465)[2] ) then return (function()
 return var(_x, "x")
 end)() else return var(_S35S461466, "#.1466")() end
 end)((_S35S461465)[1] ) else return var(_S35S461466, "#.1466")() end
 end)((function()
 return (function(_S35S461467  )
 if _caseS45pairS63(_S35S461465 ) then return (function(_x  )
 return (function(_S35S461470  )
 if _caseS45pairS63((_S35S461465)[2] ) then return (function(_y  )
 if _S61(scm_nil,  ((_S35S461465)[2])[2] ) then return (function()
 return var(_gcd2, "gcd2")(var(_x, "x"),  var(_y, "y") )
 end)() else return var(_S35S461470, "#.1470")() end
 end)(((_S35S461465)[2])[1] ) else return var(_S35S461470, "#.1470")() end
 end)((function()
 return var(_S35S461467, "#.1467")()
 end) )
 end)((_S35S461465)[1] ) else return var(_S35S461467, "#.1467")() end
 end)((function()
 return (function(_S35S461468  )
 if _caseS45pairS63(_S35S461465 ) then return (function(_x  )
 return (function(_S35S461469  )
 if _caseS45pairS63((_S35S461465)[2] ) then return (function(_y  )
 return (function(_z  )
 return (function()
 return var(_gcd2, "gcd2")(var(_x, "x"),  var(_apply, "apply")(_gcd,  {var(_y, "y"), var(_z, "z")} ) )
 end)()
 end)(((_S35S461465)[2])[2] )
 end)(((_S35S461465)[2])[1] ) else return var(_S35S461469, "#.1469")() end
 end)((function()
 return var(_S35S461468, "#.1468")()
 end) )
 end)((_S35S461465)[1] ) else return var(_S35S461468, "#.1468")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461464, "#.1464"),  " in ",  {{scm_nil,{0,scm_nil}},{{{symbol('x'),scm_nil},{symbol('x'),scm_nil}},{{{symbol('x'),{symbol('y'),scm_nil}},{{symbol('gcd2'),{symbol('x'),{symbol('y'),scm_nil}}},scm_nil}},{{{symbol('x'),{symbol('y'),symbol('z')}},{{symbol('gcd2'),{symbol('x'),{{symbol('apply'),{symbol('gcd'),{{symbol('cons'),{symbol('y'),{symbol('z'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461464, "#.1464") )
 end })
 end)();
 ignore(_gcd)_lcm = (function()
 local _lcm2;
 _lcm2 = (function(_a ,  _b  )
 if (function(_S35S461489)
 if _S35S461489 then
 return _S61(var(_b, "b"),  0 ) else
 return false
 end
 end)(_S61(var(_a, "a"),  0 )) then return 0 else return var(_S47, "/")(_abs(var(_S42, "*")(_a,  _b ) ),  _gcd(_a,  _b ) ) end
 end);
 return setmetatable({args=symbol('#.1481'),doc="Return the least common multiple of the given numbers."}, { __call = function(_S35S461490 ,  ...)
 local _S35S461481 = list(...);
 return (function(_S35S461482  )
 if _S61(scm_nil,  var(_S35S461482, "#.1482") ) then return (function()
 return 0
 end)() else return (function(_S35S461483  )
 if _caseS45pairS63(_S35S461482 ) then return (function(_x  )
 if _S61(scm_nil,  (_S35S461482)[2] ) then return (function()
 return var(_x, "x")
 end)() else return var(_S35S461483, "#.1483")() end
 end)((_S35S461482)[1] ) else return var(_S35S461483, "#.1483")() end
 end)((function()
 return (function(_S35S461484  )
 if _caseS45pairS63(_S35S461482 ) then return (function(_x  )
 return (function(_S35S461487  )
 if _caseS45pairS63((_S35S461482)[2] ) then return (function(_y  )
 if _S61(scm_nil,  ((_S35S461482)[2])[2] ) then return (function()
 return var(_lcm2, "lcm2")(var(_x, "x"),  var(_y, "y") )
 end)() else return var(_S35S461487, "#.1487")() end
 end)(((_S35S461482)[2])[1] ) else return var(_S35S461487, "#.1487")() end
 end)((function()
 return var(_S35S461484, "#.1484")()
 end) )
 end)((_S35S461482)[1] ) else return var(_S35S461484, "#.1484")() end
 end)((function()
 return (function(_S35S461485  )
 if _caseS45pairS63(_S35S461482 ) then return (function(_x  )
 return (function(_S35S461486  )
 if _caseS45pairS63((_S35S461482)[2] ) then return (function(_y  )
 return (function(_z  )
 return (function()
 return var(_lcm2, "lcm2")(var(_x, "x"),  var(_apply, "apply")(_lcm,  {var(_y, "y"), var(_z, "z")} ) )
 end)()
 end)(((_S35S461482)[2])[2] )
 end)(((_S35S461482)[2])[1] ) else return var(_S35S461486, "#.1486")() end
 end)((function()
 return var(_S35S461485, "#.1485")()
 end) )
 end)((_S35S461482)[1] ) else return var(_S35S461485, "#.1485")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461481, "#.1481"),  " in ",  {{scm_nil,{0,scm_nil}},{{{symbol('x'),scm_nil},{symbol('x'),scm_nil}},{{{symbol('x'),{symbol('y'),scm_nil}},{{symbol('lcm2'),{symbol('x'),{symbol('y'),scm_nil}}},scm_nil}},{{{symbol('x'),{symbol('y'),symbol('z')}},{{symbol('lcm2'),{symbol('x'),{{symbol('apply'),{symbol('lcm'),{{symbol('cons'),{symbol('y'),{symbol('z'),scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}}}} )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461481, "#.1481") )
 end })
 end)();
 ignore(_lcm)_newline = setmetatable({args=scm_nil,doc="Produces an end-of-line character on the standard output port."}, { __call = function(_S35S461499  )
 return var(_display, "display")("\
" )
 end });
 ignore(_newline)_eqvS63 = (function()
 local _referenceS61S63;
 _referenceS61S63 = (function(_x ,  _y  )
 return _callS47native(symbol('load'),  "local x, y = ...; return x == y" )(var(_x, "x"),  var(_y, "y") )
 end);
 return setmetatable({args={symbol('x'),{symbol('y'),scm_nil}},doc="Return #t if ,x and ,y are equivalent in the sense of the R7RS\
      predicate of the same name."}, { __call = function(_S35S461500 ,  _x ,  _y  )
 if var(_nullS63, "null?")(var(_x, "x") ) then return (function()
 return _nullS63(var(_y, "y") )
 end)() else if var(_pairS63, "pair?")(_x ) then return (function()
 return var(_referenceS61S63, "reference=?")(_x,  var(_y, "y") )
 end)() else if var(_procedureS63, "procedure?")(_x ) then return (function()
 return var(_referenceS61S63, "reference=?")(_x,  var(_y, "y") )
 end)() else if _else then return (function()
 return _S61(_x,  var(_y, "y") )
 end)() else return false end end end end
 end })
 end)();
 ignore(_eqvS63)_equalS63 = _eqS63;
 ignore(_equalS63)_square = setmetatable({args={symbol('x'),scm_nil},doc="Returns the square of ,x. This is equivalent to ('* ,x ,x)."}, { __call = function(_S35S461505 ,  _x  )
 return var(_S42, "*")(var(_x, "x"),  _x )
 end });
 ignore(_square)_sqrt = (function(_z  )
 if var(_S60, "<")(var(_z, "z"),  0 ) then return (function()
 return var(_error, "error")("Scheme 51 does not support complex numbers. negative sqrt: ",  _z )
 end)() else if _S61(_z,  0 ) then return (function()
 return 0
 end)() else if _else then return (function()
 return _expt(_z,  0.5 )
 end)() else return false end end end
 end);
 ignore(_sqrt)_makeS45list = setmetatable({args={symbol('k'),symbol('init')},doc="Return a newly-allocated list of size ,k. If the initial value\
   ,init is not given, it is taken to be #f."}, { __call = function(_S35S461509 ,  _k ,  ...)
 local _init = list(...);
 local _init;
 _init = (function(_S35S461510)
 if _S35S461510 then
 return false else
 return (var(_init, "init"))[1]
 end
 end)(var(_nullS63, "null?")(_init ));
 return (function()
 local _loop;
 _loop = (function(_x  )
 if var(_S62S61, ">=")(var(_x, "x"),  var(_k, "k") ) then return scm_nil else return {_init, var(_loop, "loop")(var(_S43, "+")(1,  _x ) )} end
 end);
 return var(_loop, "loop")(0 )
 end)()
 end });
 ignore(_makeS45list)_booleanS63 = setmetatable({args={symbol('b'),scm_nil},doc="Return #t if ,b is a boolean."}, { __call = function(_S35S461515 ,  _b  )
 return (function(_S35S461512  )
 if var(_S35S461512, "#.1512") then return _S35S461512 else return (function(_S35S461513  )
 if var(_S35S461513, "#.1513") then return _S35S461513 else return false end
 end)(_S61(var(_b, "b"),  false ) ) end
 end)(_S61(var(_b, "b"),  true ) )
 end });
 ignore(_booleanS63)_phase = _makeS45parameter(symbol('compiling') );
 ignore(_phase)_compilerS45format = (function(...)
 local _args = list(...);
 return var(_apply, "apply")(_callS47native(symbol('load'),  "return string.format" )(),  var(_args, "args") )
 end);
 ignore(_compilerS45format)_escapeS45symbol = (function(_e  )
 (function(_S35S461518)
 if _S35S461518 then
 return var(_error, "error")("not a symbol",  var(_e, "e") ) else
 return false
 end
 end)(_not(var(_symbolS63, "symbol?")(_e ) ));
 return _compilerS45format("_%s",  _callS47native({symbol('string'),{symbol('gsub'),scm_nil}},  (_e)[1],  "[^%w_]",  _callS47native(symbol('load'),  "return function(s) return 'S' .. string.byte(s) end" )() ) )
 end);
 ignore(_escapeS45symbol)_variablesS45inS45scope = _list(_makeS45hashS45table() );
 ignore(_variablesS45inS45scope)_removeS45variablesS45fromS45scope = (function(_x  )
 if var(_pairS63, "pair?")(var(_x, "x") ) then return (function()
 _removeS45variablesS45fromS45scope((_x)[1] );
 return _removeS45variablesS45fromS45scope((_x)[2] )
 end)() else if var(_symbolS63, "symbol?")(_x ) then return (function()
 return _hashS45setS33((_variablesS45inS45scope)[1],  _escapeS45symbol(_x ),  false )
 end)() else return false end end
 end);
 ignore(_removeS45variablesS45fromS45scope)_enterS45scope = (function(_vars  )
 (function()
 _variablesS45inS45scope = scm_set_helper(_variablesS45inS45scope, {_copyS45hashS45table((_variablesS45inS45scope)[1] ), _variablesS45inS45scope}, "variables-in-scope");
 return _variablesS45inS45scope
 end)();
 return _removeS45variablesS45fromS45scope(var(_vars, "vars") )
 end);
 ignore(_enterS45scope)_leaveS45scope = (function()
 return (function()
 _variablesS45inS45scope = scm_set_helper(_variablesS45inS45scope, (_variablesS45inS45scope)[2], "variables-in-scope");
 return _variablesS45inS45scope
 end)()
 end);
 ignore(_leaveS45scope)ignore(_pushS45macroS33(symbol('in-scope'),  (function(_S35S461521  )
 return var(_apply, "apply")((function(_syms ,  ...)
 local _body = list(...);
 local _s;
 _s = var(_gensym, "gensym")();
 return {symbol('begin'), {{symbol('enter-scope'), {var(_syms, "syms"), scm_nil}}, {{symbol('let'), {{{var(_s, "s"), {{symbol('begin'), var(_body, "body")}, scm_nil}}, scm_nil}, {{symbol('leave-scope'), scm_nil}, {_s, scm_nil}}}}, scm_nil}}}
 end),  var(_S35S461521, "#.1521") )
 end) ))_warn = (function(...)
 local _rest = list(...);
 return var(_apply, "apply")(var(_display, "display"),  {"-- Warning: ", _append(var(_rest, "rest"),  {"\
", scm_nil} )} )
 end);
 ignore(_warn)ignore(_pushS45macroS33(symbol('warn-redefinition'),  (function(_S35S461522  )
 return var(_apply, "apply")((function(_name  )
 return {symbol('warn'), {"redefinition of built-in function ", {var(_name, "name"), {" will not take effect for saturated applications", scm_nil}}}}
 end),  var(_S35S461522, "#.1522") )
 end) ))_compileS45simpleS45expression = (function(_return ,  _e  )
 if var(_rationalS63, "rational?")(var(_e, "e") ) then return (function()
 return var(_return, "return")(_compilerS45format("rational(%d, %d)",  (_e)[1],  (_e)[2] ) )
 end)() else if var(_numberS63, "number?")(_e ) then return (function()
 return var(_return, "return")(_callS47native(symbol('tostring'),  _e ) )
 end)() else if var(_stringS63, "string?")(_e ) then return (function()
 return var(_return, "return")(_compilerS45format("%q",  _e ) )
 end)() else if var(_keywordS63, "keyword?")(_e ) then return (function()
 return var(_error, "error")("use of keyword in expression position: ",  _e )
 end)() else if var(_symbolS63, "symbol?")(_e ) then return (function()
 if var(_hashS45ref, "hash-ref")((_variablesS45inS45scope)[1],  _escapeS45symbol(_e ) ) then return var(_return, "return")(_escapeS45symbol(_e ) ) else return (function()
 _hashS45setS33((_variablesS45inS45scope)[1],  _escapeS45symbol(_e ),  true );
 return _return(_compilerS45format("var(%s, %q)",  _escapeS45symbol(_e ),  (_e)[1] ) )
 end)() end
 end)() else if _S61(_e,  true ) then return (function()
 return var(_return, "return")("true" )
 end)() else if _S61(_e,  false ) then return (function()
 return var(_return, "return")("false" )
 end)() else if _S61(_e,  scm_eof ) then return (function()
 return var(_return, "return")("scm_eof" )
 end)() else if var(_nullS63, "null?")(_e ) then return (function()
 return var(_return, "return")("scm_nil" )
 end)() else if (function(_S35S461534)
 if _S35S461534 then
 return _S61(symbol('quote'),  (_e)[1] ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_e )) then return (function()
 return var(_compileS45quote, "compile-quote")(var(_return, "return"),  _cadr(_e ) )
 end)() else if _else then return (function()
 return var(_error, "error")("not a simple expression",  _e )
 end)() else return false end end end end end end end end end end end
 end);
 ignore(_compileS45simpleS45expression)_compileS45quote = (function(_return ,  _e  )
 if var(_keywordS63, "keyword?")(var(_e, "e") ) then return (function()
 return var(_return, "return")(_compilerS45format("keyword('%s')",  (_e)[1] ) )
 end)() else if var(_symbolS63, "symbol?")(_e ) then return (function()
 return var(_return, "return")(_compilerS45format("symbol('%s')",  (_e)[1] ) )
 end)() else if var(_pairS63, "pair?")(_e ) then return (function()
 return var(_return, "return")(_compilerS45format("{%s,%s}",  _compileS45quote((function(_x  )
 return var(_x, "x")
 end),  (_e)[1] ),  _compileS45quote((function(_x  )
 return var(_x, "x")
 end),  (_e)[2] ) ) )
 end)() else if _else then return (function()
 return _compileS45simpleS45expression(var(_return, "return"),  _e )
 end)() else return false end end end end
 end);
 ignore(_compileS45quote)_listS63 = (function(_x  )
 if var(_nullS63, "null?")(var(_x, "x") ) then return true else if var(_pairS63, "pair?")(_x ) then return _listS63((_x)[2] ) else return false end end
 end);
 ignore(_listS63)_simpleS45argumentS45list = (function(_empty ,  _args  )
 if var(_nullS63, "null?")(var(_args, "args") ) then return var(_empty, "empty") else if var(_symbolS63, "symbol?")((_args)[1] ) then return _compilerS45format("%s %s %s",  _escapeS45symbol((_args)[1] ),  (function(_S35S461544)
 if _S35S461544 then
 return "" else
 return ", "
 end
 end)((function(_S35S461545)
 if _S35S461545 then
 return _eqS63(_empty,  "" ) else
 return false
 end
 end)(_nullS63((_args)[2] ))),  _simpleS45argumentS45list(_empty,  (_args)[2] ) ) else return var(_error, "error")("non-symbol in simple-argument-list ",  _args ) end end
 end);
 ignore(_simpleS45argumentS45list)_complexS45argumentS45list = (function(_args  )
 if var(_symbolS63, "symbol?")(var(_args, "args") ) then return (function()
 return {scm_nil, _args}
 end)() else if var(_pairS63, "pair?")(_args ) then return (function()
 return (function(_r  )
 return {{(_args)[1], (var(_r, "r"))[1]}, (_r)[2]}
 end)(_complexS45argumentS45list((_args)[2] ) )
 end)() else return false end end
 end);
 ignore(_complexS45argumentS45list)_compileS45lambda = (function(_return ,  _args ,  _body  )
 return (function()
 _enterS45scope(var(_args, "args") );
 return (function(_S35S461548  )
 _leaveS45scope();
 return var(_S35S461548, "#.1548")
 end)((function()
 if _listS63(_args ) then return (function()
 return var(_return, "return")(_compilerS45format("(%s)\
 %s\
 end",  _simpleS45argumentS45list("",  _args ),  var(_compileS45body, "compile-body")((function(_x  )
 return _compilerS45format("return %s",  var(_x, "x") )
 end),  var(_body, "body") ) ) )
 end)() else if _else then return (function()
 return (function(_argsS45andS45rest  )
 return (function(_proper ,  _rest  )
 _hashS45setS33((_variablesS45inS45scope)[1],  (var(_rest, "rest"))[1],  true );
 return var(_return, "return")(_compilerS45format("(%s)\
 local %s = list(...);\
 %s\
 end",  _simpleS45argumentS45list("...",  var(_proper, "proper") ),  _escapeS45symbol(_rest ),  var(_compileS45body, "compile-body")((function(_x  )
 return _compilerS45format("return %s",  var(_x, "x") )
 end),  var(_body, "body") ) ) )
 end)((var(_argsS45andS45rest, "args-and-rest"))[1],  (_argsS45andS45rest)[2] )
 end)(_complexS45argumentS45list(_args ) )
 end)() else return false end end
 end)() )
 end)()
 end);
 ignore(_compileS45lambda)_atomicS63 = (function(_p  )
 return (function(_S35S461551  )
 if var(_S35S461551, "#.1551") then return _S35S461551 else return (function(_S35S461552  )
 if var(_S35S461552, "#.1552") then return _S35S461552 else return (function(_S35S461553  )
 if var(_S35S461553, "#.1553") then return _S35S461553 else return (function(_S35S461554  )
 if var(_S35S461554, "#.1554") then return _S35S461554 else return (function(_S35S461555  )
 if var(_S35S461555, "#.1555") then return _S35S461555 else return false end
 end)((function(_S35S461565)
 if _S35S461565 then
 return (function(_S35S461557  )
 if var(_S35S461557, "#.1557") then return _S35S461557 else return (function(_S35S461558  )
 if var(_S35S461558, "#.1558") then return _S35S461558 else return false end
 end)(_S61((var(_p, "p"))[1],  symbol('quote') ) ) end
 end)(_S61((var(_p, "p"))[1],  symbol('lambda') ) ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_p )) ) end
 end)(_S61(var(_p, "p"),  false ) ) end
 end)(_S61(var(_p, "p"),  true ) ) end
 end)(var(_stringS63, "string?")(var(_p, "p") ) ) end
 end)(var(_numberS63, "number?")(var(_p, "p") ) )
 end);
 ignore(_atomicS63)_defineS63 = (function(_e  )
 if var(_pairS63, "pair?")(var(_e, "e") ) then return _S61((_e)[1],  symbol('define') ) else return false end
 end);
 ignore(_defineS63)_compileS45body = (function(_return ,  _b  )
 if _not(var(_pairS63, "pair?")(var(_b, "b") ) ) then return (function()
 return var(_compileS45expr, "compile-expr")(var(_return, "return"),  _b )
 end)() else if var(_nullS63, "null?")(_b ) then return (function()
 return var(_return, "return")("false" )
 end)() else if (function(_S35S461572)
 if _S35S461572 then
 return _nullS63((_b)[2] ) else
 return false
 end
 end)(_pairS63(_b )) then return (function()
 return var(_compileS45expr, "compile-expr")(var(_return, "return"),  (_b)[1],  true )
 end)() else if _atomicS63((_b)[1] ) then return (function()
 return _compileS45body(var(_return, "return"),  (_b)[2] )
 end)() else if _defineS63((_b)[1] ) then return (function()
 return _compilerS45format("%s;\
 %s",  var(_apply, "apply")(var(_compileS45bodyS45define, "compile-body-define"),  _cdar(_b ) ),  _compileS45body(var(_return, "return"),  (_b)[2] ) )
 end)() else if _else then return (function()
 return _compilerS45format("%s;\
 %s",  var(_compileS45expr, "compile-expr")((function(_x  )
 return var(_x, "x")
 end),  (_b)[1] ),  _compileS45body(var(_return, "return"),  (_b)[2] ) )
 end)() else return false end end end end end end
 end);
 ignore(_compileS45body)_compileS45bodyS45define = (function(_name ,  ...)
 local _body = list(...);
 if var(_pairS63, "pair?")(var(_name, "name") ) then return _compileS45bodyS45define((_name)[1],  {symbol('lambda'), {(_name)[2], var(_body, "body")}} ) else return var(_compileS45expr, "compile-expr")((function(_x  )
 return _compilerS45format("local %s;\
 %s = %s",  _escapeS45symbol(_name ),  _escapeS45symbol(_name ),  var(_x, "x") )
 end),  (_body)[1] ) end
 end);
 ignore(_compileS45bodyS45define)_compileS45args = (function(_alist  )
 if var(_nullS63, "null?")(var(_alist, "alist") ) then return "" else if _defineS63((_alist)[1] ) then return var(_error, "error")("illegal define expression",  (_alist)[1],  "in argument list" ) else return _compilerS45format("%s%s %s",  var(_compileS45expr, "compile-expr")((function(_x  )
 return var(_x, "x")
 end),  (_alist)[1] ),  (function(_S35S461579)
 if _S35S461579 then
 return "" else
 return ", "
 end
 end)(_nullS63((_alist)[2] )),  _compileS45args((_alist)[2] ) ) end end
 end);
 ignore(_compileS45args)ignore(_pushS45macroS33(symbol('warn-extra-arguments'),  (function(_S35S461580  )
 return var(_apply, "apply")((function(_rest ,  _expr  )
 local _temp;
 _temp = var(_gensym, "gensym")();
 return {symbol('if'), {{symbol('not'), {{symbol('null?'), {var(_rest, "rest"), scm_nil}}, scm_nil}}, {{symbol('let'), {{{var(_name, "name"), {var(_expr, "expr"), scm_nil}}, scm_nil}, {{symbol('warn'), {"Potentially too many arguments for function", {{symbol('car'), {_name, scm_nil}}, {_name, scm_nil}}}}, scm_nil}}}, scm_nil}}}
 end),  var(_S35S461580, "#.1580") )
 end) ))_compilationS45rules = _makeS45hashS45table();
 ignore(_compilationS45rules)_builtinS45functionS63 = (function(_s  )
 return (function(_S35S461581  )
 if var(_S35S461581, "#.1581") then return _S35S461581 else return (function(_S35S461582  )
 if var(_S35S461582, "#.1582") then return _S35S461582 else return (function(_S35S461583  )
 if var(_S35S461583, "#.1583") then return _S35S461583 else return false end
 end)(_S61(var(_s, "s"),  symbol('cdr') ) ) end
 end)(_S61(var(_s, "s"),  symbol('car') ) ) end
 end)(_S61(var(_s, "s"),  symbol('cons') ) )
 end);
 ignore(_builtinS45functionS63)ignore(_pushS45macroS33(symbol('define-compilation-rule'),  (function(_S35S461588  )
 return var(_apply, "apply")((function(_name ,  ...)
 local _body = list(...);
 return {symbol('hash-set!'), {symbol('compilation-rules'), {_caar(var(_name, "name") ), {{symbol('cons'), {_length((_name)[2] ), {{symbol('lambda'), {(_name)[2], var(_body, "body")}}, scm_nil}}}, scm_nil}}}}
 end),  var(_S35S461588, "#.1588") )
 end) ))_compileS45builtinS45function = (function(_func ,  _args  )
 local _fallback;
 _fallback = (function()
 return _compilerS45format("%s(%s)",  var(_compileS45expr, "compile-expr")((function(_x  )
 return var(_x, "x")
 end),  var(_func, "func") ),  _compileS45args(var(_args, "args") ) )
 end);
 return (function(_rules  )
 if (function(_S35S461590)
 if _S35S461590 then
 return var(_procedureS63, "procedure?")((var(_rules, "rules"))[2] ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_rules )) then return (function()
 if _S61(_length(var(_args, "args") ),  (_rules)[1] ) then return var(_apply, "apply")((_rules)[2],  _args ) else return (function()
 _warn("Incorrect number of arguments to ",  var(_func, "func"),  " (expected ",  (_rules)[1],  " got, ",  _length(_args ),  ")" );
 return var(_fallback, "fallback")()
 end)() end
 end)() else if _else then return (function()
 return var(_fallback, "fallback")()
 end)() else return false end end
 end)(var(_hashS45ref, "hash-ref")(_compilationS45rules,  (var(_func, "func"))[1] ) )
 end);
 ignore(_compileS45builtinS45function)ignore(_hashS45setS33(_compilationS45rules,  "car",  {1, (function(_arg  )
 return var(_compileS45expr, "compile-expr")((function(_x  )
 return _compilerS45format("(%s)[1]",  var(_x, "x") )
 end),  var(_arg, "arg") )
 end)} ))ignore(_hashS45setS33(_compilationS45rules,  "cdr",  {1, (function(_arg  )
 return var(_compileS45expr, "compile-expr")((function(_x  )
 return _compilerS45format("(%s)[2]",  var(_x, "x") )
 end),  var(_arg, "arg") )
 end)} ))ignore(_hashS45setS33(_compilationS45rules,  "cons",  {2, (function(_head ,  _tail  )
 return _compilerS45format("{%s, %s}",  var(_compileS45expr, "compile-expr")((function(_x  )
 return var(_x, "x")
 end),  var(_head, "head") ),  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  var(_tail, "tail") ) )
 end)} ))_compileS45expr = (function(_return ,  _expr ,  ...)
 local _isS45tail = list(...);
 return (function(_S35S461593  )
 return (function(_S35S461594  )
 return (function(_e  )
 if _not(var(_pairS63, "pair?")(var(_e, "e") ) ) then return (function()
 return _compileS45simpleS45expression(var(_return, "return"),  _e )
 end)() else return var(_S35S461594, "#.1594")() end
 end)(var(_S35S461593, "#.1593") )
 end)((function()
 return (function(_S35S461595  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('quote'),  (_S35S461593)[1] ) then return (function(_S35S461625  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_e  )
 if _S61(scm_nil,  ((_S35S461593)[2])[2] ) then return (function()
 return _compileS45quote(var(_return, "return"),  var(_e, "e") )
 end)() else return var(_S35S461625, "#.1625")() end
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461625, "#.1625")() end
 end)((function()
 return var(_S35S461595, "#.1595")()
 end) ) else return var(_S35S461595, "#.1595")() end else return _S35S461595() end
 end)((function()
 return (function(_S35S461596  )
 return (function(_S35S461621  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('lambda'),  (_S35S461593)[1] ) then return (function(_S35S461622  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_args  )
 return (function(_S35S461623  )
 if _caseS45pairS63(((_S35S461593)[2])[2] ) then return (function(_doc  )
 return (function(_S35S461624  )
 if _caseS45pairS63((((_S35S461593)[2])[2])[2] ) then return (function(_body1  )
 return (function(_body  )
 if var(_stringS63, "string?")(var(_doc, "doc") ) then return (function()
 return (function(_ret  )
 return _compileS45lambda((function(_x  )
 return var(_ret, "ret")(_compilerS45format("setmetatable({args=%s,doc=%q}, { __call = function%s })",  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  {symbol('quote'), {var(_args, "args"), scm_nil}} ),  _doc,  var(_x, "x") ) )
 end),  {var(_gensym, "gensym")(), var(_args, "args")},  {var(_body1, "body1"), var(_body, "body")} )
 end)(var(_return, "return") )
 end)() else return var(_S35S461596, "#.1596")() end
 end)(((((_S35S461593)[2])[2])[2])[2] )
 end)(((((_S35S461593)[2])[2])[2])[1] ) else return var(_S35S461624, "#.1624")() end
 end)((function()
 return var(_S35S461623, "#.1623")()
 end) )
 end)((((_S35S461593)[2])[2])[1] ) else return var(_S35S461623, "#.1623")() end
 end)((function()
 return var(_S35S461622, "#.1622")()
 end) )
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461622, "#.1622")() end
 end)((function()
 return var(_S35S461621, "#.1621")()
 end) ) else return var(_S35S461621, "#.1621")() end else return _S35S461621() end
 end)((function()
 return var(_S35S461596, "#.1596")()
 end) )
 end)((function()
 return (function(_S35S461597  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('lambda'),  (_S35S461593)[1] ) then return (function(_S35S461620  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_args  )
 return (function(_body  )
 return (function()
 return (function(_ret  )
 return _compileS45lambda((function(_x  )
 return var(_ret, "ret")(_compilerS45format("(function%s)",  var(_x, "x") ) )
 end),  var(_args, "args"),  var(_body, "body") )
 end)(var(_return, "return") )
 end)()
 end)(((_S35S461593)[2])[2] )
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461620, "#.1620")() end
 end)((function()
 return var(_S35S461597, "#.1597")()
 end) ) else return var(_S35S461597, "#.1597")() end else return _S35S461597() end
 end)((function()
 return (function(_S35S461598  )
 return (function(_S35S461617  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('define'),  (_S35S461593)[1] ) then return (function(_S35S461618  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_name  )
 return (function(_S35S461619  )
 if _caseS45pairS63(((_S35S461593)[2])[2] ) then return (function(_value  )
 if _S61(scm_nil,  (((_S35S461593)[2])[2])[2] ) then if var(_symbolS63, "symbol?")(var(_name, "name") ) then return (function()
 _callS47native(symbol('rawset'),  (_variablesS45inS45scope)[1],  _escapeS45symbol(_name ),  true );
 return _compilerS45format("%s;\
 %s",  _compileS45expr((function(_v  )
 return _compilerS45format("%s = %s",  _escapeS45symbol(_name ),  var(_v, "v") )
 end),  var(_value, "value") ),  var(_return, "return")(_escapeS45symbol(_name ) ) )
 end)() else return var(_S35S461598, "#.1598")() end else return var(_S35S461619, "#.1619")() end
 end)((((_S35S461593)[2])[2])[1] ) else return var(_S35S461619, "#.1619")() end
 end)((function()
 return var(_S35S461618, "#.1618")()
 end) )
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461618, "#.1618")() end
 end)((function()
 return var(_S35S461617, "#.1617")()
 end) ) else return var(_S35S461617, "#.1617")() end else return _S35S461617() end
 end)((function()
 return var(_S35S461598, "#.1598")()
 end) )
 end)((function()
 return (function(_S35S461599  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('define'),  (_S35S461593)[1] ) then return (function(_S35S461615  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_S35S461616  )
 if _caseS45pairS63(((_S35S461593)[2])[1] ) then return (function(_name  )
 return (function(_args  )
 return (function(_body  )
 return (function()
 return _compileS45expr(var(_return, "return"),  {symbol('define'), {var(_name, "name"), {{symbol('lambda'), {var(_args, "args"), var(_body, "body")}}, scm_nil}}} )
 end)()
 end)(((_S35S461593)[2])[2] )
 end)((((_S35S461593)[2])[1])[2] )
 end)((((_S35S461593)[2])[1])[1] ) else return var(_S35S461616, "#.1616")() end
 end)((function()
 return var(_S35S461615, "#.1615")()
 end) ) else return var(_S35S461615, "#.1615")() end
 end)((function()
 return var(_S35S461599, "#.1599")()
 end) ) else return var(_S35S461599, "#.1599")() end else return _S35S461599() end
 end)((function()
 return (function(_S35S461600  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('set!'),  (_S35S461593)[1] ) then return (function(_S35S461613  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_name  )
 return (function(_S35S461614  )
 if _caseS45pairS63(((_S35S461593)[2])[2] ) then return (function(_expr  )
 if _S61(scm_nil,  (((_S35S461593)[2])[2])[2] ) then return (function()
 return (function(_ret  )
 return _compileS45expr((function(_v  )
 return var(_ret, "ret")(_compilerS45format("(function()\
 %s = scm_set_helper(%s, %s, %q);\
 return %s\
 end)()",  _escapeS45symbol(var(_name, "name") ),  _escapeS45symbol(_name ),  var(_v, "v"),  (_name)[1],  _escapeS45symbol(_name ) ) )
 end),  var(_expr, "expr") )
 end)(var(_return, "return") )
 end)() else return var(_S35S461614, "#.1614")() end
 end)((((_S35S461593)[2])[2])[1] ) else return var(_S35S461614, "#.1614")() end
 end)((function()
 return var(_S35S461613, "#.1613")()
 end) )
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461613, "#.1613")() end
 end)((function()
 return var(_S35S461600, "#.1600")()
 end) ) else return var(_S35S461600, "#.1600")() end else return _S35S461600() end
 end)((function()
 return (function(_S35S461601  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('if'),  (_S35S461593)[1] ) then return (function(_S35S461612  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_c  )
 if _S61(scm_nil,  ((_S35S461593)[2])[2] ) then return (function()
 return var(_error, "error")("If expression missing 'then' case: ",  {symbol('if'), {var(_c, "c"), scm_nil}} )
 end)() else return var(_S35S461612, "#.1612")() end
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461612, "#.1612")() end
 end)((function()
 return var(_S35S461601, "#.1601")()
 end) ) else return var(_S35S461601, "#.1601")() end else return _S35S461601() end
 end)((function()
 return (function(_S35S461602  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('if'),  (_S35S461593)[1] ) then return (function(_S35S461610  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_c  )
 return (function(_S35S461611  )
 if _caseS45pairS63(((_S35S461593)[2])[2] ) then return (function(_t  )
 if _S61(scm_nil,  (((_S35S461593)[2])[2])[2] ) then return (function()
 local _it;
 _it = _escapeS45symbol(var(_gensym, "gensym")() );
 if _S61((var(_isS45tail, "is-tail"))[1],  true ) then return _compilerS45format("if %s then %s else return false end",  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  var(_c, "c") ),  _compileS45expr(var(_return, "return"),  var(_t, "t"),  true ) ) else return _return(_compilerS45format("(function(%s)\
 if %s then\
 %s else\
 return false\
 end\
 end)(%s)",  var(_it, "it"),  _it,  _compileS45expr((function(_x  )
 return _compilerS45format("return %s",  var(_x, "x") )
 end),  _t ),  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  _c ) ) ) end
 end)() else return var(_S35S461611, "#.1611")() end
 end)((((_S35S461593)[2])[2])[1] ) else return var(_S35S461611, "#.1611")() end
 end)((function()
 return var(_S35S461610, "#.1610")()
 end) )
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461610, "#.1610")() end
 end)((function()
 return var(_S35S461602, "#.1602")()
 end) ) else return var(_S35S461602, "#.1602")() end else return _S35S461602() end
 end)((function()
 return (function(_S35S461603  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then if _S61(symbol('if'),  (_S35S461593)[1] ) then return (function(_S35S461607  )
 if _caseS45pairS63((_S35S461593)[2] ) then return (function(_c  )
 return (function(_S35S461608  )
 if _caseS45pairS63(((_S35S461593)[2])[2] ) then return (function(_t  )
 return (function(_S35S461609  )
 if _caseS45pairS63((((_S35S461593)[2])[2])[2] ) then return (function(_e  )
 if _S61(scm_nil,  ((((_S35S461593)[2])[2])[2])[2] ) then return (function()
 local _it;
 _it = _escapeS45symbol(var(_gensym, "gensym")() );
 if _S61((var(_isS45tail, "is-tail"))[1],  true ) then return _compilerS45format("if %s then %s else %s end",  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  var(_c, "c") ),  _compileS45expr(var(_return, "return"),  var(_t, "t"),  true ),  _compileS45expr(_return,  var(_e, "e"),  true ) ) else return _return(_compilerS45format("(function(%s)\
 if %s then\
 %s else\
 %s\
 end\
 end)(%s)",  var(_it, "it"),  _it,  _compileS45expr((function(_x  )
 return _compilerS45format("return %s",  var(_x, "x") )
 end),  _t ),  _compileS45expr((function(_x  )
 return _compilerS45format("return %s",  var(_x, "x") )
 end),  _e ),  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  _c ) ) ) end
 end)() else return var(_S35S461609, "#.1609")() end
 end)(((((_S35S461593)[2])[2])[2])[1] ) else return var(_S35S461609, "#.1609")() end
 end)((function()
 return var(_S35S461608, "#.1608")()
 end) )
 end)((((_S35S461593)[2])[2])[1] ) else return var(_S35S461608, "#.1608")() end
 end)((function()
 return var(_S35S461607, "#.1607")()
 end) )
 end)(((_S35S461593)[2])[1] ) else return var(_S35S461607, "#.1607")() end
 end)((function()
 return var(_S35S461603, "#.1603")()
 end) ) else return var(_S35S461603, "#.1603")() end else return _S35S461603() end
 end)((function()
 return (function(_S35S461604  )
 return (function(_S35S461606  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then return (function(_fun  )
 return (function(_args  )
 if _builtinS45functionS63(var(_fun, "fun") ) then return (function()
 return var(_return, "return")(_compileS45builtinS45function(_fun,  var(_args, "args") ) )
 end)() else return var(_S35S461604, "#.1604")() end
 end)((_S35S461593)[2] )
 end)((_S35S461593)[1] ) else return var(_S35S461606, "#.1606")() end
 end)((function()
 return var(_S35S461604, "#.1604")()
 end) )
 end)((function()
 return (function(_S35S461605  )
 if _caseS45pairS63(var(_S35S461593, "#.1593") ) then return (function(_fun  )
 return (function(_args  )
 return (function()
 return var(_return, "return")(_compilerS45format("%s(%s)",  _compileS45expr((function(_x  )
 return var(_x, "x")
 end),  var(_fun, "fun") ),  _compileS45args(var(_args, "args") ) ) )
 end)()
 end)((_S35S461593)[2] )
 end)((_S35S461593)[1] ) else return var(_S35S461605, "#.1605")() end
 end)((function()
 return false
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end) )
 end)(var(_expr, "expr") )
 end);
 ignore(_compileS45expr)_compileS45andS45load = (function(_e ,  _env  )
 local _name;
 _name = (function(_S35S461676)
 if _S35S461676 then
 return (function(_S35S461677)
 if _S35S461677 then
 return ((_cadr(var(_e, "e") ))[1])[1] else
 return (_cadr(_e ))[1]
 end
 end)(var(_pairS63, "pair?")(_cadr(_e ) )) else
 return "[expr]"
 end
 end)(_defineS63(_e ));
 return _callS47native(symbol('load'),  (function()
 _enterS45scope(scm_nil );
 return (function(_S35S461675  )
 _leaveS45scope();
 return var(_S35S461675, "#.1675")
 end)((function()
 return var(_compile, "compile")(_e )
 end)() )
 end)(),  var(_name, "name"),  "t",  var(_env, "env") )
 end);
 ignore(_compileS45andS45load)_compileS45andS45run = (function(_e ,  _env  )
 return _compileS45andS45load(var(_e, "e"),  var(_env, "env") )()
 end);
 ignore(_compileS45andS45run)_repl = (function(...)
 local _S35S461678 = list(...);
 return (function(_S35S461679  )
 if _S61(scm_nil,  var(_S35S461679, "#.1679") ) then return (function()
 return _repl(true,  1,  var(_ENV, "ENV"),  false )
 end)() else return (function(_S35S461680  )
 if _caseS45pairS63(_S35S461679 ) then return (function(_p  )
 if _S61(scm_nil,  (_S35S461679)[2] ) then return (function()
 return _repl(var(_p, "p"),  1,  var(_ENV, "ENV"),  false )
 end)() else return var(_S35S461680, "#.1680")() end
 end)((_S35S461679)[1] ) else return var(_S35S461680, "#.1680")() end
 end)((function()
 return (function(_S35S461681  )
 if _caseS45pairS63(_S35S461679 ) then return (function(_p  )
 return (function(_S35S461693  )
 if _caseS45pairS63((_S35S461679)[2] ) then return (function(_i  )
 if _S61(scm_nil,  ((_S35S461679)[2])[2] ) then return (function()
 return _repl(var(_p, "p"),  var(_i, "i"),  var(_ENV, "ENV"),  false )
 end)() else return var(_S35S461693, "#.1693")() end
 end)(((_S35S461679)[2])[1] ) else return var(_S35S461693, "#.1693")() end
 end)((function()
 return var(_S35S461681, "#.1681")()
 end) )
 end)((_S35S461679)[1] ) else return var(_S35S461681, "#.1681")() end
 end)((function()
 return (function(_S35S461682  )
 if _caseS45pairS63(_S35S461679 ) then return (function(_p  )
 return (function(_S35S461691  )
 if _caseS45pairS63((_S35S461679)[2] ) then return (function(_i  )
 return (function(_S35S461692  )
 if _caseS45pairS63(((_S35S461679)[2])[2] ) then return (function(_env  )
 if _S61(scm_nil,  (((_S35S461679)[2])[2])[2] ) then return (function()
 return _repl(var(_p, "p"),  var(_i, "i"),  var(_env, "env"),  false )
 end)() else return var(_S35S461692, "#.1692")() end
 end)((((_S35S461679)[2])[2])[1] ) else return var(_S35S461692, "#.1692")() end
 end)((function()
 return var(_S35S461691, "#.1691")()
 end) )
 end)(((_S35S461679)[2])[1] ) else return var(_S35S461691, "#.1691")() end
 end)((function()
 return var(_S35S461682, "#.1682")()
 end) )
 end)((_S35S461679)[1] ) else return var(_S35S461682, "#.1682")() end
 end)((function()
 return (function(_S35S461683  )
 if _caseS45pairS63(_S35S461679 ) then return (function(_p  )
 return (function(_S35S461688  )
 if _caseS45pairS63((_S35S461679)[2] ) then return (function(_i  )
 return (function(_S35S461689  )
 if _caseS45pairS63(((_S35S461679)[2])[2] ) then return (function(_env  )
 return (function(_S35S461690  )
 if _caseS45pairS63((((_S35S461679)[2])[2])[2] ) then if _S61(false,  ((((_S35S461679)[2])[2])[2])[1] ) then if _S61(scm_nil,  ((((_S35S461679)[2])[2])[2])[2] ) then return (function()
 return _repl(var(_p, "p"),  var(_i, "i"),  var(_env, "env"),  _phase(symbol('loading') ) )
 end)() else return var(_S35S461690, "#.1690")() end else return _S35S461690() end else return _S35S461690() end
 end)((function()
 return var(_S35S461689, "#.1689")()
 end) )
 end)((((_S35S461679)[2])[2])[1] ) else return var(_S35S461689, "#.1689")() end
 end)((function()
 return var(_S35S461688, "#.1688")()
 end) )
 end)(((_S35S461679)[2])[1] ) else return var(_S35S461688, "#.1688")() end
 end)((function()
 return var(_S35S461683, "#.1683")()
 end) )
 end)((_S35S461679)[1] ) else return var(_S35S461683, "#.1683")() end
 end)((function()
 return (function(_S35S461684  )
 if _caseS45pairS63(_S35S461679 ) then return (function(_p  )
 return (function(_S35S461685  )
 if _caseS45pairS63((_S35S461679)[2] ) then return (function(_i  )
 return (function(_S35S461686  )
 if _caseS45pairS63(((_S35S461679)[2])[2] ) then return (function(_env  )
 return (function(_S35S461687  )
 if _caseS45pairS63((((_S35S461679)[2])[2])[2] ) then return (function(_old  )
 if _S61(scm_nil,  ((((_S35S461679)[2])[2])[2])[2] ) then return (function()
 (function(_S35S461715)
 if _S35S461715 then
 return (function(_S35S461716)
 if _S35S461716 then
 return (function()
 return var(_display, "display")("boot> " )
 end)() else
 return (function(_S35S461717)
 if _S35S461717 then
 return (function()
 return var(_display, "display")("> " )
 end)() else
 return (function(_S35S461718)
 if _S35S461718 then
 return (function()
 return var(_display, "display")("load> " )
 end)() else
 return false
 end
 end)(_else)
 end
 end)(_eqS63(_platform,  "Scheme 51" ))
 end
 end)(_eqS63(_platform,  "Boot Scheme" )) else
 return false
 end
 end)(var(_p, "p"));
 return (function(_x  )
 if _eqS63(var(_x, "x"),  scm_eof ) then return _phase(var(_old, "old") ) else return (function()
 var(_catch, "catch")((function()
 (function(_S35S461720)
 if _S35S461720 then
 return var(_write, "write") else
 return (function(_x  )
 return false
 end)
 end
 end)(_p)(_compileS45andS45run(_x,  var(_env, "env") ) );
 if _p then return _newline() else return false end
 end),  (function(_e  )
 return var(_display, "display")("Error in user code: ",  var(_e, "e"),  "\
" )
 end) );
 return _repl(_p,  var(_S43, "+")(1,  var(_i, "i") ),  var(_env, "env"),  _old )
 end)() end
 end)(var(_read, "read")() )
 end)() else return var(_S35S461687, "#.1687")() end
 end)(((((_S35S461679)[2])[2])[2])[1] ) else return var(_S35S461687, "#.1687")() end
 end)((function()
 return var(_S35S461686, "#.1686")()
 end) )
 end)((((_S35S461679)[2])[2])[1] ) else return var(_S35S461686, "#.1686")() end
 end)((function()
 return var(_S35S461685, "#.1685")()
 end) )
 end)(((_S35S461679)[2])[1] ) else return var(_S35S461685, "#.1685")() end
 end)((function()
 return var(_S35S461684, "#.1684")()
 end) )
 end)((_S35S461679)[1] ) else return var(_S35S461684, "#.1684")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461678, "#.1678"),  " in ",  {{scm_nil,{{symbol('repl'),{true,{1,{symbol('ENV'),{false,scm_nil}}}}},scm_nil}},{{{symbol('p'),scm_nil},{{symbol('repl'),{symbol('p'),{1,{symbol('ENV'),{false,scm_nil}}}}},scm_nil}},{{{symbol('p'),{symbol('i'),scm_nil}},{{symbol('repl'),{symbol('p'),{symbol('i'),{symbol('ENV'),{false,scm_nil}}}}},scm_nil}},{{{symbol('p'),{symbol('i'),{symbol('env'),scm_nil}}},{{symbol('repl'),{symbol('p'),{symbol('i'),{symbol('env'),{false,scm_nil}}}}},scm_nil}},{{{symbol('p'),{symbol('i'),{symbol('env'),{false,scm_nil}}}},{{symbol('repl'),{symbol('p'),{symbol('i'),{symbol('env'),{{symbol('phase'),{{symbol('quote'),{symbol('loading'),scm_nil}},scm_nil}},scm_nil}}}}},scm_nil}},{{{symbol('p'),{symbol('i'),{symbol('env'),{symbol('old'),scm_nil}}}},{{symbol('if'),{symbol('p'),{{symbol('cond'),{{{symbol('eq?'),{symbol('platform'),{"Boot Scheme",scm_nil}}},{{symbol('display'),{"boot> ",scm_nil}},scm_nil}},{{{symbol('eq?'),{symbol('platform'),{"Scheme 51",scm_nil}}},{{symbol('display'),{"> ",scm_nil}},scm_nil}},{{symbol('else'),{{symbol('display'),{"load> ",scm_nil}},scm_nil}},scm_nil}}}},scm_nil}}},{{symbol('let'),{{{symbol('x'),{{symbol('read'),scm_nil},scm_nil}},scm_nil},{{symbol('if'),{{symbol('eq?'),{symbol('x'),{scm_eof,scm_nil}}},{{symbol('phase'),{symbol('old'),scm_nil}},{{symbol('begin'),{{symbol('catch'),{{symbol('lambda'),{scm_nil,{{{symbol('if'),{symbol('p'),{symbol('write'),{{symbol('lambda'),{{symbol('x'),scm_nil},{false,scm_nil}}},scm_nil}}}},{{symbol('compile-and-run'),{symbol('x'),{symbol('env'),scm_nil}}},scm_nil}},{{symbol('if'),{symbol('p'),{{symbol('newline'),scm_nil},scm_nil}}},scm_nil}}}},{{symbol('lambda'),{{symbol('e'),scm_nil},{{symbol('display'),{"Error in user code: ",{symbol('e'),{"\
",scm_nil}}}},scm_nil}}},scm_nil}}},{{symbol('repl'),{symbol('p'),{{symbol('+'),{1,{symbol('i'),scm_nil}}},{symbol('env'),{symbol('old'),scm_nil}}}}},scm_nil}}},scm_nil}}}},scm_nil}}},scm_nil}}},scm_nil}}}}}} )
 end) )
 end) )
 end) )
 end) )
 end) ) end
 end)(var(_S35S461678, "#.1678") )
 end);
 ignore(_repl)_S42loadedS45modulesS42 = scm_nil;
 ignore(_S42loadedS45modulesS42)_compileS45file = (function(_path  )
 local _loop;
 _loop = (function()
 return (function(_x  )
 if _eqS63(var(_x, "x"),  scm_eof ) then return true else return (function()
 var(_display, "display")(_compileS45expr((function(_x  )
 return _compilerS45format("ignore(%s)",  var(_x, "x") )
 end),  _expand(_x ) ) );
 return var(_loop, "loop")()
 end)() end
 end)(var(_read, "read")() )
 end);
 return (function(_S35S461722  )
 _phase((function()
 return symbol('compiling')
 end)() );
 return (function(_S35S461723  )
 _phase(var(_S35S461722, "#.1722") );
 return var(_S35S461723, "#.1723")
 end)((function()
 return var(_withS45inputS45fromS45file, "with-input-from-file")(var(_path, "path"),  var(_loop, "loop") )
 end)() )
 end)(_phase() )
 end);
 ignore(_compileS45file)_eval = (function(...)
 local _S35S461725 = list(...);
 return (function(_S35S461726  )
 return (function(_S35S461727  )
 if _caseS45pairS63(var(_S35S461726, "#.1726") ) then return (function(_e  )
 if _S61(scm_nil,  (_S35S461726)[2] ) then return (function()
 return _compileS45andS45run(var(_e, "e"),  var(_ENV, "ENV") )
 end)() else return var(_S35S461727, "#.1727")() end
 end)((_S35S461726)[1] ) else return var(_S35S461727, "#.1727")() end
 end)((function()
 return (function(_S35S461728  )
 if _caseS45pairS63(var(_S35S461726, "#.1726") ) then return (function(_e  )
 return (function(_S35S461729  )
 if _caseS45pairS63((_S35S461726)[2] ) then return (function(_env  )
 if _S61(scm_nil,  ((_S35S461726)[2])[2] ) then return (function()
 return _compileS45andS45run(var(_e, "e"),  var(_env, "env") )
 end)() else return var(_S35S461729, "#.1729")() end
 end)(((_S35S461726)[2])[1] ) else return var(_S35S461729, "#.1729")() end
 end)((function()
 return var(_S35S461728, "#.1728")()
 end) )
 end)((_S35S461726)[1] ) else return var(_S35S461728, "#.1728")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461725, "#.1725"),  " in ",  {{{symbol('e'),scm_nil},{{symbol('compile-and-run'),{symbol('e'),{symbol('ENV'),scm_nil}}},scm_nil}},{{{symbol('e'),{symbol('env'),scm_nil}},{{symbol('compile-and-run'),{symbol('e'),{symbol('env'),scm_nil}}},scm_nil}},scm_nil}} )
 end) )
 end) )
 end)(var(_S35S461725, "#.1725") )
 end);
 ignore(_eval)_load = (function(...)
 local _S35S461735 = list(...);
 return (function(_S35S461736  )
 return (function(_S35S461737  )
 if _caseS45pairS63(var(_S35S461736, "#.1736") ) then return (function(_path  )
 if _S61(scm_nil,  (_S35S461736)[2] ) then return (function()
 return var(_withS45inputS45fromS45file, "with-input-from-file")(var(_path, "path"),  (function()
 return _repl(false,  0 )
 end) )
 end)() else return var(_S35S461737, "#.1737")() end
 end)((_S35S461736)[1] ) else return var(_S35S461737, "#.1737")() end
 end)((function()
 return (function(_S35S461738  )
 if _caseS45pairS63(var(_S35S461736, "#.1736") ) then return (function(_path  )
 return (function(_S35S461739  )
 if _caseS45pairS63((_S35S461736)[2] ) then return (function(_env  )
 if _S61(scm_nil,  ((_S35S461736)[2])[2] ) then return (function()
 return var(_withS45inputS45fromS45file, "with-input-from-file")(var(_path, "path"),  (function()
 return _repl(false,  0,  var(_env, "env") )
 end) )
 end)() else return var(_S35S461739, "#.1739")() end
 end)(((_S35S461736)[2])[1] ) else return var(_S35S461739, "#.1739")() end
 end)((function()
 return var(_S35S461738, "#.1738")()
 end) )
 end)((_S35S461736)[1] ) else return var(_S35S461738, "#.1738")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461735, "#.1735"),  " in ",  {{{symbol('path'),scm_nil},{{symbol('with-input-from-file'),{symbol('path'),{{symbol('lambda'),{scm_nil,{{symbol('repl'),{false,{0,scm_nil}}},scm_nil}}},scm_nil}}},scm_nil}},{{{symbol('path'),{symbol('env'),scm_nil}},{{symbol('with-input-from-file'),{symbol('path'),{{symbol('lambda'),{scm_nil,{{symbol('repl'),{false,{0,{symbol('env'),scm_nil}}}},scm_nil}}},scm_nil}}},scm_nil}},scm_nil}} )
 end) )
 end) )
 end)(var(_S35S461735, "#.1735") )
 end);
 ignore(_load)ignore(_pushS45macroS33(symbol('run/native'),  (function(_S35S461745  )
 return var(_apply, "apply")((function(_r  )
 return {symbol('begin'), {{symbol('if'), {symbol('booting'), {{symbol('display'), {"\
", {var(_r, "r"), {"\
", scm_nil}}}}, scm_nil}}}, {{{symbol('call/native'), {{symbol('quote'), {symbol('load'), scm_nil}}, {_r, scm_nil}}}, scm_nil}, scm_nil}}}
 end),  var(_S35S461745, "#.1745") )
 end) ))ignore((function()
 (function(_S35S461746)
 if _S35S461746 then
 return var(_display, "display")("\
",  "function var(x, n)\
      if x ~= nil then return x end\
      return error('no binding for symbol ' .. n, 2)\
    end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function var(x, n)\
      if x ~= nil then return x end\
      return error('no binding for symbol ' .. n, 2)\
    end" )()
 end)())ignore((function()
 (function(_S35S461747)
 if _S35S461747 then
 return var(_display, "display")("\
",  "function list(car, ...)\
     if car ~= nil then\
       return { car, list(...) }\
     else\
       return scm_nil\
     end\
   end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function list(car, ...)\
     if car ~= nil then\
       return { car, list(...) }\
     else\
       return scm_nil\
     end\
   end" )()
 end)())ignore((function()
 (function(_S35S461748)
 if _S35S461748 then
 return var(_display, "display")("\
",  "function scm_set_helper(var, val, name)\
     assert(var ~= nil, 'no previous binding for symbol ' .. name);\
     return val\
   end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function scm_set_helper(var, val, name)\
     assert(var ~= nil, 'no previous binding for symbol ' .. name);\
     return val\
   end" )()
 end)())ignore(_pushS45macroS33(symbol('define/native'),  (function(_S35S461749  )
 return var(_apply, "apply")((function(_name ,  _body  )
 if var(_pairS63, "pair?")(var(_name, "name") ) then return {symbol('run/native'), {_compilerS45format("function %s(%s) %s end",  _escapeS45symbol((_name)[1] ),  _simpleS45argumentS45list("",  (_name)[2] ),  var(_body, "body") ), scm_nil}} else return {symbol('run/native'), {_compilerS45format("%s = %s",  _escapeS45symbol((_name)[1] ),  _body ), scm_nil}} end
 end),  var(_S35S461749, "#.1749") )
 end) ))_compile = (function(_e  )
 return _compileS45expr((function(_x  )
 return _compilerS45format("return %s",  var(_x, "x") )
 end),  _expand(var(_e, "e") ) )
 end);
 ignore(_compile)ignore((function()
 (function(_S35S461752)
 if _S35S461752 then
 return var(_display, "display")("\
",  "if not _ENV then _ENV = _G end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "if not _ENV then _ENV = _G end" )()
 end)())ignore((function()
 (function(_S35S461753)
 if _S35S461753 then
 return var(_display, "display")("\
",  "function _environment() return _ENV end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _environment() return _ENV end" )()
 end)())ignore((function()
 (function(_S35S461754)
 if _S35S461754 then
 return var(_display, "display")("\
",  "function _setS45carS33(_cell ,  _val  ) _cell[1] = _val; return true end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _setS45carS33(_cell ,  _val  ) _cell[1] = _val; return true end" )()
 end)())ignore((function()
 (function(_S35S461755)
 if _S35S461755 then
 return var(_display, "display")("\
",  "function _car(_cell  ) return _cell[1] end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _car(_cell  ) return _cell[1] end" )()
 end)())ignore((function()
 (function(_S35S461756)
 if _S35S461756 then
 return var(_display, "display")("\
",  "function _setS45cdrS33(_cell ,  _val  ) _cell[2] = _val; return true end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _setS45cdrS33(_cell ,  _val  ) _cell[2] = _val; return true end" )()
 end)())ignore((function()
 (function(_S35S461757)
 if _S35S461757 then
 return var(_display, "display")("\
",  "function _cdr(_cell  ) return _cell[2] end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _cdr(_cell  ) return _cell[2] end" )()
 end)())ignore((function()
 (function(_S35S461758)
 if _S35S461758 then
 return var(_display, "display")("\
",  "function _nullS63(_p  ) return _p == scm_nil or _p == nil end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _nullS63(_p  ) return _p == scm_nil or _p == nil end" )()
 end)())ignore((function()
 (function(_S35S461759)
 if _S35S461759 then
 return var(_display, "display")("\
",  "function _numberS63(_p  ) return type(_p) == 'number' or (type(_p) == 'table' and _p[0] == rational) end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _numberS63(_p  ) return type(_p) == 'number' or (type(_p) == 'table' and _p[0] == rational) end" )()
 end)())ignore((function()
 (function(_S35S461760)
 if _S35S461760 then
 return var(_display, "display")("\
",  "function _stringS63(_p  ) return type(_p) == 'string' end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _stringS63(_p  ) return type(_p) == 'string' end" )()
 end)())ignore((function()
 (function(_S35S461761)
 if _S35S461761 then
 return var(_display, "display")("\
",  "function _keywordS63(_p  ) return _symbolS63(_p) and _p.kw ~= nil end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _keywordS63(_p  ) return _symbolS63(_p) and _p.kw ~= nil end" )()
 end)())ignore((function()
 (function(_S35S461762)
 if _S35S461762 then
 return var(_display, "display")("\
",  "function _procedureS63(_p  ) return type(_p) == 'function' or (type(_p) == 'table' and getmetatable(_p) and type(getmetatable(_p).__call) == 'function') end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _procedureS63(_p  ) return type(_p) == 'function' or (type(_p) == 'table' and getmetatable(_p) and type(getmetatable(_p).__call) == 'function') end" )()
 end)())ignore((function()
 (function(_S35S461763)
 if _S35S461763 then
 return var(_display, "display")("\
",  "function _charS63(_p  ) return type(_p) == 'string' and #_p == 1 end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _charS63(_p  ) return type(_p) == 'string' and #_p == 1 end" )()
 end)())ignore((function()
 (function(_S35S461764)
 if _S35S461764 then
 return var(_display, "display")("\
",  "function _cons(_a ,  _b  ) return {_a,_b} end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _cons(_a ,  _b  ) return {_a,_b} end" )()
 end)())ignore((function()
 (function(_S35S461765)
 if _S35S461765 then
 return var(_display, "display")("\
",  "function _hashS45ref(_t ,  _k ,  _def  ) if _t[_k] ~= nil then return _t[_k] else return _def or false end end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _hashS45ref(_t ,  _k ,  _def  ) if _t[_k] ~= nil then return _t[_k] else return _def or false end end" )()
 end)())ignore((function()
 (function(_S35S461766)
 if _S35S461766 then
 return var(_display, "display")("\
",  "function _callS45withS45values(_pro ,  _con  ) return _con(_pro()) end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _callS45withS45values(_pro ,  _con  ) return _con(_pro()) end" )()
 end)())ignore((function()
 (function(_S35S461767)
 if _S35S461767 then
 return var(_display, "display")("\
",  "function _values(...)\
     return ...\
   end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _values(...)\
     return ...\
   end" )()
 end)())ignore((function()
 (function(_S35S461768)
 if _S35S461768 then
 return var(_display, "display")("\
",  "function _hashS45forS45each(_hash ,  _func  ) local n = 0\
   for k, v in pairs(_hash) do\
     _func(k, v)\
     n = n + 1\
   end\
   return n end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _hashS45forS45each(_hash ,  _func  ) local n = 0\
   for k, v in pairs(_hash) do\
     _func(k, v)\
     n = n + 1\
   end\
   return n end" )()
 end)())ignore((function()
 (function(_S35S461769)
 if _S35S461769 then
 return var(_display, "display")("\
",  "function _rationalS63(_p  ) return type(_p) == 'table' and _p[0] == rational end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _rationalS63(_p  ) return type(_p) == 'table' and _p[0] == rational end" )()
 end)())ignore((function(_S35S461773)
 if _S35S461773 then
 return (function()
 (function(_S35S461774)
 if _S35S461774 then
 return var(_display, "display")("\
",  "_platform = 'Scheme 51'",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "_platform = 'Scheme 51'" )()
 end)() else
 return (function()
 (function(_S35S461775)
 if _S35S461775 then
 return var(_display, "display")("\
",  "_platform = 'Boot Scheme'",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "_platform = 'Boot Scheme'" )()
 end)()
 end
 end)((function(_S35S461770  )
 if var(_S35S461770, "#.1770") then return _S35S461770 else return (function(_S35S461771  )
 if var(_S35S461771, "#.1771") then return _S35S461771 else return false end
 end)(_eqS63(_platform,  "Boot Scheme" ) ) end
 end)(_eqS63(_platform,  "Scheme 51" ) )))ignore((function()
 (function(_S35S461778)
 if _S35S461778 then
 return var(_display, "display")("\
",  "function _withS45inputS45fromS45file(_path ,  _thunk  ) return input_from_file(_path, _thunk) end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _withS45inputS45fromS45file(_path ,  _thunk  ) return input_from_file(_path, _thunk) end" )()
 end)())ignore((function()
 (function(_S35S461779)
 if _S35S461779 then
 return var(_display, "display")("\
",  "function _withS45outputS45toS45file(_path ,  _thunk  ) return output_to_file(_path, _thunk) end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _withS45outputS45toS45file(_path ,  _thunk  ) return output_to_file(_path, _thunk) end" )()
 end)())ignore((function()
 (function(_S35S461780)
 if _S35S461780 then
 return var(_display, "display")("\
",  "_booting = false",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "_booting = false" )()
 end)())ignore((function()
 (function(_S35S461781)
 if _S35S461781 then
 return var(_display, "display")("\
",  "function _exactS63(_n  ) return _rationalS63(_n) or math.type(_n) == 'integer' end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _exactS63(_n  ) return _rationalS63(_n) or math.type(_n) == 'integer' end" )()
 end)())ignore((function()
 (function(_S35S461782)
 if _S35S461782 then
 return var(_display, "display")("\
",  "function _inexactS63(_n  ) return math.type(_n) == 'float' end",  "\
" ) else
 return false
 end
 end)(var(_booting, "booting"));
 return _callS47native(symbol('load'),  "function _inexactS63(_n  ) return math.type(_n) == 'float' end" )()
 end)())_moduleS45name = _makeS45parameter(symbol('main') );
 ignore(_moduleS45name)_pathS45S62string = (function(_modS45path  )
 return (function(_S35S461783  )
 if _S61(scm_nil,  var(_S35S461783, "#.1783") ) then return (function()
 return var(_error, "error")("empty path" )
 end)() else return (function(_S35S461784  )
 return (function(_S35S461787  )
 if _caseS45pairS63(_S35S461783 ) then return (function(_x  )
 if _S61(scm_nil,  (_S35S461783)[2] ) then if var(_symbolS63, "symbol?")(var(_x, "x") ) then return (function()
 return _compilerS45format("%s.ss",  (_x)[1] )
 end)() else return var(_S35S461784, "#.1784")() end else return var(_S35S461787, "#.1787")() end
 end)((_S35S461783)[1] ) else return var(_S35S461787, "#.1787")() end
 end)((function()
 return var(_S35S461784, "#.1784")()
 end) )
 end)((function()
 return (function(_S35S461785  )
 return (function(_S35S461786  )
 if _caseS45pairS63(_S35S461783 ) then return (function(_a  )
 return (function(_b  )
 if var(_symbolS63, "symbol?")(var(_a, "a") ) then return (function()
 return _compilerS45format("%s/%s",  (_a)[1],  _pathS45S62string(var(_b, "b") ) )
 end)() else return var(_S35S461785, "#.1785")() end
 end)((_S35S461783)[2] )
 end)((_S35S461783)[1] ) else return var(_S35S461786, "#.1786")() end
 end)((function()
 return var(_S35S461785, "#.1785")()
 end) )
 end)((function()
 return false
 end) )
 end) ) end
 end)(var(_modS45path, "mod-path") )
 end);
 ignore(_pathS45S62string)_loadedS45modules = _makeS45parameter(_makeS45hashS45table() );
 ignore(_loadedS45modules)_dofile = (function(_p ,  _env  )
 local _h;
 _h = _callS47native(symbol('assert'),  _callS47native({symbol('fs'),{symbol('open'),scm_nil}},  var(_p, "p"),  "r" ) );
 local _c;
 _c = _callS42(var(_h, "h"),  "readAll" );
 return _callS47native(symbol('load'),  var(_c, "c"),  _p,  "t",  var(_env, "env") )()
 end);
 ignore(_dofile)_doS45loadS45module = (function(_S35S461794)
 if _S35S461794 then
 return (function(_path ,  _env  )
 (function(_S35S461795)
 if _S35S461795 then
 return false else
 return (function()
 return var(_error, "error")("no such module",  var(_path, "path") )
 end)()
 end
 end)(_callS47native({symbol('fs'),{symbol('exists'),scm_nil}},  var(_path, "path") ));
 local _moduleS45aux;
 _moduleS45aux = _stringS45append(_path,  ".lua" );
 local _modS45mtime;
 _modS45mtime = var(_hashS45ref, "hash-ref")(_callS47native({symbol('fs'),{symbol('attributes'),scm_nil}},  _path ),  "modification" );
 return var(_catch, "catch")((function()
 if (function(_S35S461797)
 if _S35S461797 then
 return var(_S62S61, ">=")(_hashS45ref(_callS47native({symbol('fs'),{symbol('attributes'),scm_nil}},  var(_moduleS45aux, "module-aux") ),  "modification" ),  var(_modS45mtime, "mod-mtime") ) else
 return false
 end
 end)(_callS47native({symbol('fs'),{symbol('exists'),scm_nil}},  _moduleS45aux )) then return (function()
 return _dofile(_moduleS45aux,  var(_env, "env") )
 end)() else if _else then return (function()
 var(_withS45outputS45toS45file, "with-output-to-file")(_moduleS45aux,  (function()
 return _compileS45file(_path )
 end) );
 return _dofile(_moduleS45aux,  var(_env, "env") )
 end)() else return false end end
 end),  (function(_e  )
 if (function(_S35S461800)
 if _S35S461800 then
 return _exitS45errorS63((var(_e, "e"))[1] ) else
 return false
 end
 end)(var(_pairS63, "pair?")(_e )) then return (function()
 _callS47native({symbol('fs'),{symbol('delete'),scm_nil}},  var(_moduleS45aux, "module-aux") );
 return _load(_path,  var(_env, "env") )
 end)() else return var(_error, "error")(_e ) end
 end) )
 end) else
 return _load
 end
 end)(var(_hashS45ref, "hash-ref")(var(_environment, "environment")(),  "fs" ));
 ignore(_doS45loadS45module)_loadS45mod = (function(_S35S461810)
 if _S35S461810 then
 return (function(_module ,  _env  )
 local _path;
 _path = _pathS45S62string(var(_module, "module") );
 (function(_S35S461811)
 if _S35S461811 then
 return false else
 return (function()
 return var(_error, "error")("no such module: ",  _module )
 end)()
 end
 end)(_callS47native({symbol('fs'),{symbol('exists'),scm_nil}},  var(_path, "path") ));
 local _mtime;
 _mtime = _hashS45ref(_callS47native({symbol('fs'),{symbol('attributes'),scm_nil}},  _path ),  "modification" );
 local _doS45load;
 _doS45load = (function()
 _hashS45setS33(_loadedS45modules(),  _path,  symbol('loading') );
 (function(_name  )
 _moduleS45name(_module );
 _doS45loadS45module(_path,  var(_env, "env") );
 return _moduleS45name(var(_name, "name") )
 end)((function(_S35S461801  )
 if var(_S35S461801, "#.1801") then return _S35S461801 else return (function(_S35S461802  )
 if var(_S35S461802, "#.1802") then return _S35S461802 else return false end
 end)(symbol('main') ) end
 end)(_moduleS45name() ) );
 _hashS45setS33(_loadedS45modules(),  _path,  var(_mtime, "mtime") );
 return true
 end);
 return (function(_S35S461804  )
 if _S61(symbol('loading'),  var(_S35S461804, "#.1804") ) then return (function()
 return var(_error, "error")("Cycle in module dependency graph: ",  _module,  " is already being loaded" )
 end)() else if _S61(false,  _S35S461804 ) then return (function()
 return var(_doS45load, "do-load")()
 end)() else return (function(_S35S461805  )
 return (function(_x  )
 if var(_numberS63, "number?")(var(_x, "x") ) then return (function()
 if var(_S62, ">")(var(_mtime, "mtime"),  _x ) then return var(_doS45load, "do-load")() else return "already loaded" end
 end)() else return var(_S35S461805, "#.1805")() end
 end)(_S35S461804 )
 end)((function()
 return false
 end) ) end end
 end)(_hashS45ref(_loadedS45modules(),  _path ) )
 end) else
 return (function(_module ,  _env  )
 local _path;
 _path = _pathS45S62string(var(_module, "module") );
 var(_write, "write")(var(_path, "path"),  "\
" );
 return (function(_S35S461806  )
 if _S61(symbol('loading'),  var(_S35S461806, "#.1806") ) then return (function()
 return var(_error, "error")("Cycle in module dependency graph: ",  _module,  " is already being loaded" )
 end)() else if _S61(false,  _S35S461806 ) then return (function()
 _hashS45setS33(_loadedS45modules(),  _path,  symbol('loading') );
 (function(_name  )
 _moduleS45name(_module );
 _load(_path,  var(_env, "env") );
 return _moduleS45name(var(_name, "name") )
 end)((function(_S35S461807  )
 if var(_S35S461807, "#.1807") then return _S35S461807 else return (function(_S35S461808  )
 if var(_S35S461808, "#.1808") then return _S35S461808 else return false end
 end)(symbol('main') ) end
 end)(_moduleS45name() ) );
 _hashS45setS33(_loadedS45modules(),  _path,  false );
 return true
 end)() else if _S61(true,  _S35S461806 ) then return (function()
 return true
 end)() else return false end end end
 end)(_hashS45ref(_loadedS45modules(),  _path ) )
 end)
 end
 end)(_hashS45ref(var(_ENV, "ENV"),  "fs" ));
 ignore(_loadS45mod)ignore(_pushS45macroS33(symbol('use-module'),  (function(_S35S461823  )
 return var(_apply, "apply")((function(_path  )
 (function(_x  )
 return true
 end)(_runS45withS45exit((function()
 _eval(_loadS45mod(var(_path, "path"),  _ENV ) );
 return _hashS45setS33(_loadedS45modules(),  _pathS45S62string(_path ),  false )
 end) ) );
 return {symbol('load-mod'), {{symbol('quote'), {var(_path, "path"), scm_nil}}, {symbol('ENV'), scm_nil}}}
 end),  var(_S35S461823, "#.1823") )
 end) ))ignore(_pushS45macroS33(symbol('use-modules'),  (function(_S35S461824  )
 return var(_apply, "apply")((function(...)
 local _mods = list(...);
 if var(_nullS63, "null?")(var(_mods, "mods") ) then return true else return {symbol('begin'), {{symbol('use-module'), {(_mods)[1], scm_nil}}, {{symbol('use-modules'), (_mods)[2]}, scm_nil}}} end
 end),  var(_S35S461824, "#.1824") )
 end) ))