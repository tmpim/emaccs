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
