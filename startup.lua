local input, input_file = {}

local write = write

if not table.pack then
  table.pack = function(...)
    return { n = select('#', ...), ... }
  end
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

local function getchar()
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

local function read_number(acc, allow_dot)
  local allow_dot = allow_dot == nil and true or false
  local ch = getchar()
  if '0' <= ch and ch <= '9' then
    return read_number(acc .. ch)
  elseif ch == '.' and allow_dot then
    local dec = read_number('', false)
    return read_number(acc .. ch .. tostring(dec))
  elseif ch == '/' and allow_dot then
    local denom = read_number('', false)
    return rational(tonumber(acc), tonumber(denom))
  else
    ungetchar(ch)
    return assert(tonumber(acc))
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

local function insist_delim(x)
  local ch = peek()
  if ch == ' ' or ch == '\t' or ch == '\n' then
    return x
  else
    return error("expected delimiter, got " .. getchar())
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
    end
    return insist_delim('n')
  elseif ch == 't' then
    if peekp('a') then
      eat('ab')
      return '\t'
    end
    return insist_delim('t')
  else
    return ch
  end
end

local scm_nil, scm_eof, eval, callproc = {}, {}, {}, {}

local function read_special_atom()
  local ch = getchar()
  if ch == 't' then
    return true
  elseif ch == 'f' then
    return false
  elseif ch == '\\' then
    return read_character()
  elseif ch == ';' then
    read_sexpr()
    return true
  elseif ch == ':' then
    return read_symbol('', function(s)
      return {[0] = symbol, kw = true, s}
    end)
  elseif ch == 'e' then
    eat('of')
    return scm_eof
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
      return -read_number(ch)
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

--{{{
local eval, apply, callproc, eval_args, apply_dispatch
--}}}

local function scm_print(e)
  if type(e) == 'table' then
    if symbolp(e) then
      write(e[1])
    elseif e[0] == eval then
      write '#\'<closure>'
    elseif consp(e) then
      write '('
      repeat
        scm_print(e[1])
        if consp(e[2]) then
          write ' '
        end
        e = e[2]
      until not consp(e)
      if e ~= scm_nil then
        write ' . '
        scm_print(e)
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
        scm_print(k)
        write ' . '
        scm_print(v)
        write ')'
        if next(e, k) ~= nil then
          write ' '
        end
      end
      write ')'
    end
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
--{{{

local function throw(errk, ...)
  return errk({[0]='error', ...})
end

local function find(env, ex)
  if env == nil or env == scm_nil then
    return nil
  elseif env[1][ex] ~= nil then
    return env[1][ex]
  else
    return find(env[2], ex)
  end
end

local function setenv(okk, errk, env, what, to)
  if env == nil or env == scm_nil then
    return throw(errk, "no binding for", what, "anywhere in environment")
  elseif env[1] and env[1][what] ~= nil then
    env[1][what] = to
    return okk(to)
  else
    return setenv(okk, errk, env[2], what, to)
  end
end

local reductions = 0

function eval(expr, env, okk, errk)
  if _CC_DEFAULT_SETTINGS and reductions % (2^12) == 0 then
    os.queueEvent('x')
    os.pullEvent('x')
  end
  reductions = reductions + 1
  if symbolp(expr) and expr[1] ~= '' and not expr.kw then
    local ex = find(env, expr[1])
    if ex ~= nil then
      return okk(ex)
    else
      return throw(errk, 'no binding for symbol ' .. expr[1])
    end
  elseif consp(expr) and expr[1] == _lambda then
    if not consp(expr[2][2]) then
      return throw(errk, "not a valid lambda expression", expr)
    end
    return okk({[0]=eval,expr[2][1],env,expr[2][2]})
  elseif consp(expr) and expr[1] == _if then
    return eval(expr[2][1], env, function(c)
      if c ~= false then
        return eval(expr[2][2][1], env, okk, errk)
      elseif expr[2][2][2] == scm_nil then
        return okk(false)
      else
        return eval(expr[2][2][2][1], env, okk, errk)
      end
    end, errk)
  elseif consp(expr) and expr[1] == _define then
    if consp(expr[2]) and symbolp(expr[2][1]) and consp(expr[2][2]) then
      return eval(expr[2][2][1], env, function(x)
        env[1][expr[2][1][1]] = x
        return okk(x)
      end, errk)
    elseif consp(expr[2]) and consp(expr[2][1]) and consp(expr[2][2]) then
      return eval({ _define, { expr[2][1][1], { { _lambda, { expr[2][1][2], expr[2][2] } }, scm_nil } } }, env, okk, errk)
    else
      return throw(errk, 'invalid define expression', expr)
    end
  elseif consp(expr) and expr[1] == _quote then
    return okk(expr[2][1])
  elseif consp(expr) and expr[1] == _set then
    if not symbolp(expr[2][1]) then
      return throw(errk, 'can not set! non-symbol',expr[2][1])
    end
    local what = expr[2][1][1]
    return eval(expr[2][2][1], env, function(x)
      return setenv(okk, errk, env, what, x)
    end, errk)
  elseif consp(expr) then
    return eval(expr[1], env, function(f)
      return apply_dispatch(f, expr[2], env, okk, function(e)
        scm_print(expr); print()
        return errk(e)
      end)
    end, errk)
  elseif is_self_eval(expr) then
    return okk(expr)
  elseif expr[0] == symbol and expr.kw then
    return throw(errk, "illegal use of keyword", expr, "as expression")
  else
    return throw(errk, "don't know how to evaluate object", expr)
  end
end

local function copy(e)
  local t = {}
  for k, v in pairs(e) do
    t[k] = v
  end
  return t
end

local function make_env(a, b, t)
  if consp(a) and symbolp(a[1]) then
    if consp(b) then
      t[a[1][1]] = b[1]
      return make_env(a[2], b[2], t)
    end
  elseif symbolp(a) then
    t[a[1]] = b
  end
  return t
end

function apply(fun, args, env, okk, errk)
  return eval_args(args, env, function(args)
    local fa, fb = fun[1], fun[3]
    local fenv = { make_env(fa, args, {}), { fun[2][1], env } }
    local function eval_body(b, acc, ...)
      if b == scm_nil then
        return okk((acc or false), ...)
      else
        return eval(b[1], fenv, function(...)
          return eval_body(b[2], ...)
        end, errk)
      end
    end
    return eval_body(fb)
  end, errk)
end

function callproc(fun, args, env, okk, errk, cont_aware)
  return eval_args(args, env, function(args)
    local t = {}
    repeat
      table.insert(t, args[1])
      args = args[2]
    until args == scm_nil or not args
    if cont_aware then
      assert(errk)
      return fun[1](okk, errk, env, unpack(t))
    else
      -- if we use pcall here then we can't guarantee unbounded stack space
      -- for calls that go through a Lua function
      return okk(fun(unpack(t)))
    end
  end, errk)
end

function apply_dispatch(f, args, env, okk, errk)
  if type(f) == 'table' and f[0] == eval then
    return apply(f, args, env, okk, errk)
  elseif type(f) == 'table' and f[0] == callproc then
    assert(errk)
    return callproc(f, args, env, okk, errk, true)
  elseif type(f) == 'function' then
    return callproc(f, args, env, okk, errk)
  else
    return throw(errk, 'can not apply non-functional object of type ' .. type(f))
  end
end

function eval_args(args, env, okk, errk)
  if args == scm_nil or args == nil then
    return okk(scm_nil)
  elseif args[0] == eval_args then
    return okk(args[1])
  else
    return eval(args[1], env, function(c)
      return eval_args(args[2], env, function(t)
        return okk({c,t})
      end, errk)
    end, errk)
  end
end

local function quote(e)
  return {{_quote, {e, scm_nil}}, scm_nil}
end

local scm_loaded = {}

local function scm_load(okk, errk, env, path)
  if not path then return throw(errk, 'no file path specified') end
  local h = io.open(path, 'r')
  if not h then
    return throw(errk, "failed to open file " .. path .. " for reading")
  else
    input_file = h
    local function scm_load_loop(i)
      local ok, err = pcall(read_sexpr)
      if ok and err ~= scm_eof then
        input_file = nil
        return apply_dispatch(find(env, 'expand'), quote(err), env, function(expr)
          return eval(expr, env, function(value)
            input_file = h
            return scm_load_loop(i + 1)
          end, errk)
        end, errk)
      elseif err == scm_eof then
        input_file = nil
        h:close()
        return okk(true)
      else
        input_file = nil
        h:close()
        return errk(err)
      end
    end
    return scm_load_loop(0)
  end
end
--}}}
--
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
  return rational(x[1] * y[2] + y[1] * x[2], x[2] * y[2])
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
--{{{
  ['call/cc'] = {
    [0] = callproc,
    function(okk, errk, env, f)
      return apply_dispatch(f, {okk,scm_nil}, env, okk, errk)
    end
  },
  ['+'] = function(...)
    local args = table.pack(...)
    if args.n == 0 then
      return 0
    else
      local s = 0
      for i = 1, args.n do
        s = add_rat(s, args[i])
      end
      return s
    end
  end,
  ['*'] = function(...)
    local args = table.pack(...)
    if args.n == 0 then
      return 1
    else
      local s = 1
      for i = 1, args.n do
        s = times_rat(s, args[i])
      end
      return s
    end
  end,
  ['set-car!'] = function(p, x)
    p[1] = x
  end,
  ['set-cdr!'] = function(p, x)
    p[2] = x
  end,
  ['null?'] = function(p) return p == scm_nil or p == nil end,
  load = { [0] = callproc, scm_load },
  ['call/native'] = function(s, ...)
    if symbolp(s) then
      return _G[s[1]](...)
    else
      local o = _G
      repeat
        o = o[s[1][1]]
        s = s[2]
      until not consp(s)
      return o(...)
    end
  end,
  expand = function(x) return x end,
  ['eq?'] = scm_eq,
  cons = cons,
  car = function(p) return p[1] end,
  cdr = function(p) return p[2] end,
  ['pair?'] = consp,
  ['symbol?'] = symbolp,
  ['keyword?'] = function(s)
    return symbolp(s) and s.kw ==  true
  end,
  ['string?'] = function(x) return type(x) == "string" end,
  ['number?'] = function(x)
    return type(x) == "number" or (type(x) == 'table' and x[0] == rational)
  end,
  ['rational?'] = function(x)
    return type(x) == 'table' and x[0] == rational
  end,
  write = function(...)
    local a = table.pack(...)
    for i = 1, a. n do
      scm_print(a[i])
    end
  end,
  gensym = function()
    gensym_counter = gensym_counter + 1
    return mksymbol("#." .. gensym_counter)
  end,
  ['hash-ref'] = function(table, key, default)
    if not table[key] then
      return default or false
    else
      return table[key]
    end
  end,
  ['exact?'] = function(n)
    return (type(n) == 'table' and n[0] == rational)
        or math.type(n) == 'integer'
  end,
  ['inexact?'] = function(n)
    return math.type(n) == 'float'
  end,
--}}}
}
--{{{

local function defproc(name, thnk)
  scm_env[name] = { [0] = callproc, thnk }
end

defproc('apply', function(ok, err, env, fun, args)
  return apply_dispatch(fun, {[0]=eval_args,args}, env, ok, err)
end)

defproc('procedure?', function(ok, err, env, f)
  if type(f) == 'table' and f[0] == eval then
    return ok(true)
  elseif type(f) == 'table' and f[0] == callproc then
    return ok(true)
  elseif type(f) == 'function' then
    return ok(true)
  else
    return ok(false)
  end
end)

defproc('error', function(ok, err, env, ...)
  return throw(err, ...)
end)

defproc('cons', function(ok, err, env, car, cdr)
  if car == nil then
    return throw(err, "pair must have a car")
  end
  if cdr == nil then
    return throw(err, "pair must have a cdr")
  end
  return ok{car,cdr}
end)

defproc('eval', function(ok, err, env, exp)
  return eval(exp, env, ok, err)
end)

defproc('defined?', function(ok, err, env, exp)
  if not symbolp(exp) then
    return throw(err, 'not a symbol', exp)
  end
  return ok(find(env, exp[1]) ~= nil)
end)

defproc('exit', function() end)

defproc('-', function(ok, err, env, ...)
  assert(err)
  local a = table.pack(...)
  if a.n == 0 then
    return throw(err, 'insufficient arguments for -')
  elseif a.n == 1 then
    return minus_rat(0, a[1])
  else
    local x = a[1]
    for i = 2, #a do
      x = minus_rat(x, a[i])
    end
    return ok(x)
  end
end)

defproc('/', function(ok, err, env, ...)
  local a = table.pack(...)
  if a.n == 0 then
    return throw(err, 'insufficient arguments for /')
  elseif a.n == 1 then
    return over_rat(1, a[1])
  else
    local x = a[1]
    for i = 2, #a do
      x = over_rat(x, a[i])
    end
    return ok(x)
  end
end)

defproc('values', function(ok, err, env, ...)
  return ok(...)
end)

defproc('call-with-values', function(ok, err, env, producer, consumer)
  local function listify(car, ...)
    if car ~= nil then
      return {{_quote, {car, scm_nil}}, listify(...)}
    else
      return scm_nil
    end
  end
  return apply_dispatch(producer, scm_nil, env, function(...)
    print(...)
    local l = listify(...)
    return apply_dispatch(consumer, l, env, ok, err)
  end, err)
end)

defproc('call-with-prompt', function(okk, errk, env, tag, thunk, handler)
  local v = apply_dispatch(thunk, scm_nil, env, function(x)
    return {okk, x}
  end, function(e)
    if e[0] == 'prompt' and scm_eq(e.tag, tag) then
      return apply_dispatch(handler, {e.cont, {{_quote,{e.val,scm_nil}}, scm_nil}}, env, function(x)
        return {okk, x}
      end, function(e)
        return {errk, x}
      end)
    else
      return {errk, e}
    end
  end)
  assert((v[1] == okk or v[1] == errk) and v[2])
  if consp(v[2]) and v[2][1] == v[1] then
    return v[2][1](v[2][2])
  end
  return v[1](v[2])
end)

defproc('abort-to-prompt', function(ok, err, env, tag, val)
  return err({ [0] = 'prompt', cont = ok, tag = tag, val = val })
end)

defproc('catch', function(ok, err, env, thunk, handler)
  return apply_dispatch(thunk, scm_nil, env, ok, function(x)
    return apply_dispatch(handler, quote(x), env, ok, err)
  end)
end)

defproc('with-input-from-file', function(ok, err, env, p, f)
  local h, i = io.open(p, 'r'), input_file
  if h then
    input_file = h
    return apply_dispatch(f, scm_nil, env, function(r)
      input_file = i
      h:close()
      return ok(r)
    end, function(e)
      input_file = i
      h:close()
      return err(e)
    end)
  else
    return throw(err, 'failed to open', p, 'for reading')
  end
end)

local function hash_for_each(ok, err, env, hash, func, k1, n)
  local k, v = next(hash, k1)
  local n = n or 0
  if k == nil then
    return ok(n)
  else
    local args = {{_quote, {k, scm_nil}}, {{_quote, {v, scm_nil}}, scm_nil}}
    return apply_dispatch(func, args, env, function(v)
      return hash_for_each(ok, err, env, hash, func, k, n + 1)
    end)
  end
end

defproc('hash-for-each', hash_for_each)

local function repl()
  if term then
    term.setCursorBlink(true)
  end
  write('> ')
  local c, e = pcall(read_sexpr)
  if e == scm_eof then
    if os.exit then
      os.exit(0)
    else
      error('Goodbye', 0)
    end
  end
  if not c then
    (printError or print)(e)
    return repl()
  else
    if term then
      term.setCursorBlink(false)
    end
    return apply_dispatch(scm_env.expand, quote(e), {scm_env,scm_nil}, function(e)
      return eval(e, {scm_env,scm_nil}, function(x)
        scm_print(x)
        write('\n')
        return repl()
      end, function(x)
        write('scheme error: ')
        if x[0] == 'error' then
          for i = 1, #x do
            scm_print(x[i])
            write(' ')
          end
        else
          scm_print(x)
        end
        write('\n')
        return repl()
      end)
    end, scm_print)
  end
end

if _CC_DEFAULT_SETTINGS then
  scm_env.platform = mksymbol('computercraft')
elseif component then
  scm_env.platform = mksymbol('opencomputers')
elseif jit then
  scm_env.platform = mksymbol('luajit')
else
  scm_env.platform = mksymbol('puc-lua')
end
--}}}

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
scm_env.read  = read_sexpr

local handles = {}

function _G.redirect(p, t)
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

--{{{
dofile 'operators.lua'
--}}}

return scm_load(repl, function(x)
  print('error while scm_loading boot file:')
  if x[0] == 'error' then
    for i = 1, #x do
      scm_print(x[i])
      write(' ')
    end
  end
end, {scm_env,scm_nil}, "boot.ss")
