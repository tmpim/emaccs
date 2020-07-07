local input, input_file = {}

local write = write

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

local function read_number(acc)
  local ch = getchar()
  if '0' <= ch and ch <= '9' then
    return read_number(acc .. ch)
  else
    ungetchar(ch)
    return assert(tonumber(acc))
  end
end

local symbol_chars = {
  ['+'] = true, ['-'] = true, ['?'] = true, ['!'] = true, ['='] = true
}

local function symbol_carp(ch)
  return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or symbol_chars[ch]
end

local symbol_table, symbol = {}, {}

local function mksymbol(x)
  if symbol_table[x] then
    return symbol_table[x]
  else
    symbol_table[x] = {[0]=symbol,x}
    return symbol_table[x]
  end
end

_G.symbol_table = symbol_table

local _lambda = mksymbol('lambda')
local _if = mksymbol('if')
local _quote = mksymbol('quote')
local _quasiquote = mksymbol('quasiquote')
local _unquote = mksymbol('unquote')
local _define = mksymbol('define')

local function read_symbol(acc)
  local ch = getchar()
  if symbol_carp(ch) or (ch <= '9' and ch >= '0') or ch == '/' then
    return read_symbol(acc .. ch)
  else
    ungetchar(ch)
    return mksymbol(acc)
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
  if peekp() == chs:sub(i, i) then
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
  else
    return error('unexpected special atom ' .. ch)
  end
end

local scm_nil, scm_eof = {}, {}
local function cons(a,b)
  return {a,b}
end

local function read_sexpr_list()
  local ch = skip_spaces()
  if ch == ')' then
    return scm_nil
  else
    ungetchar(ch)
    return {read_sexpr(), read_sexpr_list()}
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
    else
      error("unknown escape sequence " .. ch)
    end
  elseif ch == '"' then
    return acc
  else
    return read_string(acc .. ch)
  end
end

function read_sexpr()
  local ch = skip_spaces()
  if not ch then
    return scm_eof
  elseif ch == '(' then
    return read_sexpr_list()
  elseif ch == '\'' then
    return {_quote, {read_sexpr(), scm_nil}}
  elseif ch == '`' then
    return {_quasiquote, {read_sexpr(), scm_nil}}
  elseif ch == ',' then
    return {_unquote, {read_sexpr(), scm_nil}}
  elseif ch <= '9' and ch >= '0' then
    return read_number(ch)
  elseif ch == '#' then
    return read_special_atom()
  elseif symbol_carp(ch) then
    return read_symbol(ch)
  elseif ch == '"' then
    return read_string("")
  else
    return error("lexical error at character:" .. ch)
  end
end

local function symbolp(e)
  return type(e) == 'table' and e[0] == symbol
end

local function consp(e)
  return type(e) == 'table' and #e == 2
end

local eval, apply, callproc, eval_args, apply_dispatch

local function scm_print(e)
  if type(e) == 'table' then
    if symbolp(e) then
      write(e[1])
    elseif e[0] == eval then
      write('<closure>')
    elseif consp(e) then
      write '('
      repeat
        scm_print(e[1])
        if consp(e[2]) then
          write ' '
        end
        e = e[2]
      until not consp(e)
      write ')'
    elseif e == scm_nil then
      write ('\'()')
    elseif e[0] == callproc then
      write '<procedure>'
    elseif e == scm_eof then
      return '#eof'
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
      or (type(e) == 'table' and e[0] == eval)
      or e == scm_nil
      or e == scm_eof
      or e == nil
end

function eval(expr, env, okk, errk)
  assert(errk)
  if symbolp(expr) and expr[1] ~= '' then
    if env[expr[1]] ~= nil then
      return okk(env[expr[1]])
    else
      return errk('no binding for symbol ' .. expr[1])
    end
  elseif consp(expr) and expr[1] == _lambda then
    if not consp(expr[2][2]) then
      return errk("not a valid lambda expression")
    end
    return okk({[0]=eval,expr[2][1],env,expr[2][2]})
  elseif consp(expr) and expr[1] == _if then
    return eval(expr[2][1], env, function(c)
      if c ~= false and c ~= scm_nil then
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
        env[expr[2][1][1]] = x
        return okk(x)
      end, errk)
    elseif consp(expr[2]) and consp(expr[2][1]) and consp(expr[2][2]) then
      return eval({ _define, { expr[2][1][1], { { _lambda, { expr[2][1][2], expr[2][2] } }, scm_nil } } }, env, okk, errk)
    else
      return errk('invalid define expression')
    end
  elseif consp(expr) and expr[1] == _quote then
    return okk(expr[2][1])
  elseif consp(expr) then
    return eval(expr[1], env, function(f)
      return apply_dispatch(f, expr[2], env, okk, errk)
    end, function(e)
      scm_print(expr)
      print()
      return errk(e)
    end)
  elseif is_self_eval(expr) then
    return okk(expr)
  else
    print("don't know how to evaluate form:")
    scm_print(expr)
    print()
    return errk("aborting")
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
    local fa, fb, fe = fun[1], fun[3], setmetatable(copy(fun[2]), { __index = env })
    local fenv = make_env(fa, args, {})
    setmetatable(fenv, { __index = fe })
    local function eval_body(b, acc)
      if b == scm_nil then
        return okk(acc or false)
      else
        return eval(b[1], fenv, function(x)
          return eval_body(b[2], x)
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
      return fun[1](okk, errk, env, unpack(t))
    else
      local ok, err = pcall(fun, unpack(t))
      if not ok then
        return errk(err)
      else
        return okk(err)
      end
    end
  end, errk)
end

function apply_dispatch(f, args, env, okk, errk)
  if type(f) == 'table' and f[0] == eval then
    return apply(f, args, env, okk, errk)
  elseif type(f) == 'table' and f[0] == callproc then
    return callproc(f, args, env, okk, errk, true)
  elseif type(f) == 'function' then
    return callproc(f, args, env, okk, errk)
  else
    return errk('can not apply non-functional object of type ' .. type(f))
  end
end

function eval_args(args, env, okk, errk)
  if args == scm_nil then
    return okk(scm_nil)
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

local function load(okk, errk, env, path)
  if not path then return errk('no file path specified') end
  local h = io.open(path, 'r')
  if not h then
    return errk("failed to open file " .. path .. " for reading")
  else
    input_file = h
    local function load_loop(i)
      local ok, err = pcall(read_sexpr)
      if ok and err ~= scm_eof then
        input_file = nil
        return apply_dispatch(env.expand, quote(err), env, function(expr)
          return eval(expr, env, function(value)
            input_file = h
            return load_loop(i + 1)
          end, errk)
        end, function(e)
          return errk(e .. ' in the ' .. tostring(i))
        end)
      elseif err == scm_eof then
        input_file = nil
        h:close()
        return okk(i)
      else
        input_file = nil
        h:close()
        return errk(err)
      end
    end
    return load_loop(0)
  end
end

local function scm_eq(a, b)
  if a == b then
    return true
  elseif consp(a) and consp(b) then
      return scm_eq(a[1]) and scm_eq(a[2])
  elseif type(a) == 'table' and (a[0] == eval or a[0] == callproc) then
    return false
  end
  return false
end

local scm_env = {
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
        s = s + args[i]
      end
      return s
    end
  end,
  ['exit'] = {
    [0] = callproc,
    function(okk, errk, env)
      print 'exiting'
    end
  },
  ['set-car!'] = function(p, x)
    p[1] = x
  end,
  ['set-cdr!'] = function(p, x)
    p[2] = x
  end,
  ['null?'] = function(p) return p == scm_nil end,
  load = { [0] = callproc, load },
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
  write = scm_print
}

local function repl()
  if term then
    term.setCursorBlink(true)
  end
  write('> ')
  local c, e = pcall(read_sexpr)
  if not c then
    (printError or print)(e)
    return repl()
  else
    if term then
      term.setCursorBlink(false)
    end
    return apply_dispatch(scm_env.expand, quote(e), scm_env, function(e)
      return eval(e, scm_env, function(x)
        scm_print(x)
        write('\n')
        return repl()
      end, function(x)
        write('scheme error: ')
        scm_print(x)
        write('\n')
        return repl()
      end)
    end, print)
  end
end

return load(repl, function(x)
  print('error while loading boot file:')
  scm_print(x)
end, scm_env, "boot.ss")
