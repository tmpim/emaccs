_textS45afterS45point = (function(_x ,  _y  )
 return var(_stringS45chop, "string-chop")(var(_hashS45ref, "hash-ref")(var(_lines, "lines"),  var(_y, "y") ),  var(_x, "x") )
 end);
 ignore(_textS45afterS45point)_textS45beforeS45point = (function(_x ,  _y  )
 return var(_substring, "substring")(var(_hashS45ref, "hash-ref")(var(_lines, "lines"),  var(_y, "y") ),  1,  var(_x, "x") )
 end);
 ignore(_textS45beforeS45point)_getS45charS45ev = (function()
 return var(_list, "list")(var(_callS47native, "call/native")({symbol('os'),{symbol('pullEvent'),scm_nil}},  "char" ) )
 end);
 ignore(_getS45charS45ev)_textS45objectS45table = var(_makeS45hashS45table, "make-hash-table")({"w", (function(_x ,  _y  )
 return (function(_S35S461052  )
 return (function(_S35S461053  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S461052, "#.1052") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 return {var(_x, "x"), var(_S43, "+")(_x,  var(_end, "end") )}
 end)()
 end)((_S35S461052)[2] )
 end)((_S35S461052)[1] ) else return var(_S35S461053, "#.1053")() end
 end)((function()
 if var(_S61, "=")(false,  var(_S35S461052, "#.1052") ) then return (function()
 return false
 end)() else return false end
 end) )
 end)(var(_stringS45find, "string-find")(_textS45afterS45point(var(_x, "x"),  var(_y, "y") ),  "^%w+" ) )
 end)},  {"W", (function(_x ,  _y  )
 return (function(_S35S461054  )
 return (function(_S35S461055  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S461054, "#.1054") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 return {var(_x, "x"), var(_S43, "+")(_x,  var(_end, "end") )}
 end)()
 end)((_S35S461054)[2] )
 end)((_S35S461054)[1] ) else return var(_S35S461055, "#.1055")() end
 end)((function()
 if var(_S61, "=")(false,  var(_S35S461054, "#.1054") ) then return (function()
 return false
 end)() else return false end
 end) )
 end)(var(_stringS45find, "string-find")(_textS45afterS45point(var(_x, "x"),  var(_y, "y") ),  "^[^%s]+" ) )
 end)},  {"b", (function(_x ,  _y  )
 return var(_stringS45find, "string-find")(_textS45beforeS45point(var(_x, "x"),  var(_y, "y") ),  "%w+$" )
 end)},  {"B", (function(_x ,  _y  )
 return var(_stringS45find, "string-find")(_textS45beforeS45point(var(_x, "x"),  var(_y, "y") ),  "[^%s]+$" )
 end)},  {"$", (function(_x ,  _y  )
 return {var(_x, "x"), var(_lengthS45ofS45line, "length-of-line")(var(_y, "y") )}
 end)} );
 ignore(_textS45objectS45table)ignore(var(_pushS45macroS33, "push-macro!")(symbol('define-text-object'),  (function(_S35S461060  )
 return var(_apply, "apply")((function(...)
 local _S35S461061 = list(...);
 return (function(_S35S461062  )
 return (function(_S35S461063  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S461062, "#.1062") ) then return (function(_S35S461066  )
 if _caseS45pairS63((_S35S461062)[1] ) then return (function(_name  )
 return (function(_args  )
 return (function(_body  )
 return (function()
 return {symbol('define-text-object'), {var(_name, "name"), {{symbol('lambda'), {var(_args, "args"), var(_body, "body")}}, scm_nil}}}
 end)()
 end)((_S35S461062)[2] )
 end)(((_S35S461062)[1])[2] )
 end)(((_S35S461062)[1])[1] ) else return var(_S35S461066, "#.1066")() end
 end)((function()
 return var(_S35S461063, "#.1063")()
 end) ) else return var(_S35S461063, "#.1063")() end
 end)((function()
 return (function(_S35S461064  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S461062, "#.1062") ) then return (function(_name  )
 return (function(_S35S461065  )
 if _caseS45pairS63((_S35S461062)[2] ) then return (function(_body  )
 if var(_S61, "=")(scm_nil,  ((_S35S461062)[2])[2] ) then return (function()
 return {symbol('hash-set!'), {symbol('text-object-table'), {var(_symbolS45S62string, "symbol->string")(var(_name, "name") ), {symbol('body'), scm_nil}}}}
 end)() else return var(_S35S461065, "#.1065")() end
 end)(((_S35S461062)[2])[1] ) else return var(_S35S461065, "#.1065")() end
 end)((function()
 return var(_S35S461064, "#.1064")()
 end) )
 end)((_S35S461062)[1] ) else return var(_S35S461064, "#.1064")() end
 end)((function()
 return var(_error, "error")("no matching case for ",  var(_S35S461061, "#.1061"),  " in ",  {{{{symbol('name'),symbol('args')},symbol('body')},{{symbol('quasiquote'),{{symbol('define-text-object'),{{symbol('unquote'),{symbol('name'),scm_nil}},{{symbol('lambda'),{{symbol('unquote'),{symbol('args'),scm_nil}},{symbol('unquote'),{symbol('body'),scm_nil}}}},scm_nil}}},scm_nil}},scm_nil}},{{{symbol('name'),{symbol('body'),scm_nil}},{{symbol('quasiquote'),{{symbol('hash-set!'),{symbol('text-object-table'),{{symbol('unquote'),{{symbol('symbol->string'),{symbol('name'),scm_nil}},scm_nil}},{symbol('body'),scm_nil}}}},scm_nil}},scm_nil}},scm_nil}} )
 end) )
 end) )
 end)(var(_S35S461061, "#.1061") )
 end),  var(_S35S461060, "#.1060") )
 end) ))_getS45textS45object = (function(_action ,  _x ,  _y  )
 _getS45charS45ev();
 local _ev;
 _ev = _getS45charS45ev();
 return (function(_S35S461072  )
 return (function(_S35S461073  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S461072, "#.1072") ) then if var(_S61, "=")("char",  (_S35S461072)[1] ) then return (function(_S35S461074  )
 if _caseS45pairS63((_S35S461072)[2] ) then return (function(_c  )
 if _S61(scm_nil,  ((_S35S461072)[2])[2] ) then return (function()
 if _S61(var(_c, "c"),  var(_action, "action") ) then return (function()
 return {1, var(_S43, "+")(1,  var(_lengthS45ofS45line, "length-of-line")(var(_y, "y") ) )}
 end)() else if var(_procedureS63, "procedure?")(var(_hashS45ref, "hash-ref")(_textS45objectS45table,  _c ) ) then return (function()
 return _hashS45ref(_textS45objectS45table,  _c )(var(_x, "x"),  var(_y, "y") )
 end)() else if var(_else, "else") then return (function()
 return false
 end)() else return false end end end
 end)() else return var(_S35S461074, "#.1074")() end
 end)(((_S35S461072)[2])[1] ) else return var(_S35S461074, "#.1074")() end
 end)((function()
 return var(_S35S461073, "#.1073")()
 end) ) else return var(_S35S461073, "#.1073")() end else return _S35S461073() end
 end)((function()
 return false
 end) )
 end)(var(_ev, "ev") )
 end);
 ignore(_getS45textS45object)