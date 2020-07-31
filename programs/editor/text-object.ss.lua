_textS45afterS45point = (function(_x ,  _y  )
 return var(_stringS45chop, "string-chop")(var(_hashS45ref, "hash-ref")(var(_lines, "lines"),  var(_y, "y") ),  var(_x, "x") )
 end);
 ignore(_textS45afterS45point)_getS45charS45ev = (function()
 return var(_list, "list")(var(_callS47native, "call/native")({symbol('os'),{symbol('pullEvent'),scm_nil}},  "char" ) )
 end);
 ignore(_getS45charS45ev)_wordS45afterS45point = (function(_x ,  _y  )
 return (function(_S35S46310  )
 return (function(_S35S46311  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46310, "#.310") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 return {var(_S43, "+")(var(_x, "x"),  var(_start, "start") ), _S43(_x,  var(_end, "end") )}
 end)()
 end)((_S35S46310)[2] )
 end)((_S35S46310)[1] ) else return var(_S35S46311, "#.311")() end
 end)((function()
 if var(_S61, "=")(false,  var(_S35S46310, "#.310") ) then return (function()
 return false
 end)() else return false end
 end) )
 end)(var(_stringS45find, "string-find")(_textS45afterS45point(var(_x, "x"),  var(_y, "y") ),  "^%w+" ) )
 end);
 ignore(_wordS45afterS45point)_WORDS45afterS45point = (function(_x ,  _y  )
 return (function(_S35S46314  )
 return (function(_S35S46315  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46314, "#.314") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 return {var(_S43, "+")(var(_x, "x"),  var(_start, "start") ), _S43(_x,  var(_end, "end") )}
 end)()
 end)((_S35S46314)[2] )
 end)((_S35S46314)[1] ) else return var(_S35S46315, "#.315")() end
 end)((function()
 if var(_S61, "=")(false,  var(_S35S46314, "#.314") ) then return (function()
 return false
 end)() else return false end
 end) )
 end)(var(_stringS45find, "string-find")(_textS45afterS45point(var(_x, "x"),  var(_y, "y") ),  "^[^%s]+" ) )
 end);
 ignore(_WORDS45afterS45point)_getS45textS45object = (function(_x ,  _y  )
 _getS45charS45ev();
 local _ev;
 _ev = _getS45charS45ev();
 return (function(_S35S46318  )
 return (function(_S35S46319  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46318, "#.318") ) then if var(_S61, "=")("char",  (_S35S46318)[1] ) then return (function(_S35S46326  )
 if _caseS45pairS63((_S35S46318)[2] ) then if _S61("w",  ((_S35S46318)[2])[1] ) then if _S61(scm_nil,  ((_S35S46318)[2])[2] ) then return (function()
 return _wordS45afterS45point(var(_x, "x"),  var(_y, "y") )
 end)() else return var(_S35S46326, "#.326")() end else return _S35S46326() end else return _S35S46326() end
 end)((function()
 return var(_S35S46319, "#.319")()
 end) ) else return var(_S35S46319, "#.319")() end else return _S35S46319() end
 end)((function()
 return (function(_S35S46320  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46318, "#.318") ) then if var(_S61, "=")("char",  (_S35S46318)[1] ) then return (function(_S35S46325  )
 if _caseS45pairS63((_S35S46318)[2] ) then if _S61("W",  ((_S35S46318)[2])[1] ) then if _S61(scm_nil,  ((_S35S46318)[2])[2] ) then return (function()
 return _WORDS45afterS45point(var(_x, "x"),  var(_y, "y") )
 end)() else return var(_S35S46325, "#.325")() end else return _S35S46325() end else return _S35S46325() end
 end)((function()
 return var(_S35S46320, "#.320")()
 end) ) else return var(_S35S46320, "#.320")() end else return _S35S46320() end
 end)((function()
 return (function(_S35S46321  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46318, "#.318") ) then if var(_S61, "=")("char",  (_S35S46318)[1] ) then return (function(_S35S46324  )
 if _caseS45pairS63((_S35S46318)[2] ) then if _S61("$",  ((_S35S46318)[2])[1] ) then if _S61(scm_nil,  ((_S35S46318)[2])[2] ) then return (function()
 return {var(_x, "x"), var(_S43, "+")(1,  var(_stringS45length, "string-length")(var(_hashS45ref, "hash-ref")(var(_lines, "lines"),  var(_y, "y"),  "" ) ) )}
 end)() else return var(_S35S46324, "#.324")() end else return _S35S46324() end else return _S35S46324() end
 end)((function()
 return var(_S35S46321, "#.321")()
 end) ) else return var(_S35S46321, "#.321")() end else return _S35S46321() end
 end)((function()
 return (function(_S35S46322  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46318, "#.318") ) then if var(_S61, "=")("char",  (_S35S46318)[1] ) then return (function(_S35S46323  )
 if _caseS45pairS63((_S35S46318)[2] ) then return (function(_c  )
 if _S61(scm_nil,  ((_S35S46318)[2])[2] ) then return (function()
 return false
 end)() else return var(_S35S46323, "#.323")() end
 end)(((_S35S46318)[2])[1] ) else return var(_S35S46323, "#.323")() end
 end)((function()
 return var(_S35S46322, "#.322")()
 end) ) else return var(_S35S46322, "#.322")() end else return _S35S46322() end
 end)((function()
 return false
 end) )
 end) )
 end) )
 end) )
 end)(var(_ev, "ev") )
 end);
 ignore(_getS45textS45object)