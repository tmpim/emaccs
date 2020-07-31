ignore(var(_bindS45forS45mode, "bind-for-mode")(symbol('up'),  (function(_x ,  _y  )
 if var(_S62, ">")(var(_y, "y"),  1 ) then return var(_setS45cursorS33, "set-cursor!")(var(_min, "min")(var(_x, "x"),  var(_S43, "+")(1,  var(_lengthS45ofS45line, "length-of-line")(var(_S45, "-")(_y,  1 ) ) ) ),  _S45(_y,  1 ) ) else return false end
 end) ))ignore(_bindS45forS45mode(symbol('down'),  (function(_x ,  _y  )
 if var(_S60, "<")(var(_y, "y"),  var(_lineS45count, "line-count")() ) then return var(_setS45cursorS33, "set-cursor!")(var(_min, "min")(var(_x, "x"),  var(_S43, "+")(1,  var(_lengthS45ofS45line, "length-of-line")(_S43(_y,  1 ) ) ) ),  _S43(_y,  1 ) ) else return false end
 end) ))ignore(_bindS45forS45mode(symbol('left'),  (function(_x ,  _y  )
 return var(_setS45cursorS33, "set-cursor!")(var(_max, "max")(1,  var(_S45, "-")(var(_x, "x"),  1 ) ),  var(_y, "y") )
 end) ))ignore(_bindS45forS45mode(symbol('right'),  (function(_x ,  _y  )
 return var(_setS45cursorS33, "set-cursor!")(var(_min, "min")(var(_S43, "+")(1,  var(_lengthS45ofS45line, "length-of-line")(var(_y, "y") ) ),  _S43(var(_x, "x"),  1 ) ),  _y )
 end) ))ignore(_bindS45forS45mode(symbol('home'),  (function(_x ,  _y  )
 return var(_setS45cursorS33, "set-cursor!")(1,  var(_y, "y") )
 end) ))ignore(_bindS45forS45mode(symbol('end'),  (function(_x ,  _y  )
 return var(_setS45cursorS33, "set-cursor!")(var(_lengthS45ofS45line, "length-of-line")(var(_y, "y") ),  _y )
 end) ))ignore(_bindS45forS45mode(symbol('pageUp'),  (function(_x ,  _y  )
 local _jumpS45y;
 _jumpS45y = var(_max, "max")(1,  var(_S45, "-")(var(_y, "y"),  var(_inexact, "inexact")(var(_S47, "/")(var(_height, "height")(),  2 ) ) ) );
 return var(_setS45cursorS33, "set-cursor!")(var(_min, "min")(var(_x, "x"),  var(_S43, "+")(1,  var(_lengthS45ofS45line, "length-of-line")(var(_jumpS45y, "jump-y") ) ) ),  _jumpS45y )
 end) ))ignore(_bindS45forS45mode(symbol('pageDown'),  (function(_x ,  _y  )
 local _jumpS45y;
 _jumpS45y = var(_max, "max")(1,  var(_S43, "+")(var(_y, "y"),  var(_inexact, "inexact")(var(_S47, "/")(var(_height, "height")(),  2 ) ) ) );
 return var(_setS45cursorS33, "set-cursor!")(var(_min, "min")(var(_x, "x"),  _S43(1,  var(_lengthS45ofS45line, "length-of-line")(var(_jumpS45y, "jump-y") ) ) ),  _jumpS45y )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  symbol('i'),  (function()
 return var(_currentS45mode, "current-mode")(symbol('insert') )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  symbol('a'),  (function(_x ,  _y  )
 var(_setS45cursorS33, "set-cursor!")(var(_min, "min")(var(_S43, "+")(1,  var(_lengthS45ofS45line, "length-of-line")(var(_y, "y") ) ),  _S43(var(_x, "x"),  1 ) ),  _y );
 return var(_currentS45mode, "current-mode")(symbol('insert') )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  symbol('o'),  (function(_x ,  _y  )
 var(_callS47native, "call/native")({symbol('table'),{symbol('insert'),scm_nil}},  var(_lines, "lines"),  var(_S43, "+")(var(_y, "y"),  1 ),  "" );
 var(_setS45cursorS33, "set-cursor!")(1,  _S43(_y,  1 ) );
 return var(_currentS45mode, "current-mode")(symbol('insert') )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  4,  symbol('o'),  (function(_x ,  _y  )
 var(_callS47native, "call/native")({symbol('table'),{symbol('insert'),scm_nil}},  var(_lines, "lines"),  var(_y, "y"),  "" );
 var(_setS45cursorS33, "set-cursor!")(1,  _y );
 var(_redrawS45text, "redraw-text")();
 return var(_currentS45mode, "current-mode")(symbol('insert') )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  4,  symbol('c'),  (function(_x ,  _y  )
 var(_hashS45setS33, "hash-set!")(var(_lines, "lines"),  var(_y, "y"),  var(_substring, "substring")(var(_hashS45ref, "hash-ref")(_lines,  _y ),  1,  var(_S45, "-")(var(_x, "x"),  1 ) ) );
 var(_redrawS45line, "redraw-line")(_y );
 return var(_currentS45mode, "current-mode")(symbol('insert') )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  2,  symbol('c'),  (function(_x ,  _y  )
 return var(_statusS45barS45message, "status-bar-message")("Use M-x (exit) to exit the editor." )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  symbol('c'),  (function(_x ,  _y  )
 return (function(_S35S46386  )
 return (function(_S35S46387  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46386, "#.386") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 var(_hashS45setS33, "hash-set!")(var(_lines, "lines"),  var(_y, "y"),  var(_stringS45append, "string-append")(var(_substring, "substring")(var(_hashS45ref, "hash-ref")(_lines,  _y ),  1,  var(_S45, "-")(var(_x, "x"),  1 ) ),  var(_stringS45chop, "string-chop")(_hashS45ref(_lines,  _y ),  var(_end, "end") ) ) );
 var(_redrawS45line, "redraw-line")(_y );
 return var(_currentS45mode, "current-mode")(symbol('insert') )
 end)()
 end)((_S35S46386)[2] )
 end)((_S35S46386)[1] ) else return var(_S35S46387, "#.387")() end
 end)((function()
 if var(_S61, "=")(false,  var(_S35S46386, "#.386") ) then return (function()
 return false
 end)() else return false end
 end) )
 end)(var(_getS45textS45object, "get-text-object")(var(_x, "x"),  var(_y, "y") ) )
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  symbol('d'),  (function(_x ,  _y  )
 return (function(_S35S46390  )
 return (function(_S35S46391  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46390, "#.390") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 var(_hashS45setS33, "hash-set!")(var(_lines, "lines"),  var(_y, "y"),  var(_stringS45append, "string-append")(var(_substring, "substring")(var(_hashS45ref, "hash-ref")(_lines,  _y ),  1,  var(_S45, "-")(var(_x, "x"),  1 ) ),  var(_stringS45chop, "string-chop")(_hashS45ref(_lines,  _y ),  var(_end, "end") ) ) );
 return var(_redrawS45line, "redraw-line")(_y )
 end)()
 end)((_S35S46390)[2] )
 end)((_S35S46390)[1] ) else return var(_S35S46391, "#.391")() end
 end)((function()
 if var(_S61, "=")(false,  var(_S35S46390, "#.390") ) then return (function()
 return false
 end)() else return false end
 end) )
 end)(var(_getS45textS45object, "get-text-object")(var(_x, "x"),  var(_y, "y") ) )
 end) ))ignore(_bindS45forS45mode(symbol('insert'),  2,  symbol('c'),  (function()
 var(_currentS45mode, "current-mode")(symbol('normal') );
 return true
 end) ))ignore(_bindS45forS45mode(symbol('insert'),  2,  symbol('w'),  (function(_x ,  _y  )
 return (function(_front ,  _back  )
 return (function(_S35S46394  )
 return (function(_S35S46395  )
 if var(_caseS45pairS63, "case-pair?")(var(_S35S46394, "#.394") ) then return (function(_start  )
 return (function(_end  )
 return (function()
 var(_hashS45setS33, "hash-set!")(var(_lines, "lines"),  var(_y, "y"),  var(_stringS45append, "string-append")(var(_substring, "substring")(var(_front, "front"),  0,  var(_S45, "-")(var(_start, "start"),  1 ) ),  var(_back, "back") ) );
 return var(_setS45cursorS33, "set-cursor!")(_start,  _y )
 end)()
 end)((_S35S46394)[2] )
 end)((_S35S46394)[1] ) else return var(_S35S46395, "#.395")() end
 end)((function()
 return (function(_x  )
 return (function()
 return false
 end)()
 end)(var(_S35S46394, "#.394") )
 end) )
 end)(var(_stringS45find, "string-find")(var(_front, "front"),  "%w+%s*$" ) )
 end)(var(_substring, "substring")(var(_hashS45ref, "hash-ref")(var(_lines, "lines"),  var(_y, "y") ),  1,  var(_S45, "-")(var(_x, "x"),  1 ) ),  var(_stringS45chop, "string-chop")(_hashS45ref(_lines,  _y ),  _x ) )
 end) ))ignore(_bindS45forS45mode(symbol('insert'),  symbol('enter'),  (function(_x ,  _y  )
 local _line;
 _line = var(_hashS45ref, "hash-ref")(var(_lines, "lines"),  var(_y, "y") );
 var(_hashS45setS33, "hash-set!")(_lines,  _y,  var(_substring, "substring")(var(_line, "line"),  1,  var(_S45, "-")(var(_x, "x"),  1 ) ) );
 var(_callS47native, "call/native")({symbol('table'),{symbol('insert'),scm_nil}},  _lines,  var(_S43, "+")(_y,  1 ),  var(_stringS45chop, "string-chop")(_line,  _x ) );
 var(_setS45cursorS33, "set-cursor!")(1,  _S43(_y,  1 ) );
 return var(_redrawS45text, "redraw-text")()
 end) ))ignore(_bindS45forS45mode(symbol('insert'),  symbol('backspace'),  (function(_x ,  _y  )
 if var(_S62, ">")(var(_x, "x"),  1 ) then return (function()
 var(_hashS45setS33, "hash-set!")(var(_lines, "lines"),  var(_y, "y"),  var(_stringS45append, "string-append")(var(_substring, "substring")(var(_hashS45ref, "hash-ref")(_lines,  _y ),  1,  var(_S45, "-")(_x,  2 ) ),  var(_stringS45chop, "string-chop")(_hashS45ref(_lines,  _y ),  _x ) ) );
 return var(_setS45cursorS33, "set-cursor!")(var(_max, "max")(1,  _S45(_x,  1 ) ),  _y )
 end)() else if _S62(var(_y, "y"),  1 ) then return (function()
 local _prevlen;
 _prevlen = var(_lengthS45ofS45line, "length-of-line")(var(_S45, "-")(_y,  1 ) );
 var(_hashS45setS33, "hash-set!")(var(_lines, "lines"),  _S45(_y,  1 ),  var(_stringS45append, "string-append")(var(_hashS45ref, "hash-ref")(_lines,  _S45(_y,  1 ) ),  _hashS45ref(_lines,  _y ) ) );
 var(_callS47native, "call/native")({symbol('table'),{symbol('remove'),scm_nil}},  _lines,  _y );
 var(_setS45cursorS33, "set-cursor!")(var(_S43, "+")(1,  var(_prevlen, "prevlen") ),  _S45(_y,  1 ) );
 return var(_redrawS45text, "redraw-text")()
 end)() else return false end end
 end) ))ignore(_bindS45forS45mode(symbol('normal'),  1,  symbol('x'),  (function(_x ,  _y  )
 var(_callS47native, "call/native")({symbol('os'),{symbol('pullEvent'),scm_nil}},  "char" );
 local _cmd;
 _cmd = var(_withS45inputS45fromS45string, "with-input-from-string")(var(_stringS45append, "string-append")(var(_promptS45forS45input, "prompt-for-input")("M-x " ),  " " ),  var(_read, "read") );
 var(_catch, "catch")((function()
 return var(_statusS45barS45message, "status-bar-message")(var(_eval, "eval")(var(_cmd, "cmd") ) )
 end),  (function(_e  )
 return var(_statusS45barS45message, "status-bar-message")(var(_e, "e") )
 end) );
 return var(_redrawS45text, "redraw-text")()
 end) ))