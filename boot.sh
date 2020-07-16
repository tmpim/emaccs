#!/usr/bin/env bash

code () {
cat <<EOF
(define booting #t)
(load "compiler.ss")
(begin
  (write "local function ignore(x) end" #\\newline)
  (compile-file "boot.ss")
  (compile-file "case.ss")
  (compile-file "compiler.ss"))
EOF
}

scm51=$(mktemp)
booted=$(mktemp)

code \
 | lua $1 \
 | sed -re 's/^(> )?true$//g' \
 | sed -re 's/^>//g' \
 | sed -re 's/\)true$/)/g' > $scm51


head -n -10 startup.lua > $booted

# A cursed fix
echo 'local symbol = mksymbol' >> $booted
cat operators.lua >> $booted
cat $scm51 >> $booted

echo '_platform = "Scheme 51"' >> $booted
echo '_ENV.var = var' >> $booted
echo "_repl(true, 0)" >> $booted
cp $booted ${2:-scheme51.lua}
rm $scm51 $booted
