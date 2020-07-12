boot:
	luajit startup.lua boot > /tmp/scheme51.lua
	@head -n -10 startup.lua > /tmp/booted.lua

	# A cursed fix
	@echo 'local symbol = mksymbol' >> /tmp/booted.lua
	cat operators.lua >> /tmp/booted.lua
	cat /tmp/scheme51.lua >> /tmp/booted.lua

	# Some more cursed fixes
	@echo '_platform = "Scheme 51"' >> /tmp/booted.lua
	@echo '_G.var = var' >> /tmp/booted.lua
	@echo "_repl(true, 0)" >> /tmp/booted.lua
	luamin -f /tmp/booted.lua > scheme51.lua
	rm /tmp/*.lua
