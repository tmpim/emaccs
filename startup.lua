if not fs.exists'scheme51.lua' then
  error('no scheme boot file found. is your installation complete?')
end

print '> loading saved boot file'
dofile('scheme51.lua')
return _load('/emaccs/startup.ss')
