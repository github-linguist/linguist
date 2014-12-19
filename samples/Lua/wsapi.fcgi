#!/usr/bin/lua

-- Generic WSAPI FastCGI launcher, extracts application to launch
-- from SCRIPT_FILENAME/PATH_TRANSLATED, each application (defined
-- by its script entry point) gets an isolated Lua VM; sequential
-- requests to the same application go to the same VM

pcall(require,"luarocks.require")

local common = require "wsapi.common"
local fastcgi = require "wsapi.fastcgi"

local ONE_HOUR = 60 * 60
local ONE_DAY = 24 * ONE_HOUR

local wsapi_loader = common.make_loader{
  isolated = true,         -- isolate each script in its own Lua state
  filename = nil,          -- if you want to force the launch of a single script
  launcher = "wsapi.fcgi", -- the name of this script
  reload = false,          -- if you want to reload the application on every request
  period = ONE_HOUR,       -- frequency of Lua state staleness checks
  ttl = ONE_DAY,           -- time-to-live for Lua states
  vars =                   -- order of checking for the path of the script
   { "SCRIPT_FILENAME",
     "PATH_TRANSLATED" } 
}

fastcgi.run(wsapi_loader)
