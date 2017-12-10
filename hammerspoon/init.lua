-- hammerspoon init.lua
-- NOTE: When you reload your configuration you keep all the old "watchers" running!
local grid = require("hs.grid")
local window = require("hs.window")
local hotkey = require("hs.hotkey")
local alert = require("hs.alert")
local layout = require("hs.layout")
local utils = require("utils")


local HOME = os.getenv("HOME")
local sleepScript = HOME .. "/.sleep"
local wakeupScript = HOME .. "/.wakeup"

-- Audio Configuration
local AUDIO_EXTERNAL_NAME = "FiiO USB DAC-E10"
local AUDIO_INTERNAL_NAME = "Built-in Output"

-- Screens Configuration
local BUILTIN_MONITOR = "Color LCD"
local EXTERNAL_MONITOR = "Thunderbolt Display"


local function hasExternalMonitor()
   if hs.screen.find(EXTERNAL_MONITOR) then
      return true
   end
   return false
end

-- Grid configuration
grid.setGrid("4x2", BUILTIN_MONITOR)
if hasExternalMonitor() then
   grid.setGrid("4x2`", EXTERNAL_MONITOR)
end

-- credo serva anche questo:
grid.setMargins("0x0")

-- no animation pls
window.animationDuration = 0

local mash = {"cmd", "alt", "ctrl"}
-- local mod1 = {"alt", "shift"}
-- local mod2 = {"ctrl", "option", "shift"}
-- local mod3 = {"cmd", "ctrl"}
local mod4 = {"cmd", "shift"} -- usa lo shift di destra, e' piu' comodo!

local function centerpoint()
   local current = grid.getGrid()
   return { x = 1, y = 0, w = current.w / 2, h = current.h / 2 }
end

local function cutLeft(width)
   return { x = 0, y = 0, w = width, h = grid.getGrid().h }
end

local function cutRight(width)
   local current = grid.getGrid()
   return { x = current.w - width, y = 0, w = width, h = current.h }
end

local bindings = {
   ["1"] = function() grid.set(window.focusedWindow(), cutLeft(1), window.focusedWindow():screen()) alert.show("1") end,
   ["2"] = function() grid.set(window.focusedWindow(), cutLeft(2), window.focusedWindow():screen()) alert.show("2") end,
   ["3"] = function() grid.set(window.focusedWindow(), cutLeft(3), window.focusedWindow():screen()) alert.show("3") end,
   ["9"] = function() grid.set(window.focusedWindow(), cutRight(3), window.focusedWindow():screen()) alert.show("9") end,
   ["0"] = function() grid.set(window.focusedWindow(), cutRight(2), window.focusedWindow():screen()) alert.show("0") end,
   ["-"] = function() grid.set(window.focusedWindow(), cutRight(1), window.focusedWindow():screen()) alert.show("-") end,
   ["m"] = function() window.focusedWindow():maximize() end,
   ["space"] = function() hs.spotify.displayCurrentTrack() end,
   ["delete"] = function() hs.caffeinate.startScreensaver() end,
   ["h"] = function() grid.resizeWindowThinner() end,
   ["l"] = function() grid.resizeWindowWider() end,
   ["j"] = function() grid.resizeWindowTaller() end,
   ["k"] = function() grid.resizeWindowShorter() end,
   ["up"] = function() grid.pushWindowUp() end,
   ["down"] = function() grid.pushWindowDown() end,
   ["left"] = function() grid.pushWindowLeft() end,
   ["right"] = function() grid.pushWindowRight() end,
   [";"] = function() grid.snap(window.focusedWindow()) end,
   ["'"] = function() hs.fnutils.map(window.visibleWindows(), grid.snap) end,
}

for key, func in pairs(bindings) do
   hotkey.bind(mash, key, func)
end


hotkey.bind(mod4, "g", function() grid.show() end)

-- Window layouts
local layouts = {
   work = {
      { "Spotify", nil, BUILTIN_MONITOR, layout.maximized, nil, nil },
      { "Emacs", nil, EXTERNAL_MONITOR, layout.maximized, nil, nil },
      { "Google Chrome", nil, EXTERNAL_MONITOR, layout.maximized, nil, nil },
      { "iTerm2", nil, EXTERNAL_MONITOR, layout.maximized, nil, nil },
   },
   home = {
      { "Spotify", nil, BUILTIN_MONITOR, layout.maximized, nil, nil },
      { "Emacs", nil, BUILTIN_MONITOR, layout.maximized, nil, nil },
      { "Google Chrome", nil, BUILTIN_MONITOR, layout.maximized, nil, nil },
      { "iTerm2", nil, BUILTIN_MONITOR, layout.maximized, nil, nil },
   }
}

hotkey.bind(mod4, "1", function() if hasExternalMonitor() then layout.apply(layouts["work"]) end end)
hotkey.bind(mod4, "2", function() layout.apply(layouts["home"]) end)

-- WiFi Watcher --
------------------
local wifi_work_ssid = "zendesk"
local wifi_last_ssid = hs.wifi.currentNetwork()

local wifi_watcher = hs.wifi.watcher.new(function ()
   local wifi_new_ssid = hs.wifi.currentNetwork()

   if wifi_new_ssid == wifi_work_ssid and wifi_last_ssid ~= wifi_work_ssid then
      hs.audiodevice.defaultOutputDevice():setVolume(1)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send()
   end

   wifi_last_ssid = wifi_new_ssid
end)
wifi_watcher:start()

--- caffeine mode
require("caffeine")

--- Sleep watcher ---
--------------------
-- Can execute ~/.sleep and ~/.wakeup after sleep or wakeup events; note that those files
-- must be shell scripts with the executable bit set
local sleepWatcher = hs.caffeinate.watcher.new(function (eventType)
      if (eventType == hs.caffeinate.watcher.systemDidWake) then
         if utils.ken_lee(wakeupScript) then
            hs.notify.new({title="Hammerspoon", informativeText="Running wakeup script"}):send()
            hs.execute(wakeupScript)
         end
      elseif (eventType == hs.caffeinate.watcher.systemWillSleep) then
         if utils.ken_lee(sleepScript) then
            hs.notify.new({title="Hammerspoon", informativeText="Running sleep script"}):send()
            hs.execute(sleepScript)
         end
      end
end)
sleepWatcher:start()


-- Monitor watcher, used to toggle the dynamic profile in iTerm2 --
-------------------------------------------------------------------

-- Switch dynamic profile in iTerm2
-- Valid values: iTerm2_Dynamic_12.json, iTerm2_Dynamic_11.json
local function setIterm2Profile(filename)
   hs.execute("ln -sf $HOME/Preferences/iTerm2/" .. filename .. " \"$HOME/Library/Application Support/iTerm2/DynamicProfiles/iTerm2_Dynamic.json\"")
end

-- Switch font in emacs
local function tellEmacsToChangeFont()
   hs.execute("/usr/local/bin/emacsclient -e '(set-the-right-font)'")
end

local function displayWatcherCallback()
   -- Change the font in Emacs every time there's a change with the monitor(s).
   tellEmacsToChangeFont()

   if hasExternalMonitor() then
      -- Set the grid on the external monitor
      grid.setGrid("8x6", EXTERNAL_MONITOR)

      setIterm2Profile("iTerm2_Dynamic_12.json")
   else
      setIterm2Profile("iTerm2_Dynamic_11.json")
   end
end

local monitorWatcher = hs.screen.watcher.new(displayWatcherCallback)
monitorWatcher:start()


-- Audio Device toggle --
-------------------------
local audioToggleMenu = hs.menubar.new()
audioToggleMenu:setTooltip("Toggle the default audio output device")

-- Set the menubar icon corresponding to the default output device
local function setToggleAudioIcon()
   local device = hs.audiodevice.defaultOutputDevice()
   if device:name() == AUDIO_INTERNAL_NAME then
      audioToggleMenu:setTitle("🔈")
   else
      audioToggleMenu:setTitle("🎧")
   end
end

-- Toggle the default sound output bewteen built-in and DAC
local function toggleAudioDevice(modifier)
   local currOutput = hs.audiodevice.defaultOutputDevice()
   local builtinOut = hs.audiodevice.findOutputByName(AUDIO_INTERNAL_NAME)
   local fioOutput = hs.audiodevice.findOutputByName(AUDIO_EXTERNAL_NAME)

   if not builtinOut or not fioOutput then
      alert.show("Cannot find output devices, or FIO is not plugged in")
      setToggleAudioIcon()
      return
   end

   if currOutput:name() == fioOutput:name() then
      builtinOut:setDefaultOutputDevice()
   else
      fioOutput:setDefaultOutputDevice()
   end

   hs.notify.new({title="Hammerspoon", informativeText="New default audio device: " .. hs.audiodevice.defaultOutputDevice():name()}):send()

   setToggleAudioIcon()
end
audioToggleMenu:setClickCallback(toggleAudioDevice)

-- Set up a watcher to update the menubar icon when the FIO gets unplugged
hs.audiodevice.watcher.setCallback(function(event)
      setToggleAudioIcon(audioToggleMenu)
end)
hs.audiodevice.watcher.start()

-- Set the initial menubar icon
setToggleAudioIcon()

--- external modules
-- require("weather")
-- require("music_menu")
require("cheatsheet")
-- require("calendar")
-- require("expose")
local anycomplete = require("anycomplete")
anycomplete.registerDefaultBindings()

-- reload hammerspoon (and stop all the running watchers) --
------------------------------------------------------------
local function reloadHammerspoon()
   wifi_watcher:stop()
   sleepWatcher:stop()
   monitorWatcher:stop()
   hs.audiodevice.watcher.stop()
   hs.reload()
end
hotkey.bind(mash, "R", reloadHammerspoon)


--- ole'
alert.show("Hammerspoon 💩 loaded!")
