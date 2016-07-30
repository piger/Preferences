-- hammerspoon init.lua
-- NOTE: When you reload your configuration you keep all the old "watchers" running!

-- set grid size
hs.grid.GRIDWIDTH = 40
hs.grid.GRIDHEIGHT = 40
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

-- Audio Configuration
local dacName = "FiiO USB DAC-E10"

-- no animation pls
hs.window.animationDuration = 0

local mash = {"cmd", "alt", "ctrl"}
-- local mashift = {"cmd", "alt", "shift"}

hs.hotkey.bind(mash, "M", function() hs.window.focusedWindow():maximize() end)
hs.hotkey.bind(mash, "space", hs.spotify.displayCurrentTrack)
hs.hotkey.bind(mash, "h", hs.grid.resizeWindowThinner)
hs.hotkey.bind(mash, "l", hs.grid.resizeWindowWider)
hs.hotkey.bind(mash, "j", hs.grid.resizeWindowTaller)
hs.hotkey.bind(mash, "k", hs.grid.resizeWindowShorter)
hs.hotkey.bind(mash, "up", hs.grid.pushWindowUp)
hs.hotkey.bind(mash, "down", hs.grid.pushWindowDown)
hs.hotkey.bind(mash, "left", hs.grid.pushWindowLeft)
hs.hotkey.bind(mash, "right", hs.grid.pushWindowRight)
hs.hotkey.bind(mash, ";", function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(mash, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)


-- functions and callbacks --
-----------------------------

-- Open a URL in Chrome
function openInChrome(url)
   local chrome = hs.application.find("Google Chrome")
   if chrome == nil then
      return
   end

   hs.urlevent.openURLWithBundle(url, chrome:bundleID())
end


-- WiFi Watcher --
------------------
local wifi_work_ssid = "zendesk"
local wifi_last_ssid = hs.wifi.currentNetwork()

local wifi_watcher = hs.wifi.watcher.new(function ()
   local wifi_new_ssid = hs.wifi.currentNetwork()

   if wifi_new_ssid == wifi_work_ssid and wifi_last_ssid ~= wifi_work_ssid then
      hs.audiodevice.defaultOutputDevice():setVolume(1)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send():release()
   end

   wifi_last_ssid = wifi_new_ssid
end)
wifi_watcher:start()


-- caffeine mode --
-------------------
caffeineMenu = hs.menubar.new()
caffeineMenu:setTooltip("Caffeinate mode (i.e. disable screen locking)")
caffeinateSleepType = "displayIdle"

function setCaffeineDisplay(state)
   if state then
      caffeineMenu:setTitle("🐴")
   else
      caffeineMenu:setTitle("🎋")
   end
end

function caffeineClicked()
   setCaffeineDisplay(hs.caffeinate.toggle(caffeinateSleepType))
   if hs.caffeinate.get(caffeinateSleepType) then
      hs.alert.show("Niiihhhh!")
   else
      hs.alert.show("Zzz...")
   end
end

caffeineMenu:setClickCallback(caffeineClicked)
setCaffeineDisplay(hs.caffeinate.get(caffeinateSleepType))

-- disable caffeine mode after sleep
local sleepWatcher = hs.caffeinate.watcher.new(function (eventType)
   if (eventType == hs.caffeinate.watcher.systemDidWake) then
      setCaffeineDisplay(hs.caffeinate.set(caffeinateSleepType, false, true))
   end
end)
sleepWatcher:start()


-- Monitor watcher, used to toggle the dynamic profile in iTerm2 --
-------------------------------------------------------------------
function hasExternalMonitor()
   for _, screen in pairs(hs.screen.allScreens()) do
      if screen:name() == "Thunderbolt Display" then
         return true
      end
   end
   return false
end

-- Switch dynamic profile in iTerm2
-- Valid values: iTerm2_Dynamic_12.json, iTerm2_Dynamic_11.json
function setIterm2Profile(filename)
   hs.execute("ln -sf $HOME/Preferences/iTerm2/" .. filename .. " \"$HOME/Library/Application Support/iTerm2/DynamicProfiles/iTerm2_Dynamic.json\"")
end

function updateItermProfile()
   if hasExternalMonitor() then
      setIterm2Profile("iTerm2_Dynamic_12.json")
   else
      setIterm2Profile("iTerm2_Dynamic_11.json")
   end
end

local monitorWatcher = hs.screen.watcher.new(updateItermProfile)
monitorWatcher:start()


-- Audio Device toggle --
-------------------------
audioToggleMenu = hs.menubar.new()
audioToggleMenu:setTooltip("Toggle the default audio output device")

-- Set the menubar icon corresponding to the default output device
function setToggleAudioIcon()
   local device = hs.audiodevice.defaultOutputDevice()
   if device == nil then
      return nil
   end
   local name = device:name()
   if name == "Built-in Output" then
      audioToggleMenu:setTitle("🔈")
   elseif name == dacName then
      audioToggleMenu:setTitle("🎧")
   else
      audioToggleMenu:setTitle("🎵")
   end
end

-- Toggle the default sound output bewteen built-in and DAC
function toggleAudioDevice(modifier)
   local currOutput = hs.audiodevice.defaultOutputDevice()
   local builtinOut = hs.audiodevice.findOutputByName("Built-in Output")
   local fioOutput = hs.audiodevice.findOutputByName(dacName)
   if currOutput == nil or builtinOut == nil or fioOutput == nil then
      hs.alert.show("Cannot find output devices, or FIO is not plugged in")
      return nil
   end

   if currOutput:name() == "Built-in Output" then
      fioOutput:setDefaultOutputDevice()
   else
      builtinOut:setDefaultOutputDevice()
   end

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


-- Music players controls --
----------------------------
function getCurrentMusicPlayer()
   if hs.itunes.isRunning() and hs.itunes.isPlaying() then
      return hs.itunes
   elseif hs.spotify.isRunning() and hs.spotify.isPlaying() then
      return hs.spotify
   end
   return nil
end

-- Get current Artist and Track name from the current music player.
-- Returns: "Artist name", "Track name"
function getCurrentTrackInfo()
   local player = getCurrentMusicPlayer()
   if player == nil then
      return nil, nil
   end

   return player.getCurrentArtist(), player.getCurrentTrack()
end

function lookupOnYoutube()
   local currArtist, currTrack = getCurrentTrackInfo()
   if currArtist == nil or currTrack == nil then
      return
   end
   openInChrome("https://www.youtube.com/results?search_query=" .. hs.http.encodeForQuery(currArtist .. " " .. currTrack))
end

function lookupOnGenius()
   local currArtist, currTrack = getCurrentTrackInfo()
   if currArtist == nil or currTrack == nil then
      return
   end
   openInChrome("http://genius.com/search?q=" .. hs.http.encodeForQuery(currArtist .. " " .. currTrack))
end

musicMenu = hs.menubar.new()
musicMenu:setTitle("🎷")
musicMenu:setTooltip("Music Player (Spotify or iTunes) controls")

musicMenuLayout = {
   { title = "📺 Search on YouTube", fn = lookupOnYoutube },
   { title = "🔮 Search on Genius", fn = lookupOnGenius },
}
musicMenu:setMenu(musicMenuLayout)

-- reload hammerspoon (and stop all the running watchers) --
------------------------------------------------------------
function reloadHammerspoon()
   wifi_watcher:stop()
   sleepWatcher:stop()
   monitorWatcher:stop()
   hs.audiodevice.watcher.stop()
   hs.reload()
end
hs.hotkey.bind(mash, "R", reloadHammerspoon)


--- ole'
hs.alert.show("Hammerspoon 💩 loaded!")
