-- music_menu.lua

local utils = require("utils")

-- Music players controls --
----------------------------
local function getCurrentMusicPlayer()
   if hs.itunes.isRunning() and hs.itunes.isPlaying() then
      return hs.itunes
   elseif hs.spotify.isRunning() then
      -- NOTE: hs 0.9.46: hs.spotify.isPlaying() always returns nil?
      -- or just when playing radio?
      return hs.spotify
   end
end

-- Get current Artist and Track name from the current music player.
-- Returns: "Artist name", "Track name"
local function getCurrentTrackInfo()
   local player = getCurrentMusicPlayer()
   if not player then
      return nil, nil
   end

   return player.getCurrentArtist(), player.getCurrentTrack()
end

local function lookupOnYoutube()
   local currArtist, currTrack = getCurrentTrackInfo()
   if currArtist and currTrack then
      utils.openInChrome("https://www.youtube.com/results?search_query=" .. hs.http.encodeForQuery(currArtist .. " " .. currTrack))
   end
end

local function lookupOnGenius()
   local currArtist, currTrack = getCurrentTrackInfo()
   if currArtist and currTrack then
      utils.openInChrome("http://genius.com/search?q=" .. hs.http.encodeForQuery(currArtist .. " " .. currTrack))
   end
end

local musicMenu = hs.menubar.new()
musicMenu:setTitle("🎷")
musicMenu:setTooltip("Music Player (Spotify or iTunes) controls")

local musicMenuLayout = {
   { title = "📺 Search on YouTube", fn = lookupOnYoutube },
   { title = "🔮 Search on Genius", fn = lookupOnGenius },
}
musicMenu:setMenu(musicMenuLayout)
