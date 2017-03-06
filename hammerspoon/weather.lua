--- weather

--- CONFIGURATION
--- poll weather every X minutes
local WEATHER_POLL_INTERVAL = 15


--- CODE
local utils = require("utils")
local DARKSKY_TOKEN = utils.read_file(os.getenv("HOME") .. "/.darksky"):gsub("\n$", "")
local weatherPollEnabled = true


local weatherMenu = hs.menubar.new()
weatherMenu:setTitle("🌈")

-- Set the icon on the menubar
local function setWeatherIcon(icon)
   -- clear-day, clear-night, rain, snow, sleet, wind, fog, cloudy,
   -- partly-cloudy-day, or partly-cloudy-night
   if icon == "clear-day" then
      weatherMenu:setTitle("☀️")
   elseif icon == "clear-night" then
      weatherMenu:setTitle("🌝")
   elseif icon == "cloudy" then
      weatherMenu:setTitle("🌥")
   elseif icon == "partly-cloudy-day" or icon == "partly-cloudy-night" then
      weatherMenu:setTitle("⛅️")
   elseif icon == "rain" then
      weatherMenu:setTitle("🌧")
   elseif icon == "wind" then
      weatherMenu:setTitle("🌬")
   elseif icon == "snow" then
      weatherMenu:setTitle("🌨")
   else
      hs.notify.show("Hammerspoon", "Error", "Unknown weather icon: " .. icon)
   end
   -- hs.alert.show("Currently: " .. current)
end

-- Query a remote API to get forecast data and update the menubar icon
local function getForecast()
   local location = hs.location.get()
   if location == nil then
      hs.notify.show("Hammerspoon", "Error", "Location is nil")
      return
   end

   local callback = function(status, body, _)
      if status ~= 200 then
         -- disable weather poll for further investigation
         weatherPollEnabled = false
         weatherMenu:setTitle("⚠️")
         hs.printf("Weather API call failed: %d\n%s\n", status, body)
      else
         weatherPollEnabled = true
      end

      local forecast = hs.json.decode(body)
      if forecast == nil then
         hs.notify.show("Hammerspoon", "Error", "JSON decode failed (nil)")
         return
      end
      setWeatherIcon(forecast["currently"]["icon"])
   end

   local url = "https://api.darksky.net/forecast/" .. DARKSKY_TOKEN .. "/" ..
      location["latitude"] .. "," .. location["longitude"]
   hs.http.asyncGet(url, nil, callback)
end


weatherMenu:setClickCallback(getForecast)

-- execute very 30 minutes (60 seconds * 30)
hs.timer.doEvery(60 * WEATHER_POLL_INTERVAL, function ()
                 if weatherPollEnabled then getForecast() end end)

-- and call getWeather() upon reload
getForecast()
