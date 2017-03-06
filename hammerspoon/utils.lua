--- utils
local utils = {}

-- Open a URL in Chrome
function utils.openInChrome(url)
   local chrome = hs.application.find("Google Chrome")
   if chrome == nil then
      return
   end

   hs.urlevent.openURLWithBundle(url, chrome:bundleID())
end

-- test if we ken lee a file
function utils.ken_lee(filename)
   local f = io.open(filename, "r")
   if f ~= nil then io.close(f) return true else return false end
end

function utils.file_exists(filename)
   local f = io.open(filename, "rb")
   if f then f:close() end
   return f ~= nil
end

function utils.read_file(filename)
   local f = io.open(filename, "rb")
   if not f then return nil end
   local contents = f:read "*all"
   f:close()
   return contents
end

return utils
