local Device = require "device"

local nilfunc = function() end

local devices = {}

-- portnum, short, read, write

local console = Device:new()
console.stdout = ""
console.stderr = ""

-- Vector
console:addPort(0, true)
-- read char
console:addPort(2, false)
-- write char
console:addPort(8, false, nil, function(self, byte)
  self.stdout = self.stdout..string.char(byte)
end)
-- error char
console:addPort(9, false, nil, function(self, byte)
  self.stderr = self.stderr..string.char(byte)
end)

local width, height = 300, 200

local screen = Device:new()

screen.back  = love.graphics.newCanvas(width, height)
screen.front = love.graphics.newCanvas(width, height)
screen.sprites = love.graphics.newCanvas(width, height)

screen.back:setFilter("nearest", "nearest")
screen.front:setFilter("nearest", "nearest")

-- Vector
screen:addPort(0, true)

-- Width
screen:addPort(2, true)
screen[2] = width

-- Height
screen:addPort(4, true)
screen[4] = height

-- X
screen:addPort(8, true)

-- Y
screen:addPort(10, true)

-- Sprite address
screen:addPort(12, true)

function colourIndexToRGB(system, colour)
  local r,g,b = system:readShort(8), system:readShort(10), system:readShort(12)
  r,g,b = bit.tohex(r, 4), bit.tohex(g, 4), bit.tohex(b, 4)

  -- Convert from 0-index to 1-index
  colour = colour + 1
  -- Grab the hex digit for each colour channel
  colour = {
    string.sub(r, colour, colour),
    string.sub(g, colour, colour),
    string.sub(b, colour, colour),
  }

  colour = {
    tonumber(colour[1], 16) / 0xf,
    tonumber(colour[2], 16) / 0xf,
    tonumber(colour[3], 16) / 0xf,
  }

  return colour
end

-- Write a single pixel
screen:addPort(14, false, nil, function(self, pixel)
  local layer = bit.band(pixel, 0xf0)
  local colour = bit.band(pixel, 0x0f)
  
  -- Read colour palette from the system device
  colour = colourIndexToRGB(self.cpu.devices[0], colour)

  local canvas
  if layer == 0 then
    canvas = self.back
  else
    canvas = self.front
  end

  love.graphics.setCanvas(canvas)
  love.graphics.setColor(colour)
  love.graphics.points(screen:readShort(8), screen:readShort(10))

  love.graphics.setCanvas()
end)

TWO_BPP_PALETTE = {
  [0] = {"clear", "clear", "clear", "clear"},
  {0,1,2,3},
  {0,2,0,2},
  {0,3,2,1},
  {1,1,1,1},
  {1,2,3,0},
  {1,3,1,3},
  {1,0,3,2},
  {2,2,2,2},
  {2,3,0,1},
  {2,0,2,0},
  {2,1,0,3},
  {3,3,3,3},
  {3,0,1,2},
  {3,1,3,1},
  {3,2,1,0}
}

function getBit(val, n)
  return bit.rshift(bit.band(val, bit.lshift(1, n)), n)
end

-- Draw a sprite
screen:addPort(15, false, nil, function(self, spriteByte)
  -- 1bpp = 0 / 2bpp = 1
  local spriteMode = getBit(spriteByte, 7)

  local nBytes = 8 + (8 * spriteMode)

  local layer = bit.band(spriteByte, 0x40) == 0 and self.back or self.front

  local verticalFlip = bit.band(spriteByte, 0x20) ~= 0
  local horizontalFlip = bit.band(spriteByte, 0x10) ~= 0
 
  local x,y = self:readShort(0x08), self:readShort(0x0a)
  local spriteAddr = self:readShort(0x0c)
  
  local palette
  if spriteMode == 0 then
    local col1, col2
    palette = bit.band(spriteByte, 0x0f)
    
    if palette == 0 then
      col1, col2 = "clear", "clear"
    end

    col1 = bit.band(palette, 0x3)
    col2 = bit.rshift(palette, 2)
    col2 = bit.band(col2, 0x3)

    if palette == 5 or palette == 0xa or palette == 0xf then
      col2 = "none"
    end
    palette = {col2, col1}
  else
    palette = TWO_BPP_PALETTE[bit.band(spriteByte, 0x0f)]
  end

  love.graphics.setCanvas(layer)
  
  for i = 0, 7 do
    local row
    local rowAddr = i
    if verticalFlip then
      rowAddr = 7 - rowAddr
    end
    local row, row2 = self.cpu.memory[spriteAddr + rowAddr]
    -- 2bpp
    if spriteMode == 1 then
       row2 = self.cpu.memory[spriteAddr + rowAddr + 8] 
       print("row", bit.tohex(row), "row2", bit.tohex(row2))
    end
    
    for j = 0, 7 do
      local bitAddr = j
      
      if not horizontalFlip then
        bitAddr = 7 - bitAddr
      end
      
      local value, colour
      -- 1 bpp
      if spriteMode == 0 then
        value = getBit(row, bitAddr)
        colour = palette[value+1]
      else
        value = bit.bor(
          bit.lshift(getBit(row2, bitAddr), 1),
          getBit(row, bitAddr)
        )

        colour = palette[value+1]
      end

      if colour == "clear" then
        love.graphics.setBlendMode("replace")
        love.graphics.setColor(0,0,0,0)
      elseif colour ~= "none" then
        local rgb = colourIndexToRGB(self.cpu.devices[0], colour)
        love.graphics.setColor(rgb)
      end

      if colour ~= "none" then
        love.graphics.points( x+(j-1), y+(i-1) )
      end

      if colour == "clear" then
        love.graphics.setBlendMode("alpha") 
      end
    end
  end

  love.graphics.setCanvas()
end)

local controller = Device:new()

-- Button
controller:addPort(2, false)

-- Key
controller:addPort(3, false)

devices.console = console
devices.screen = screen
devices.controller = controller
return devices
