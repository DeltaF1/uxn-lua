local Device = require "device"

local devices = {}

local system = Device:new()

system.name = "system"

function regeneratePalette(system)
  local r,g,b = system:readShort(8), system:readShort(10), system:readShort(12)
  r,g,b = bit.tohex(r, 4), bit.tohex(g, 4), bit.tohex(b, 4)

  system.palette = {}

  for i = 0, 3 do
    -- Grab the hex digit for each colour channel
    colour = {
      string.sub(r, i+1, i+1),
      string.sub(g, i+1, i+1),
      string.sub(b, i+1, i+1),
    }

    colour = {
      tonumber(colour[1], 16) / 0xf,
      tonumber(colour[2], 16) / 0xf,
      tonumber(colour[3], 16) / 0xf,
    }

    system.palette[i] = colour
  end
end

system.initColours = false

system:addPort(0x08, true, nil, regeneratePalette)
system:addPort(0x0a, true, nil, regeneratePalette)
system:addPort(0x0c, true, nil, function(self)
  print("Setting up palette")
  regeneratePalette(self)

  -- This is a janky janky hack to try and detect the first time the system colours are written to

  if not self.initColours then
    self.initColours = true
    -- Set bg canvas colour to system colour 0
    love.graphics.setCanvas(self.cpu.devices[2].back)
    love.graphics.clear(self.palette[0])
    love.graphics.setCanvas()

  end
end)

-- portnum, short, read, write

local console = Device:new()
console.name = "console"
console.stdout = ""
console.stderr = ""

-- read char
console:addPort(2, false)
-- write char
console:addPort(8, false, nil, function(self, byte)
  print("wrote char")
  self.stdout = self.stdout..string.char(byte)
end)
-- error char
console:addPort(9, false, nil, function(self, byte)
  self.stderr = self.stderr..string.char(byte)
end)

local width, height = 300, 200

local screen = Device:new()

screen.name = "screen"

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

-- Write a single pixel
screen:addPort(14, false, nil, function(self, pixel)
  local layer = bit.band(pixel, 0x30)
  local colour = bit.band(pixel, 0x03)
  
  -- Read colour palette from the system device
  colour = self.cpu.devices[0].palette[colour]

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

-- Thank you to Sejo @ https://compudanzas.net for writing out these tables!

ONE_BPP_PALETTE = {
  [0] = {"clear", "clear"},
  {1, 0},
  {2, 0},
  {3, 0},
  {0, 1},
  {1, "none"},
  {2, 1},
  {3, 1},
  {0, 2},
  {1, 2},
  {2, "none"},
  {3, 2},
  {0, 3},
  {1, 3},
  {2, 3},
  {3, "none"},
}

TWO_BPP_PALETTE = {
  [0] = {0,0,1,2},
  {0,1,2,3},
  {0,2,3,1},
  {0,3,1,2},
  {1,0,1,2},
  {"none",1,2,3},
  {1,2,3,1},
  {1,3,1,2},
  {2,0,1,2},
  {2,1,2,3},
  {"none",2,3,1},
  {2,3,1,2},
  {3,0,1,2},
  {3,1,2,3},
  {3,2,3,1},
  {"none",3,1,2}
}

function getBit(val, n)
  return bit.rshift(bit.band(val, bit.lshift(1, n)), n)
end

-- Draw a sprite
screen:addPort(15, false, nil, function(self, spriteByte)
  -- 1bpp = 0 / 2bpp = 1
  local spriteMode = getBit(spriteByte, 7)

  local layer = bit.band(spriteByte, 0x40) == 0 and self.back or self.front

  local verticalFlip = bit.band(spriteByte, 0x20) ~= 0
  local horizontalFlip = bit.band(spriteByte, 0x10) ~= 0
 
  local x,y = self:readShort(0x08), self:readShort(0x0a)
  local spriteAddr = self:readShort(0x0c)
  
  local palette
  if spriteMode == 0 then
    palette = ONE_BPP_PALETTE[bit.band(spriteByte, 0x0f)]
    -- TODO: Fix the palette table
    palette = {palette[2], palette[1]}
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
    local row = self.cpu.memory[spriteAddr + rowAddr]

    -- 2bpp
    local row2
    if spriteMode == 1 then
       row2 = self.cpu.memory[spriteAddr + rowAddr + 8] 
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

      if colour ~= "none" then
        if colour == "clear" then
          if layer == self.front then
            love.graphics.setBlendMode("replace")
          end
          love.graphics.setColor(0,0,0,0)
        else
          love.graphics.setColor(self.cpu.devices[0].palette[colour])
        end

        love.graphics.points( x+(j-1), y+(i-1) )

        if colour == "clear" then
          love.graphics.setBlendMode("alpha")
        end
      end
    end
  end

  love.graphics.setCanvas()
end)

local controller = Device:new()

controller.name = "controller"

-- Button
controller:addPort(2, false)

-- Key
controller:addPort(3, false)

local mouse = Device:new()

mouse:addPort(0, true, nil, function(vec)
  love.mouse.setVisible(vec == 0x00)
end)
mouse.name = "mouse"

-- X
mouse:addPort(2, true)

-- Y
mouse:addPort(4, true)

-- Mouse state
mouse:addPort(6, false)

-- Mouse wheel
mouse:addPort(7, false)

devices.system = system
devices.console = console
devices.screen = screen
devices.controller = controller
devices.mouse = mouse

return devices
