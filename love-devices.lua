local bit = require "bit"

local band, bor, bxor, bnot = bit.band, bit.bor, bit.bxor, bit.bnot
local arshift, rshift, lshift = bit.arshift, bit.rshift, bit.lshift

local Device = require "device"

local devices = {}

local system = Device:new()

system.name = "system"

-- Default palette of black, Red, Green, and Blue
system.palette = {
  [0]={0,0,0},
  {1,0,0},
  {0,1,0},
  {0,0,1},
}

function regeneratePalette(system)
  local r,g,b = system:readShort(8), system:readShort(10), system:readShort(12)
  r,g,b = bit.tohex(r, 4), bit.tohex(g, 4), bit.tohex(b, 4)

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

  paletteShader:send("palette",
    system.palette[0],
    system.palette[1],
    system.palette[2],
    system.palette[3])

  --love.graphics.setBackgroundColor(system.palette[0])
end

system.initColours = false

system:addPort(0x02, false, function(self)
  return self.cpu.program_stack:len()
end)

system:addPort(0x03, false, function(self)
  return self.cpu.return_stack:len()
end)

system:addPort(0x08, true, nil, regeneratePalette)
system:addPort(0x0a, true, nil, regeneratePalette)
system:addPort(0x0c, true, nil, regeneratePalette)

system:addPort(0x0f, false, nil, function()
  error("halt")
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

local screen = function(width, height)
  local screen = Device:new()

  screen.name = "screen"

  screen.back  = love.graphics.newCanvas(width, height)
  screen.front = love.graphics.newCanvas(width, height)

  love.graphics.setCanvas(back)
    -- System colour 0 at full alpha
    love.graphics.clear(0,0,0,1)
  love.graphics.setCanvas(front)
    -- System colour 0 (irrelevant) with alpha
    love.graphics.clear(0,0,0,0)
  love.graphics.setCanvas()

  screen.back:setFilter("nearest", "nearest")
  screen.front:setFilter("nearest", "nearest")

  -- Width
  screen:addPort(2, true)
  screen:writeShort(2, width)

  -- Height
  screen:addPort(4, true)
  screen:writeShort(4, height)

  -- Auto
  screen:addPort(6, false, nil, function(self, byte)
    self.auto_x = band(byte, 0x01) ~= 0
    self.auto_y = band(byte, 0x02) ~= 0
    self.auto_addr = band(byte, 0x04) ~= 0
  end)

  -- X
  screen:addPort(8, true)

  -- Y
  screen:addPort(10, true)

  -- Sprite address
  screen:addPort(12, true)

  -- Write a single pixel
  screen:addPort(14, false, nil, function(self, pixel)
    local layer = band(pixel, 0x40) == 0 and self.back or self.front
    local index = band(pixel, 0x03)

    local alpha = 1.0
    if index == 0 and layer == self.front then
      -- Transparency
      alpha = 0.0
    end
    love.graphics.setBlendMode("replace", "premultiplied")
    love.graphics.setCanvas(layer)

    love.graphics.setColor(index / 4.0, 0, 0, alpha)

    local x, y = self:readShort(8), self:readShort(10)
    -- Offset to account for points grid
    love.graphics.points(x + 0.5, y + 0.5)

    love.graphics.setCanvas()

    if self.auto_x then
      x = x + 1
      self:writeShort(8, x)
    end

    if self.auto_y then
      y = y + 1
      self:writeShort(10, y)
    end
  end)

  -- Thank you to Sejo @ https://compudanzas.net for writing out these tables!

  ONE_BPP_PALETTE = {
    [0] = {0, 0},
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
    return rshift(band(val, lshift(1, n)), n)
  end

  -- Draw a sprite
  screen:addPort(15, false, nil, function(self, spriteByte)
    -- 1bpp = 0 / 2bpp = 1
    local spriteMode = getBit(spriteByte, 7)

    local layer = band(spriteByte, 0x40) == 0 and self.back or self.front

    local verticalFlip = band(spriteByte, 0x20) ~= 0
    local horizontalFlip = band(spriteByte, 0x10) ~= 0

    local x,y = self:readShort(0x08), self:readShort(0x0a)
    local spriteAddr = self:readShort(0x0c)

    local system_palette = {[0] = {0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {1, 1, 0}}

    local palette
    if spriteMode == 0 then
      palette = ONE_BPP_PALETTE[band(spriteByte, 0x0f)]
      -- TODO: Fix the palette table
      palette = {palette[2], palette[1]}
    else
      palette = TWO_BPP_PALETTE[band(spriteByte, 0x0f)]
    end

    local points = {}

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

        local value, colour_index
        -- 1 bpp
        if spriteMode == 0 then
          value = getBit(row, bitAddr)
          colour_index = palette[value+1]
        else
          value = bor(
            lshift(getBit(row2, bitAddr), 1),
            getBit(row, bitAddr)
          )

          colour_index = palette[value+1]
        end

        if colour_index ~= "none" then
          local alpha = 1.0
          if colour_index == 0 and layer == self.front then
            alpha = 0.0
          end

          -- Offset by 0.5, 0.5 to account for points grid
          points[#points+1] = {x + j + 0.5, y + i + 0.5, colour_index / 4.0, 0, 0, alpha}

        end
      end
    end

    love.graphics.setColor(1, 1, 1, 1)
    love.graphics.setBlendMode("replace", "premultiplied")
    love.graphics.setCanvas(layer)

    love.graphics.points(points)

    love.graphics.setCanvas()

    love.graphics.setBlendMode("alpha")

    if self.auto_x then
      x = x + 8
      self:writeShort(8, x)
    end

    if self.auto_y then
      y = y + 8
      self:writeShort(10, y)
    end

    if self.auto_addr then
      spriteAddr = spriteAddr + 8 + (8 * spriteMode)
      self:writeShort(0x0c, spriteAddr)
    end
  end)

  return screen
end

local controller = Device:new()

controller.name = "controller"

-- Button
controller:addPort(2, false)

-- Key
controller:addPort(3, false)

local mouse = Device:new()

mouse.name = "mouse"

-- X
mouse:addPort(2, true)

-- Y
mouse:addPort(4, true)

-- Mouse state
mouse:addPort(6, false)

-- Mouse wheel
mouse:addPort(7, false)

local function openFile(self, mode)
  local cpu = self.cpu

  -- Pointer to the name string
  local name_address = self:readShort(8)

  -- Construct a 32-bit seek offset from 2 device shorts
  local seek = lshift(self:readShort(4), 16) + self:readShort(6)

  local fileName = ""
  local counter = name_address
  local char

  -- Assume null-terminated strings
  while char ~= 0x00 do
    char = cpu.memory[counter]
    fileName = fileName .. string.char(char)
    counter = counter + 1
  end

  print("Trying to open file called ", fileName)

  local file = love.filesystem.newFile(fileName)

  local ok, err = file:open(mode)

  if not ok then return nil end

  file:seek(seek)

  return file
end

local file = Device:new()

-- Success
file:addPort(2, true)

-- File seek ( 32 bits )
file:addPort(4, true)
file:addPort(6, true)

-- Pointer to file name
file:addPort(8, true)

-- Amount of file to read
file:addPort(10, true)

-- Read
file:addPort(12, true, nil, function(self)
  local length = self:readShort(10)
  local target_address = self:readShort(12)

  local file = openFile(self, "r")

  if not file then
    print("file doesn't exist")
    -- Return 0 length for error?
    return self:writeShort(2, 0)
  end

  local contents, real_size = file:read("data", length)
  print("read", real_size, "bytes")
  -- Write size to success byte
  self:writeShort(2, bit.band(real_size, 0xffff))

  -- Interpret the file as a list of bytes
  local pattern = string.rep("B", real_size)
  local dataTable = {love.data.unpack(pattern, contents)}

  -- Copy contents to target_address:target_address+length
  for i = 1, #dataTable - 1 do -- Skip the index value that's returned
    cpu.memory[target_address + i - 1] = dataTable[i]
  end
end)

-- Write
file:addPort(14, true, function(self)
  local length = self:readShort(10)
  local target_address = self:readShort(12)

  local file = openFile(self, "w")

  if not file then
    print("file can't be opened")
    return self:writeShort(2, 0)
  end

  -- Encode memory starting at target, running length bytes

  -- file:write(encodedData)
end)

devices.system = system
devices.console = console
devices.screen = screen
devices.controller = controller
devices.mouse = mouse
devices.file = file

return devices
