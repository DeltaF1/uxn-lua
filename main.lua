local Uxn = (require "uxn").Uxn
bit = require "bit"

local band, bor, bxor, bnot = bit.band, bit.bor, bit.bxor, bit.bnot
local arshift, rshift, lshift = bit.arshift, bit.rshift, bit.lshift

local math = require "math"
local devices = require "love-devices"

Device = require "device"

-- Debugging
--jit.off()
PROFILE = nil

-- Screen parameters
WIDTH = 300
HEIGHT = 200
PADDING = 20
SCALING = 2

function setupCPU(mem)
  local cpu = Uxn:new(mem)
  cpu.ip = 0x0100
  cpu.PRINT = false
  cpu.memory.ERROR_ON_UNINITIALIZED_READ = false

  system = cpu:add_device(0, devices.system)
  console = cpu:add_device(1, devices.console)
  screen = cpu:add_device(2, devices.screen)
  controller = cpu:add_device(8, devices.controller)
  mouse = cpu:add_device(9, devices.mouse)
  file = cpu:add_device(10, devices.file)

  return cpu
end

function love.load(arg)
  love.mouse.setVisible(false)
  love.profiler = require('cigumo_profile')
  love.graphics.setDefaultFilter("nearest", "nearest")

  love.graphics.setNewFont("mono.ttf", 14)
  love.graphics.setBackgroundColor(0,0,0)
  love.window.setMode((WIDTH * SCALING) + (PADDING * 2), (HEIGHT * SCALING) + (PADDING * 2))
  
  -- This is the shader that translates system colours into
  -- palette colours
  paletteShader = love.graphics.newShader [[
    uniform vec3 palette[4];
    
    vec4 effect( vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords ) {
      vec4 pixel = Texel(tex, texture_coords);
      int index = int(pixel.r+(pixel.g*2));
      pixel.rgb = palette[index];
        
      return pixel * color;
    }
  ]]


  local memory = {}

  -- Preload the zero-page
  for i = 0, 255 do
    memory[i] = 0
  end

  -- Takes in a filename from the command line, else load boot.rom
  local data, size = love.filesystem.read("data", arg[1] or "boot.rom")

  -- Unpack the bytes into a Lua table
  data = {love.data.unpack(string.rep("B", size), data)}

  for i = 1, #data - 1 do
    memory[i + 255] = data[i]
  end

  -- Create a new CPU
  cpu = setupCPU(memory)

  -- Execute the initial vector
  cpu:runUntilBreak()
  print("cpu is done initial run") 

  if PROFILE then
    Device.DEBUG_NUM_CALLS.read = {}
    Device.DEBUG_NUM_CALLS.write = {}
    love.profiler.start()
    cpu.debug_profile = {}
    cpu.device_triggers = {}
    cpu.device_reads = {}
    cpu.device_writes = {}
  end
end

local frame = 0
function love.draw()
  frame = frame + 1

  -- Run the screen vector
  screen:trigger()

  if frame == PROFILE then
    love.profiler.stop()
    print(love.profiler.report())
    print("device", "num_triggers")
    for k, v in pairs(cpu.device_triggers) do
      print(k, v)
    end

    print("device", "cpu_reads")
    for k, v in pairs(cpu.device_reads) do
      print(k, v)
    end

    print("device", "cpu_writes")
    for k, v in pairs(cpu.device_writes) do
      print(k, v)
    end

    print("device", "total Device reads")
    for k, v in pairs(Device.DEBUG_NUM_CALLS.read) do
      print(k, v)
    end

    print("device, total Device writes")
    for k, v in pairs(Device.DEBUG_NUM_CALLS.write) do
      print(k, v)
    end

    print(cpu:print_profile())
  end

  love.graphics.push()

  love.graphics.translate(PADDING, PADDING)
  love.graphics.scale(SCALING, SCALING)
  love.graphics.setColor(1,1,1)

  love.graphics.setBlendMode("replace", "premultiplied")
  love.graphics.setShader(paletteShader)

  love.graphics.draw(screen.back)

  love.graphics.setBlendMode("alpha")
  love.graphics.draw(screen.front)

  love.graphics.setShader()

  love.graphics.pop()

  local dbg = "x: "..screen:readShort(8).." y: "..screen:readShort(10).." "
  dbg = dbg.."PS: "..table.concat(cpu.program_stack, " ").." "
  dbg = dbg.."frame: "..frame.." fps: "..love.timer.getFPS()
  love.graphics.setColor(1,0,0)
  --love.graphics.print(dbg, 10, 300)
end

-- Take in a love keyconstant and return which bit in the controller byte
keyToBit = {
  ["rctrl"] = 0, ["lctrl"] = 0,
  ["ralt"] = 2, ["lalt"] = 2,
  ["rshift"] = 4, ["lshift"] = 4,
  ["escape"] = 8,
  ["up"] = 16,
  ["down"] = 32,
  ["left"] = 64,
  ["right"] = 128,
}

function love.textinput(text)
  controller[3] = string.byte(string.sub(text,1,1))

  controller:trigger()
end

function love.keypressed(key)
  controller[2] = bor(controller[2], keyToBit[key] or 0)
  
  local ascii

  if key == "backspace" then
    ascii = 0x08
  elseif key == "return" then
    ascii = 0x0d
  elseif key == "tab" then
    ascii = 0x09
  elseif key == "space" then
    ascii = 0x20
  elseif key == "delete" then
    ascii = 0x7f
  end

  if ascii then
    controller[3] = ascii
  end

  controller:trigger()
end

function love.keyreleased(key)
  controller[2] = band(controller[2], bnot(keyToBit[key] or 0))
  controller[3] = 0

  controller:trigger()
end

--[[ TODO
function love.filedropped(file)
  -- TODO: Reset devices
  cpu = setupCPU() 

  file:open("r")

  local data = file:read("data")

  data = love.data.encode("string", "hex", data)

  cpu.memory.hex_rom = data

  cpu:runUntilBreak()
end
]]--

function normalizeMouse(x, y)
  -- Move to the center to offset math.floor
  x = x + 0.5
  y = y + 0.5

  x = x - PADDING
  y = y - PADDING

  x = x / SCALING
  y = y / SCALING

  return math.floor(x), math.floor(y)
end

local old_x, old_y

function love.mousemoved(x, y)
  x, y = normalizeMouse(x, y)
  if x ~= old_x or y ~= old_y then

    mouse:writeShort(2, x)
    mouse:writeShort(4, y)

    old_x = x
    old_y = y

    mouse:trigger()
  end
end

function love.mousepressed(x, y, button)
  x, y = normalizeMouse(x, y)

  mouse:writeShort(2, x)
  mouse:writeShort(4, y)

  mouse[6] = bor(mouse[6], button == 1 and 0x01 or 0x10)

  mouse:trigger()
end

function love.mousereleased(x, y, button)
  x, y = normalizeMouse(x, y)

  mouse:writeShort(2, x)
  mouse:writeShort(4, y)

  mouse[6] = band(mouse[6], button == 1 and 0x10 or 0x01)

  mouse:trigger()
end

function love.wheelmoved(_, y)
  mouse[7] = y > 0 and 1 or -1

  mouse:trigger()
end

function love.update(dt)
  mouse[7] = 0
end
