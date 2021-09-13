PROFILE = nil
oprint = print
Uxn = require "uxn"
bit = require "bit"
local math = require "math"
devices = require "love-devices"

Device = require "device"

function setupCPU(mem)
  local cpu = Uxn:new(mem)
  cpu.ip = 256
  cpu.PRINT = true
  system = cpu:add_device(0, devices.system)
  console = cpu:add_device(1, devices.console)
  screen = cpu:add_device(2, devices.screen)
  controller = cpu:add_device(8, devices.controller)
  mouse = cpu:add_device(9, devices.mouse)

  return cpu
end

function love.load()
  love.profiler = require('profile.profile')
  love.graphics.setDefaultFilter( "nearest", "nearest"  )
 
  love.graphics.setBackgroundColor(1,1,1)
  love.graphics.setBackgroundColor(love.math.random(), love.math.random(), love.math.random())
  cpu = setupCPU()

  local data = love.filesystem.read("data", "boot.rom")

  data = love.data.encode("string", "hex", data)

  -- preload memory
  for i = 0, 255 do
    cpu.memory[i] = 0
  end
  for i = 1, #data, 2 do
    byte = tonumber(string.sub(data, i, i+1), 16)
    cpu.memory[math.ceil(i/2)+255] = byte
  end

  cpu:runUntilBreak()
  print("cpu is done initial run") 

  if PROFILE then
    love.profiler.start()
  end
end

--[[
function love.textinput(text)
  for i = 1, #text do
    local char = string.byte(text, i)
    console[2] = char

    cpu:trig_device(1)
  end
end
]]--

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

  cpu:trig_device(8)
end

function love.keypressed(key)
  controller[2] = bit.bor(controller[2], keyToBit[key] or 0)
  
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

  cpu:trig_device(8)
end

function love.keyreleased(key)
  controller[2] = bit.band(controller[2], bit.bnot(keyToBit[key] or 0))
  controller[3] = 0

  cpu:trig_device(8)
end

frame = 0
function love.draw()
  frame = frame + 1
  print("\n=====[draw]=====\n")
  cpu:trig_device(2)
  print("draw vector done")
  if frame == PROFILE then
    love.profiler.stop()
    oprint(love.profiler.report())
  end
  love.graphics.push()
  love.graphics.scale(2,2)
  love.graphics.setColor(1,1,1)

  love.graphics.setBlendMode("alpha", "premultiplied")

  love.graphics.draw(screen.back)
  love.graphics.draw(screen.front)

  love.graphics.setBlendMode("alpha")
  
  love.graphics.pop()


  local dbg = "x: "..screen:readShort(8).." y: "..screen:readShort(10).." "
  dbg = dbg.."PS: "..table.concat(cpu.program_stack, " ").." "
  dbg = dbg.."frame: "..frame.." fps: "..love.timer.getFPS()

  love.graphics.setColor(1,0,0)
  love.graphics.print(dbg, 10, 300)

  love.graphics.setColor(0,1,1)
  love.graphics.print(console.stdout, 10, 310)
end

function love.filedropped(file)
  -- TODO: Reset devices
  cpu = setupCPU() 

  file:open("r")

  local data = file:read("data")

  data = love.data.encode("string", "hex", data)

  cpu.memory.hex_rom = data

  cpu:runUntilBreak()
end
