# Uxn-Lua

A port of the [Uxn](https://wiki.xxiivv.com/site/uxntal.html) instruction set and [Varvara](https://wiki.xxiivv.com/site/varvara.html) virtual computer to the Lua language and the Love2D game engine. Is compatible with most ROMs, but runs at a much lower speed for cpu-intensive ROMs.

## Installation

Running the unit tests requires [Busted](https://olivinelabs.com/busted/)

Running this repository as a Love game requires [Love 11+](https://love2d.org/)

Running uxn.lua as a standalone requires [LuaJIT](https://luajit.org/) or a bitwise operations libray such as the standalone [bitwise module](https://bitop.luajit.org/)

## Quickstart

```lua
Uxn = require "uxn"
cpu = Uxn.Uxn:new()

-- Start the instruction pointer at the initial location
cpu.ip = 0x0100

-- Load some bytes into memory
values = {
	0x80, 0x13, -- LIT 0x13
	0x01, -- INC
	0x80, 0x37, -- LIT 0x37

	0x18, -- ADD

	0x00, -- BREAK
}

-- Copy the values offset by 255 so they start at the location of IP
for i = 1, #values do
	cpu.memory[i + 255] = values[i]
end

cpu:runUntilBreak()

-- Should print 75 or 0x4b
print(cpu.program_stack:pop())

```

## Devices

The device object stores 16 bytes of memory as well as providing callbacks to trigger when different ports are written to

### Device:addPort(port, length, onRead, onWrite)

### Example 1

```lua
local device = Device:new()

-- Add a byte port at 0x08
device:addPort(0x08, false, nil, function(byte)
	print("wrote a byte", byte, "to port 0x08")
end)
```

## TODO

- Add file writing
- Audo
- Implement debug properties of System device
- Add datetime device
