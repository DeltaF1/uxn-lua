# ! This repo is very out of date and I have not kept it up to date with any changes to the spec since January 2022 !
I have lost interest in this project and am unlikely to update it any time soon. The instruction set itself has changed and I believe there have been lots of changes to various devices in the interim. ROMs compiled since January of 2022 may not run in this emulator

# Uxn-Lua

A port of the [Uxn](https://wiki.xxiivv.com/site/uxntal.html) instruction set and [Varvara](https://wiki.xxiivv.com/site/varvara.html) virtual computer to the Lua language and the Love2D game engine. ~~Is compatible with most ROMs, but runs at a much lower speed for cpu-intensive ROMs.~~

## Installation

Running the unit tests requires [Busted](https://olivinelabs.com/busted/)

Running this repository as a Love game requires [Love 11+](https://love2d.org/)

Running uxn.lua as a standalone requires [LuaJIT](https://luajit.org/) or a bitwise operations libray such as the standalone [bitwise module](https://bitop.luajit.org/)

## Quickstart

### As a Love game

`love .`

Will try to load a file called `boot.rom` in the top-level directory.

`love . somename.rom`

Will try to load a file called `somename.rom` in the top-level directory.

### Standalone use of the cpu

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

## CPU

`Uxn:new([memory])`

Optionally pass in a table that holds the contents of memory. Remember that this table is will be accessed starting at 0 unlike most Lua tables.

`Uxn:runUntilBreak()`

Run the CPU until it hits a BRK instruction (0x00).

`Uxn:addDevice(num, device)`

Adds a [Device] to the specified device number. See the Varvara docs for standard device numbers.

## Devices

The device object stores 16 bytes of memory as well as providing callbacks to trigger when different ports are written to

`Device:addPort(port, length, onRead, onWrite)`

`Device:readShort(addr)`

`Device:writeShort(addr, value)`

`Device:trigger`

Jumps to the vector for this device and runs until it reaches a BRK instruction.


### Examples

Create a device that prints to the console every time port 0x08 is written to.

```lua
local device = Device:new()

-- Add a byte port at 0x08
device:addPort(0x08, false, nil, function(byte)
	print("wrote a byte", byte, "to port 0x08")
end)
```

Attach a device to a cpu

```lua
local cpu = Uxn:new()

cpu:addDevice(num, device)
```

## TODO

- [ ] Add file writing
- [ ] Audo
- [ ] Implement debug properties of System device
- [ ] Add datetime device
