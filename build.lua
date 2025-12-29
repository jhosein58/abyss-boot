local host_os = package.config:sub(1, 1) == '\\' and "windows" or "linux"
local target_os = arg[1] or host_os

print("========================================")
print(">> Building Minimal TCC for: " .. target_os)
print("========================================")

local src_dir = "third_party/tcc"
local libs_dir = "libs"
local minimal_dir = "tcc_minimal"
local include_out_dir = minimal_dir .. "/include"

local cc = "zig cc"
local ar = "zig ar"

function exec(cmd)
    print("   [CMD] " .. cmd)
    local ret = os.execute(cmd)
    if ret ~= 0 and ret ~= true then
        print("!! Error executing command.")
        os.exit(1)
    end
end

function mkdir(path)
    if host_os == "windows" then
        os.execute("if not exist \"" .. path .. "\" mkdir \"" .. path .. "\"")
    else
        os.execute("mkdir -p \"" .. path .. "\"")
    end
end

function copy_file(src, dest)
    local input = io.open(src, "rb")
    if not input then
        print("!! File not found: " .. src)
        return
    end
    local data = input:read("*all")
    input:close()

    local output = io.open(dest, "wb")
    output:write(data)
    output:close()
    print("   [CP] " .. src .. " -> " .. dest)
end

mkdir(libs_dir)
mkdir(minimal_dir)
mkdir(include_out_dir)

print("\n>> [1/3] Building libtcc.a (Host Library)...")

local cflags_host = "-c -Os -fno-stack-protector -DTCC_IS_NATIVE -DTCC_TARGET_X86_64"


if target_os == "linux" then
    cflags_host = cflags_host .. " -D_GNU_SOURCE -DHAVE_SYS_MMAN_H -DHAVE_DLFCN_H"
elseif target_os == "windows" then
    cflags_host = cflags_host .. " -DTCC_TARGET_PE"
    if host_os == "linux" then cc = cc .. " -target x86_64-windows-gnu" end
end

exec(string.format("%s %s -I%s -o tcc.o %s/libtcc.c", cc, cflags_host, src_dir, src_dir))

local libtcc_path = libs_dir .. "/libtcc.a"
exec(string.format("%s rcs %s tcc.o", ar, libtcc_path))
os.remove("tcc.o")

print("\n>> [2/3] Building libtcc1.a (Runtime Support)...")

local libtcc1_src = src_dir .. "/lib/libtcc1.c"

local cflags_rt = "-c -Os -fno-stack-protector -DTCC_TARGET_X86_64"

exec(string.format("%s %s -I%s -o libtcc1.o %s", cc, cflags_rt, src_dir, libtcc1_src))

local rt_lib_path = minimal_dir .. "/libtcc1.a"
exec(string.format("%s rcs %s libtcc1.o", ar, rt_lib_path))
os.remove("libtcc1.o")

print("\n>> [3/3] Gathering Vital Headers (No stdlib)...")

local vital_headers = {
    -- "stdio.h",
    -- "stdarg.h",
    -- "stddef.h",
    -- "stdbool.h",
    -- "float.h"
}

for _, header in ipairs(vital_headers) do
    local src_h = src_dir .. "/include/" .. header
    local dest_h = include_out_dir .. "/" .. header
    copy_file(src_h, dest_h)
end

print("\n========================================")
print(">> SUCCESS!")
print("   Linker Lib:  " .. libtcc_path)
print("   Embed Dir:   " .. minimal_dir .. "/ (Use this in Rust!)")
print("========================================")
