set_project("sample")
set_version("1.0.0")

add_rules("mode.debug", "mode.release")

option("test", {default = false, description = "the test option"})

add_requires("zlib", {system = false})

target("test")
    set_kind("binary")
    add_files("src/*.c")
    add_packages("zlib")
    if is_plat("windows") then
        add_defines("WIN32")
    end
    if has_config("test") then
        add_defines("TEST")
    end

