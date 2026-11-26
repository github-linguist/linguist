let targets = #{
    esp32: #{
        arch: "xtensa",
        rust_target: "xtensa-esp32-espidf",
        gcc_target: "xtensa-esp32-elf",
        wokwi_board: "board-esp32-devkit-c-v4",
    },
    esp32c2: #{
        arch: "riscv",
        rust_target: "riscv32imc-esp-espidf",
        gcc_target: "riscv32-esp-elf",
        wokwi_board: "",
    },
    esp32c3: #{
        arch: "riscv",
        rust_target: "riscv32imc-esp-espidf",
        gcc_target: "riscv32-esp-elf",
        wokwi_board: "board-esp32-c3-devkitm-1",
    },
    esp32c5: #{
        arch: "riscv",
        rust_target: "riscv32imac-esp-espidf",
        gcc_target: "riscv32-esp-elf",
        wokwi_board: "board-esp32-c5-devkitc-1",
    },
    esp32c6: #{
        arch: "riscv",
        rust_target: "riscv32imac-esp-espidf",
        gcc_target: "riscv32-esp-elf",
        wokwi_board: "board-esp32-c6-devkitc-1",
    },
    esp32c61: #{
        arch: "riscv",
        rust_target: "riscv32imac-esp-espidf",
        gcc_target: "riscv32-esp-elf",
    },
    esp32h2: #{
        arch: "riscv",
        rust_target: "riscv32imac-esp-espidf",
        gcc_target: "riscv32-esp-elf",
        wokwi_board: "board-esp32-h2-devkitm-1",
    },
    esp32p4: #{
        arch: "riscv",
        rust_target: "riscv32imafc-esp-espidf",
        gcc_target: "riscv32-esp-elf",
        wokwi_board: "board-esp32-p4-preview",
    },
    esp32s2: #{
        arch: "xtensa",
        rust_target: "xtensa-esp32s2-espidf",
        gcc_target: "xtensa-esp32s2-elf",
        wokwi_board: "board-esp32-s2-devkitm-1",
    },
    esp32s3: #{
        arch: "xtensa",
        rust_target: "xtensa-esp32s3-espidf",
        gcc_target: "xtensa-esp32s3-elf",
        wokwi_board: "board-esp32-s3-devkitc-1",
    },
};

let target = variable::get("mcu");
let target_properties = targets.get(target);
for key in target_properties.keys() {
    variable::set(key, target_properties.get(key));
}

let advanced = variable::get("advanced");
if !advanced {
    variable::set("espidfver", "v5.5.3");
    variable::set("git", "false");
    variable::set("installdir", "workspace");
    variable::set("devcontainer", false);
    variable::set("wokwi", false);
}
