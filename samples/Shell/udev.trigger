#!/bin/sh

for x in "$@"; do
    case "$x" in
        *rules.d*)
            if [ -S /run/udev/control ]; then
                    /usr/bin/udevadm control --reload || :
            fi
            ;;
        *hwdb.d*)
            echo "Updating udev hwdb..."
            /usr/bin/udev-hwdb update || :
            ;;
    esac
done