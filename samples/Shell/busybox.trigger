#!/bin/sh

do_bb_install=

for i in "$@"; do
	case "$i" in
		/lib/modules/*)
			# don't run busybox depmod if we have kmod installed
			# we dont need to run it twice.
			target=$(readlink -f "$(command -v depmod || true)")
			if [ -d "$i" ] && [ "$target" = "/bin/busybox" ]; then
				/bin/busybox depmod ${i#/lib/modules/}
			fi
			;;
		*) do_bb_install=yes;;
	esac
done

if [ -n "$do_bb_install" ]; then
	[ -e /bin/bbsuid ] && /bin/bbsuid --install
	[ -e /bin/busybox-extras ] && /bin/busybox-extras --install -s
	/bin/busybox --install -s
fi