#
# includes.sh -- Helper functions for nicer-looking shell-scripts
#
set -e


# Switch to root directory of Linguist checkout
cd_root(){
	root="`dirname "$0"`/.."
	! have realpath || root=`realpath "$root"`
	cmd "cd '$root'"
}


# Execute string as a shell command.
#
# If running verbosely, the command will be echoed to stdout
# with a prompt-symbol prepended.
#
# Any additional arguments will be single-quoted and appended to the end of
# the first argument. This allows for the safe handling of strings that may
# contain unsafe characters; consider something like
#
#    dir=~/John\'s\ Junk\;\ worth\ \$0.00\ AUD
#    cmd 'rm -rf' "$dir"    # Safe
#    cmd "rm -rf '$dir'"    # UNSAFE: chokes on apostrophe and semicolon
#    cmd 'rm -rf "'"$dir"\' # UNSAFE: interpolates '$0'
#
# Examples:
#    cmd 'gem install bundler'
#    cmd 'rm -rf' "$string_with_unsafe_characters"
#
cmd(){
	while [ $# -gt 1 ]; do
		cmd="$1 `sq "$2"`"
		shift 2
		set -- "$cmd" "$@"
		unset cmd
	done
	if [ "$verbose" ]; then
		set -- '$' "$1"
		if [ -t 1 ]; then
			set --  "`sgr 32`$1`sgr 39`" "$2"
		fi
		verb "$1 %s" "$2"
		shift
	fi
	[ -n "$dry_run" ] || eval "$1"
}


# Terminate script with an error message.
#
# By default, an error code of 1 is returned. A different
# value can be selected by means of the `--code` option.
#
# Examples:
#     die 'Exiting with default error code'
#     die -code=2 'Invalid option: %s' "$BADOPT"
#
# shellcheck disable=SC2059
#
die(){
	unset status
	[ $# -gt 1 ] && case $1 in
		--code=*)  status="${1#*=}"; shift ;;
		-c?*)      status="${1#??}"; shift ;;
		-c|--code) status=$2; shift 2 ;;
	esac
	warn "$@"
	exit "${status:-1}"
}


# Print a formatted heading of the form “=> Foo…”
h1(){
	if [ -t 1 ]; then
		printf "`sgr 32`==>`sgr 39` `sgr 1`%s`sgr 22`\n" "$*"
	else
		printf '==> %s\n' "$*"
	fi
}


# Assert that each of the named executables exists in the user's $PATH.
#
# If $verbose is enabled, the result of each assertion is echoed to stdout.
# If $dry_run is enabled, missing executables will *not* cause the function
# to return with an error code.
#
# Example:
#     have ruby gem bundle
#
have(){
	while [ $# -gt 0 ]; do
		if command -v "$1" >/dev/null 2>&1; then
			verb "- have executable '%s': \e[32myes\e[39m" "$1"
			shift
		else
			verb "- have executable '%s': \e[31mno\e[39m" "$1"
			if [ "$dry_run" ]; then shift; else
				# shellcheck disable=SC2016
				warn 'Required command `%s` not found' "$1"
				return 1
			fi
		fi
	done
}


# Emit an ANSI escape sequence to style console output.
#
# This function honours http://no-color.org/: if the `$NO_COLOR` environment
# variable exists, this function becomes a no-op. Two extensions to the spec
# are also provided:
#  1. The alternative spelling `$NO_COLOUR` is accepted if the US variant is
#     not defined. Similarly, the spelling `$FORCE_COLOUR` is also valid.
#  2. The variable `$FORCE_COLOR` (or `$FORCE_COLOUR`) overrides `$NO_COLOR`
#     if set to the numbers 1, 2, 3, the string "true", or the empty string.
#     All other values cause colours to be disabled, à la node(1).
#
# Examples:
#     `sgr 34 22`   => "\033[34;22m"
#     `sgr 38 5 10` => "\033[38;5;10m"
#
sgr(){
	# Treat no arguments as shorthand for `sgr 0`
	[ $# -gt 0 ] || set -- 0

	# Resolve FORCE_COLOUR variable
	if   [ "${FORCE_COLOR+1}"  ]; then set -- FORCE_COLOR  "$@"
	elif [ "${FORCE_COLOUR+1}" ]; then set -- FORCE_COLOUR "$@"
	else                               set -- ""           "$@"
	fi

	# Resolve colour depth, if forced
	if [ "$1" ]; then
		case `eval "echo \"\\$$1\""` in
			''|1|true) shift; set -- 0 16       "$@" ;; # 16-colour support
			2)         shift; set -- 0 256      "$@" ;; # 256-colour support
			3)         shift; set -- 0 16000000 "$@" ;; # 16-million (“true colour”) support
			*)         shift; set -- 1 0        "$@" ;; # Invalid value; disable colours
		esac
	else
		# Resolve NO_COLOUR variable
		if   [ "${NO_COLOR+1}"  ]; then set -- 1 "$@"
		elif [ "${NO_COLOUR+1}" ]; then set -- 1 "$@"
		else                            set -- 0 "$@"
		fi
	fi

	# Do nothing if colours are suppressed
	# shellcheck disable=SC2015
	[ "$1" = 1 ] && return || shift

	shift

	# Generate the final sequence, stripping any erroneous newlines added by older sed(1) versions
	printf '\033[%sm' "$*" | sed 's/  */;/g' | tr -d '\n'
}


# Quote argument as a single-quoted string for shell consumption.
#
# Existing single-quotes are escaped as '\''.
#
# Example:
#     sq "/usr/local/Homebrew/docs/Tips-N'-Tricks.md"
#  => '/usr/local/Homebrew/docs/Tips-N'\''-Tricks.md'
#
sq(){
	printf %s "$1" | sed "
		s/'/'\\\\''/g
		1 s/^/'/
		$ s/$/'/
	"
}


# Verify that a version string is equal to or greater than a given version.
#
# Example:
#     vercmp "`node --version`" v13.2.0  # SemVer
#     vercmp "`gdate -I`" 2022-02-24     # YYYY-MM-DD comparison
#
# Returns:
#    0 - Input is equal or greater than the requested version
#    1 - Input failed to satisfy version constraint
#
vercmp(){
	# Trim "v" prefixes
	set -- "${1#v}" "${2#v}"
	set -- "${1#V}" "${2#V}"

	# Extract components of minimum version
	IFS=.- read -r min_major min_minor min_patch <<-EOF
		`printf '%s\n' "$2"`
	EOF
	min_major=${min_major:-0}
	min_minor=${min_minor:-0}
	min_patch=${min_patch:-0}

	# Extract components of version string to compare
	IFS=.- read -r major minor patch <<-EOF
		`printf '%s\n' "$1"`
	EOF
	major=${major:-0}
	minor=${minor:-0}
	patch=${patch:-0}

	# Perform the comparison
	[ "$major" -ge "$((min_major+1))" ] || {
		[ "$major" -eq "$min_major" ] && \
		[ "$minor" -gt "$min_minor" ]
	} || {
		[ "$major" -eq "$min_major" ] && \
		[ "$minor" -eq "$min_minor" ] && \
		[ "$patch" -ge "$min_patch" ]
	}
}


# Print a formatted message to stdout if running verbosely.
#
# Example:
#    verb "You shouldn't see this unless %s is passed" '--verbose'
#
# shellcheck disable=SC2059
#
verb(){
	[ "$verbose" ] || return
	if [ -t 1 ]; then
		sgr 2
		printf -- "$@"
		sgr 0
	else
		printf -- "$@"
	fi
	printf '\n'
}


# Print a formatted error message to stderr.
#
# Example:
#     warn 'Same syntax as %s' 'printf'
#
# shellcheck disable=SC2015,SC2059
#
warn(){
	[ -t 1 ] && sgr 31 || :
	printf >&2 "%s: " "$0";
	printf >&2 -- "$@"
	[ -t 1 ] && sgr 0 || :
	printf >&2 '\n'
}


# Parse verbosity-related options, unless $VERBOPTS_NO_AUTOPARSE is set
if [ ! "$VERBOPTS_NO_AUTOPARSE" ]; then

	# Recover options parsed from a calling script
	if [ "$VERBOPTS_PARSED_OPTS" ]; then
		set -- "$VERBOPTS_PARSED_OPTS" "$@"
	fi

	unset dry_run  # Enable with '-n' or '--dry-run'
	unset verbose  # Enable with '-v' or '--verbose'

	while [ $# -gt 0 ]; do
		case $1 in
			--dry-run) VERBOPTS_PARSED_OPTS='-n';  dry_run=1; ;;
			--verbose) VERBOPTS_PARSED_OPTS='-v';  verbose=1; ;;
			-nv|-vn)   VERBOPTS_PARSED_OPTS='-nv'; dry_run=1; verbose=1 ;;
			-n)        VERBOPTS_PARSED_OPTS='-n';  dry_run=1; ;;
			-v)        VERBOPTS_PARSED_OPTS='-v';  verbose=1; ;;
			--)        shift; break ;;
			*)         break ;;
		esac
		shift
	done

	# Record parsed options in an environment variable for passing to subshells
	unset VERBOPTS_PARSED_OPTS
	case ${dry_run}:${verbose} in
		1:1) VERBOPTS_PARSED_OPTS='-nv' ;;
		:1)  VERBOPTS_PARSED_OPTS='-v'  ;;
		1:)  VERBOPTS_PARSED_OPTS='-n'  ;;
	esac
	export VERBOPTS_PARSED_OPTS
fi
