// spinner - © Mohamed Gonem

get term_flush from std::term
get sleep, args, exit, pid, exec, exec_background, wait_pid, process_running, term_pid, os_name from std::process
get print, eprintln, isatty from std::io
get format, starts_with, trim, split from std::str
get arr_reverse, arr_contains, arr_last, arr_index_of, len from std::array
get to_int from std::types
get result_unwrap, is_err from std::res
get mod from std::math
get time_now_ms from std::time

// --- global variables
dec bool reversed = false
dec int style = 0
dec arr[string] frames = []
dec int count = -1
dec string message = ""
dec string finish_message = ""
dec bool color_random = false
dec string frame_color = ""
dec string msg_color = ""
dec string finish_color = ""
dec string exec_cmd = ""
dec bool show_output = false
dec string arrow_color = ""
dec string output_color = ""
dec arr[string] rainbow = ["31", "32", "33", "34", "35", "36"]
dec int timeout_secs = -1
dec bool quiet = false
dec int delay_ms = 100
dec string custom_frames_str = ""
dec string prefix = ""
dec string suffix = ""
dec bool json_mode = false
dec bool notify = false
dec string custom_msg_str = ""
dec bool show_output_line = false

// --- basic set of animations
dec dots = ["⠋", "⠙", "⠸", "⢰", "⣠", "⣄", "⡆", "⠇"]
dec wave = ["⠀", "⠄", "⠆", "⠇", "⠏", "⠗", "⠿", "⣿", "⣷", "⣶", "⣦", "⣤", "⣄", "⣀", "⠤", "⠐"]
dec pulse = ["⣀", "⣤", "⣶", "⣾", "⣿", "⣶", "⣤", "⣀"]
dec fill = ["⠀", "⡀", "⣀", "⣄", "⣤", "⣦", "⣴", "⣼", "⣶", "⣾", "⣿"]
dec bounce = ["⠁", "⠈", "⠐", "⠠", "⡀", "⢀", "⠠", "⠐", "⠈"]
dec heavy = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]
dec line = ["-", "\\", "|", "/"]
dec arc = ["◜", "◠", "◝", "◞", "◡", "◟"]
dec dots2 = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]
dec marks = [" ", "▹", "▸", "▹", " "]
dec star = ["✶", "✳", "✴", "✳"]
dec toggle = ["□", "■"]
dec bounce2 = ["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"]
dec clock = ["🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚", "🕛"]

dec args = args()

// --- checking if -h is provided
//     then prints usage and exits
if args.arr_contains("-h")? {
  print("\e[1mspinner\e[0m - feature-rich terminal spinner with process wrapper\n")
  print("\n")
  print("A customizable terminal spinner for CLI tools. Displays animated\n")
  print("loading indicators with color support, process wrapping, live\n")
  print("output, and progress tracking.\n")
  print("\n")
  print("\e[36musage:\e[0m spinner [options]\n")
  print("\n")
  print("\e[36manimation:\e[0m\n")
  print("  \e[33m-s\e[0m \e[1m<style>\e[0m     animation style (see styles below)\n")
  print("  \e[33m-r\e[0m             reverse the animation direction\n")
  print("  \e[33m-d\e[0m \e[1m<ms>\e[0m        delay between frames (default: 100ms)\n")
  print("  \e[33m-f\e[0m \e[1m<a b c>\e[0m     custom frames (space-separated)\n")
  print("\n")
  print("\e[36mmessage:\e[0m\n")
  print("  \e[33m-m\e[0m \e[1m<text>\e[0m      text to display next to the spinner\n")
  print("  \e[33m-M\e[0m \e[1m<text>\e[0m      text to display after spinner finishes\n")
  print("  \e[33m-F\e[0m \e[1m<a,b,c>\e[0m     animated messages (comma-separated, cycles)\n")
  print("\n")
  print("\e[36mprocess wrapper:\e[0m\n")
  print("  \e[33m-e\e[0m \e[1m<cmd>\e[0m       run command with spinner (captures output)\n")
  print("  \e[33m-o\e[0m             show stdout on success (always shown on failure)\n")
  print("  \e[33m-L\e[0m             show live output line from -e command\n")
  print("  \e[33m-t\e[0m \e[1m<sec>\e[0m       timeout: kill command after N seconds\n")
  print("\n")
  print("\e[36moutput:\e[0m\n")
  print("  \e[33m--json\e[0m         JSON output {status, exit_code, duration, output}\n")
  print("  \e[33m--notify\e[0m       desktop notification on success (Linux)\n")
  print("  \e[33m-q\e[0m             quiet mode: suppress animation, show result only\n")
  print("\n")
  print("\e[36mappearance:\e[0m\n")
  print("  \e[33m-C\e[0m \e[1m[colors]\e[0m    color mode (see colors below)\n")
  print("  \e[33m--pre\e[0m \e[1m<text>\e[0m   prefix before spinner\n")
  print("  \e[33m--suf\e[0m \e[1m<text>\e[0m   suffix after spinner\n")
  print("\n")
  print("\e[36mmisc:\e[0m\n")
  print("  \e[33m-n\e[0m \e[1m<count>\e[0m     run for N cycles, then stop\n")
  print("  \e[33m-h\e[0m             show this help message\n")
  print("\n")
  print("\e[36mcolor modes:\e[0m\n")
  print("  \e[33m-C\e[0m              random color per frame\n")
  print("  \e[33m-C\e[0m \e[1m<color>\e[0m      single color for frames\n")
  print("  \e[33m-C\e[0m \e[1m<f> <m>\e[0m      color for frames and messages\n")
  print("  \e[33m-C\e[0m \e[1m<f> <m> <fm>\e[0m  frames, messages, and finish colors\n")
  print("  \e[33m-C\e[0m \e[1m<f> <m> <fm> <a> <o>\e[0m  + arrow and output colors\n")
  print("  \e[33m-C\e[0m \e[1mrandom <m>\e[0m  random frames with colored messages\n")
  print("  use \e[1m_\e[0m for fallback: 3rd=_ uses 2nd, 4th/5th=_ uses 1st\n")
  print("\n")
  print("\e[36mcolors:\e[0m random | black red green yellow blue magenta cyan white\n")
  print("\n")
  print("\e[36mstyles:\e[0m\n")
  print("  \e[33mdots\e[0m \e[33mwave\e[0m \e[33mpulse\e[0m \e[33mfill\e[0m \e[33mbounce\e[0m \e[33mheavy\e[0m    braille patterns\n")
  print("  \e[33mline\e[0m \e[33marc\e[0m \e[33mdots2\e[0m \e[33mmarks\e[0m \e[33mstar\e[0m \e[33mtoggle\e[0m \e[33mbounce2\e[0m \e[33mclock\e[0m\n")
  print("\n")
  print("\e[36mexamples:\e[0m\n")
  print("  spinner -s wave -m \"Loading...\"\n")
  print("  spinner -s pulse -n 50 -M \"Done!\"\n")
  print("  spinner -C red -m \"Working...\"\n")
  print("  spinner -C cyan magenta -m \"Building...\" -M \"Build complete\"\n")
  print("  spinner -e \"make build\" -C red\n")
  print("  spinner -e \"cargo test\" -m \"Running tests\" -M \"Tests passed\"\n")
  print("  spinner -e \"make\" -L -C random cyan green _ red yellow\n")
  print("\n")
  print("\e[36mexit codes:\e[0m\n")
  print("  \e[33m0\e[0m   success\n")
  print("  \e[33m1\e[0m   general error (failed to start command, etc.)\n")
  print("  \e[33m2\e[0m   usage error (invalid option)\n")
  print("  \e[33m3\e[0m   missing argument (e.g. -e without command)\n")
  print("  \e[33m4\e[0m   invalid value (e.g. unknown style or color)\n")
  print("  \e[33m124\e[0m command timed out (-t)\n")
  print("\n")
  exit(0)
}

// --- checking weather reverse
//     option is provided or not
if args.arr_contains("-r")? {
  reversed = true
}

// --- checking if the option -s
//     is used correctly or not
//     then parses it
if args.arr_contains("-s")? {
  dec target_index = args.arr_index_of("-s")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      match args[target_index + 1] {
        "dots" => { style = 0 }
        "wave" => { style = 1 }
        "pulse" => { style = 2 }
        "fill" => { style = 3 }
        "bounce" => { style = 4 }
        "heavy" => { style = 5 }
        "line" => { style = 6 }
        "arc" => { style = 7 }
        "dots2" => { style = 8 }
        "marks" => { style = 9 }
        "star" => { style = 10 }
        "toggle" => { style = 11 }
        "bounce2" => { style = 12 }
        "clock" => { style = 13 }
        _ => {
          eprintln("\e[31merror:\e[0m '{}' is not a valid style\n  valid styles: dots wave pulse fill bounce heavy\n               line arc dots2 marks star toggle bounce2 clock")
          exit(4)
        }
      }
    } else {
      eprintln("\e[31merror:\e[0m '-s' requires a style argument\n  valid styles: dots wave pulse fill bounce heavy\n               line arc dots2 marks star toggle bounce2 clock")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing style after '-s'\n  valid styles: dots wave pulse fill bounce heavy\n               line arc dots2 marks star toggle bounce2 clock")
    exit(3)
  }
}

// --- checking if the option -n
//     is used correctly or not
if args.arr_contains("-n")? {
  dec target_index = args.arr_index_of("-n")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
        dec result[int] c = args[target_index + 1].to_int()
        if c.is_err() {
          eprintln(format("\e[31merror:\e[0m '-n' expected an integer, got '{}'", args[target_index + 1]))
          exit(4)
        } else {
          dec int c = c.result_unwrap()
          if c > 0 {
            count = c
          } else {
            eprintln(format("\e[31merror:\e[0m '-n' must be a positive integer, got {}", c))
            exit(4)
          }
        }
    } else {
      eprintln("\e[31merror:\e[0m '-n' requires a count argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing count after '-n'")
    exit(3)
  }
}

// --- checking if the option -m
//     is used correctly or not
//     then parses it
if args.arr_contains("-m")? {
  dec target_index = args.arr_index_of("-m")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      message = args[target_index + 1]
    } else {
      eprintln("\e[31merror:\e[0m '-m' requires a message argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing message after '-m'")
    exit(3)
  }
}

// --- checking if the option -M
//     is used correctly or not
//     then parses it
if args.arr_contains("-M")? {
  dec target_index = args.arr_index_of("-M")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      finish_message = args[target_index + 1]
    } else {
      eprintln("\e[31merror:\e[0m '-M' requires a finish message argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing finish message after '-M'")
    exit(3)
  }
}

// --- checking if the option -e
//     is used correctly or not
//     runs a command with spinner
if args.arr_contains("-e")? {
  dec target_index = args.arr_index_of("-e")?
  if args[target_index] == args.arr_last()? {
    eprintln("\e[31merror:\e[0m '-e' requires a command argument after it")
    exit(3)
  } else {
    exec_cmd = args[target_index + 1]
  }
}

// --- checking if -o is provided
//     show stdout on success
if args.arr_contains("-o")? {
  show_output = true
}

// --- checking if the option -t
//     timeout in seconds
if args.arr_contains("-t")? {
  dec target_index = args.arr_index_of("-t")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      dec result[int] t = args[target_index + 1].to_int()
      if t.is_err() {
        eprintln(format("\e[31merror:\e[0m '-t' expected an integer, got '{}'", args[target_index + 1]))
        exit(4)
      } else {
        dec int t = t.result_unwrap()
        if t > 0 {
          timeout_secs = t
        } else {
          eprintln(format("\e[31merror:\e[0m '-t' must be a positive integer, got {}", t))
          exit(4)
        }
      }
    } else {
      eprintln("\e[31merror:\e[0m '-t' requires a seconds argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing seconds after '-t'")
    exit(3)
  }
}

// --- checking if -q is provided
//     quiet mode - suppress animation
if args.arr_contains("-q")? {
  quiet = true
}

// --- checking if the option -d
//     delay between frames in ms
if args.arr_contains("-d")? {
  dec target_index = args.arr_index_of("-d")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      dec result[int] d = args[target_index + 1].to_int()
      if d.is_err() {
        eprintln(format("\e[31merror:\e[0m '-d' expected an integer, got '{}'", args[target_index + 1]))
        exit(4)
      } else {
        dec int d = d.result_unwrap()
        if d > 0 {
          delay_ms = d
        } else {
          eprintln(format("\e[31merror:\e[0m '-d' must be a positive integer, got {}", d))
          exit(4)
        }
      }
    } else {
      eprintln("\e[31merror:\e[0m '-d' requires a milliseconds argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing milliseconds after '-d'")
    exit(3)
  }
}

// --- checking if the option -f
//     custom frames (space-separated)
if args.arr_contains("-f")? {
  dec target_index = args.arr_index_of("-f")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      custom_frames_str = args[target_index + 1]
    } else {
      eprintln("\e[31merror:\e[0m '-f' requires a frames argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing frames after '-f'")
    exit(3)
  }
}

// --- checking if the option --pre
//     prefix before spinner
if args.arr_contains("--pre")? {
  dec target_index = args.arr_index_of("--pre")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      prefix = args[target_index + 1]
    } else {
      eprintln("\e[31merror:\e[0m '--pre' requires a prefix argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing prefix after '--pre'")
    exit(3)
  }
}

// --- checking if the option --suf
//     suffix after spinner
if args.arr_contains("--suf")? {
  dec target_index = args.arr_index_of("--suf")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      suffix = args[target_index + 1]
    } else {
      eprintln("\e[31merror:\e[0m '--suf' requires a suffix argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing suffix after '--suf'")
    exit(3)
  }
}

// --- checking if --json is provided
//     JSON output mode
if args.arr_contains("--json")? {
  json_mode = true
  quiet = true
}

// --- checking if --notify is provided
//     desktop notification on completion
if args.arr_contains("--notify")? {
  if os_name() != "linux" {
    eprintln("\e[31merror:\e[0m --notify is only supported on Linux")
    exit(2)
  }
  notify = true
}

// --- checking if the option -F
//     animated messages (comma-separated)
if args.arr_contains("-F")? {
  dec target_index = args.arr_index_of("-F")?
  if !(args[target_index] == args.arr_last()?) {
    if target_index + 1 < args.len()? {
      custom_msg_str = args[target_index + 1]
    } else {
      eprintln("\e[31merror:\e[0m '-F' requires a messages argument after it")
      exit(3)
    }
  } else {
    eprintln("\e[31merror:\e[0m missing messages after '-F'")
    exit(3)
  }
}

// --- checking if -L is provided
//     show live output line
if args.arr_contains("-L")? {
  show_output_line = true
}

// --- checking if the option -C
//     is used correctly or not
//     0 args: random color per frame
//     1 arg:  color for frames only
//     2 args: color for frames and messages
//     3 args: frame, message, finish (_ = use 2nd)
//     4 args: frame, message, finish, arrow (_ = use 1st)
//     5 args: frame, message, finish, arrow, output (_ = use 1st)
if args.arr_contains("-C")? {
  dec target_index = args.arr_index_of("-C")?
  // -C is last arg -> random mode
  if args[target_index] == args.arr_last()? {
    color_random = true
  } else {
    dec string first = args[target_index + 1]
    if first.starts_with("-") {
      color_random = true
    } else {
      // 1st arg -> frame color
      match first {
        "random" => { color_random = true }
        "black" => { frame_color = "30" }
        "red" => { frame_color = "31" }
        "green" => { frame_color = "32" }
        "yellow" => { frame_color = "33" }
        "blue" => { frame_color = "34" }
        "magenta" => { frame_color = "35" }
        "cyan" => { frame_color = "36" }
        "white" => { frame_color = "37" }
        _ => {
          eprintln(format("\e[31merror:\e[0m '{}' is not a valid color\n  valid colors: random | black | red | green | yellow | blue | magenta | cyan | white", first))
          exit(4)
        }
      }
      dec bool found_colors_2 = false
      dec bool found_colors_3 = false
      dec bool found_colors_4 = false
      // 2nd arg -> message color
      if target_index + 2 < args.len()? and !args[target_index + 2].starts_with("-") {
        dec string second = args[target_index + 2]
        match second {
          "black" => { msg_color = "30"; found_colors_2 = true }
          "red" => { msg_color = "31"; found_colors_2 = true }
          "green" => { msg_color = "32"; found_colors_2 = true }
          "yellow" => { msg_color = "33"; found_colors_2 = true }
          "blue" => { msg_color = "34"; found_colors_2 = true }
          "magenta" => { msg_color = "35"; found_colors_2 = true }
          "cyan" => { msg_color = "36"; found_colors_2 = true }
          "white" => { msg_color = "37"; found_colors_2 = true }
          _ => {
            eprintln(format("\e[31merror:\e[0m '{}' is not a valid color for messages\n  valid colors: black | red | green | yellow | blue | magenta | cyan | white", second))
            exit(4)
          }
        }
      }
      // 3rd arg -> finish color (_ = use 2nd)
      if found_colors_2 and target_index + 3 < args.len()? {
        dec string third = args[target_index + 3]
        if third == "_" {
          finish_color = msg_color
          found_colors_3 = true
        } else if !third.starts_with("-") {
          match third {
            "black" => { finish_color = "30"; found_colors_3 = true }
            "red" => { finish_color = "31"; found_colors_3 = true }
            "green" => { finish_color = "32"; found_colors_3 = true }
            "yellow" => { finish_color = "33"; found_colors_3 = true }
            "blue" => { finish_color = "34"; found_colors_3 = true }
            "magenta" => { finish_color = "35"; found_colors_3 = true }
            "cyan" => { finish_color = "36"; found_colors_3 = true }
            "white" => { finish_color = "37"; found_colors_3 = true }
            _ => {
              eprintln(format("\e[31merror:\e[0m '{}' is not a valid color for finish message\n  valid colors: _ | black | red | green | yellow | blue | magenta | cyan | white", third))
              exit(4)
            }
          }
        }
      }
      // 4th arg -> arrow color (_ = use 1st)
      if found_colors_3 and target_index + 4 < args.len()? {
        dec string fourth = args[target_index + 4]
        if fourth == "_" {
          if frame_color != "" {
            arrow_color = frame_color
          } else if color_random {
            arrow_color = rainbow[0]
          }
          found_colors_4 = true
        } else if !fourth.starts_with("-") {
          match fourth {
            "black" => { arrow_color = "30"; found_colors_4 = true }
            "red" => { arrow_color = "31"; found_colors_4 = true }
            "green" => { arrow_color = "32"; found_colors_4 = true }
            "yellow" => { arrow_color = "33"; found_colors_4 = true }
            "blue" => { arrow_color = "34"; found_colors_4 = true }
            "magenta" => { arrow_color = "35"; found_colors_4 = true }
            "cyan" => { arrow_color = "36"; found_colors_4 = true }
            "white" => { arrow_color = "37"; found_colors_4 = true }
            _ => {
              eprintln(format("\e[31merror:\e[0m '{}' is not a valid color for arrow\n  valid colors: _ | black | red | green | yellow | blue | magenta | cyan | white", fourth))
              exit(4)
            }
          }
        }
      }
      // 5th arg -> output color (_ = use 1st)
      if found_colors_4 and target_index + 5 < args.len()? {
        dec string fifth = args[target_index + 5]
        if fifth == "_" {
          if frame_color != "" {
            output_color = frame_color
          } else if color_random {
            output_color = rainbow[0]
          }
        } else if !fifth.starts_with("-") {
          match fifth {
            "black" => { output_color = "30" }
            "red" => { output_color = "31" }
            "green" => { output_color = "32" }
            "yellow" => { output_color = "33" }
            "blue" => { output_color = "34" }
            "magenta" => { output_color = "35" }
            "cyan" => { output_color = "36" }
            "white" => { output_color = "37" }
            _ => {
              eprintln(format("\e[31merror:\e[0m '{}' is not a valid color for output\n  valid colors: _ | black | red | green | yellow | blue | magenta | cyan | white", fifth))
              exit(4)
            }
          }
        }
      }
    }
  }
}

match style {
  0 => { frames = dots }
  1 => { frames = wave }
  2 => { frames = pulse }
  3 => { frames = fill }
  4 => { frames = bounce }
  5 => { frames = heavy }
  6 => { frames = line }
  7 => { frames = arc }
  8 => { frames = dots2 }
  9 => { frames = marks }
  10 => { frames = star }
  11 => { frames = toggle }
  12 => { frames = bounce2 }
  13 => { frames = clock }
}

if reversed {
  frames = frames.arr_reverse()?
}

// --- apply custom frames if -f was provided
if custom_frames_str != "" {
  dec arr[string] custom = split(custom_frames_str, " ")
  if custom.len()? == 0 {
    eprintln("\e[31merror:\e[0m '-f' requires at least one frame")
    exit(4)
  }
  frames = custom
}

// --- apply animated messages if -F was provided
dec arr[string] animated_msgs = []
if custom_msg_str != "" {
  animated_msgs = split(custom_msg_str, ",")
  if animated_msgs.len()? == 0 {
    eprintln("\e[31merror:\e[0m '-F' requires at least one message")
    exit(4)
  }
}

// --- TTY detection
//     isatty() checks stdin, used for cursor hide/show only
dec bool is_tty = isatty()

// --- process wrapper mode (-e)
//     runs command with spinner
if exec_cmd != "" {
  dec string outfile = format("/tmp/spinner_out_{}", pid())
  dec string wrapped = format("({}) > {} 2>&1", exec_cmd, outfile)
  dec result[int] pid_result = exec_background(wrapped)
  if pid_result.is_err() {
    eprintln(format("\e[31merror:\e[0m failed to start command: {}", exec_cmd))
    exit(1)
  }
  dec int bg_pid = pid_result.result_unwrap()
  dec int start_ms = time_now_ms()
  dec int timeout_ms = -1
  if timeout_secs > 0 {
    timeout_ms = timeout_secs * 1000
  }
  dec string last_output_line = ""
  if !quiet {
    print("\e[?25l")
  }
  dec int ci = 0
  dec bool timed_out = false
  while process_running(bg_pid) {
    // check timeout
    if timeout_ms > 0 {
      dec int elapsed = time_now_ms() - start_ms
      if elapsed >= timeout_ms {
        term_pid(bg_pid)
        timed_out = true
        break
      }
    }
    // animate frame
    if !quiet {
      dec string fc = frame_color
      if color_random {
        fc = rainbow[mod(ci, rainbow.len()?)?]
      }
      dec string display_msg = message
      if animated_msgs.len()? > 0 {
        display_msg = animated_msgs[mod(ci, animated_msgs.len()?)?]
      }
      dec string fc_prefix = ""
      if fc != "" {
        fc_prefix = format("\e[{}m", fc)
      }
      dec string frame_out = format("{}{}\e[0m", fc_prefix, frames[mod(ci, frames.len()?)?])
      if display_msg != "" {
        dec string mc_prefix = ""
        if msg_color != "" {
          mc_prefix = format("\e[{}m", msg_color)
        }
        dec string msg_out = format(" {}{}\e[0m", mc_prefix, display_msg)
        print(format("\r{}{}{}\e[0m{}", prefix, frame_out, msg_out, suffix))
      } else {
        print(format("\r{}{}{}", prefix, frame_out, suffix))
      }
      // show live output line
      if show_output_line {
        dec result[string] tail = exec(format("tail -1 {}", outfile))
        if !tail.is_err() {
          dec string line = tail.result_unwrap().trim()
          if line != "" and line != last_output_line {
            last_output_line = line
            dec string arrow_str = "↳"
            if arrow_color != "" {
              arrow_str = format("\e[{}m↳\e[0m", arrow_color)
            }
            dec string line_str = line
            if output_color != "" {
              line_str = format("\e[{}m{}\e[0m", output_color, line)
            }
            dec string output_text = format("{} {}", arrow_str, line_str)
            print(format("\n\e[2K\r{}\e[A", output_text))
          }
        }
      }
      term_flush()?
    }
    ci += 1
    sleep(delay_ms)
  }
  dec result[int] exit_result = wait_pid(bg_pid)
  dec int exit_code = 0
  if !exit_result.is_err() {
    exit_code = exit_result.result_unwrap()
  }
  if timed_out {
    exit_code = 124
  }
  dec int duration_ms = time_now_ms() - start_ms
  dec int duration_secs = duration_ms / 1000
  dec int duration_frac = mod((duration_ms / 100), 10).result_unwrap()
  dec string duration_str = format("{}.{}s", duration_secs, duration_frac)
  if !quiet {
    print("\e[?25h")
    print("\e[2K\r")
  }
  dec string end_color = finish_color
  if end_color == "" {
    end_color = msg_color
  }
  dec string finish_msg = finish_message
  if timed_out {
    finish_msg = "Timed out"
    end_color = "31"
  }
  if json_mode {
    // build JSON output
    dec string status = "failed"
    if exit_code == 0 {
      status = "done"
    }
    dec string escaped_out = ""
    if show_output {
      dec result[string] output = exec(format("cat {}", outfile))
      if !output.is_err() {
        escaped_out = output.result_unwrap().trim()
      }
    }
    dec string exit_str = format("{}", exit_code)
    if finish_msg != "" and escaped_out != "" {
      print(format("{\"status\":\"{}\",\"exit_code\":{},\"duration\":\"{}\",\"message\":\"{}\",\"output\":\"{}\"}\n", status, exit_str, duration_str, finish_msg, escaped_out))
    } else if finish_msg != "" {
      print(format("{\"status\":\"{}\",\"exit_code\":{},\"duration\":\"{}\",\"message\":\"{}\"}\n", status, exit_str, duration_str, finish_msg))
    } else if escaped_out != "" {
      print(format("{\"status\":\"{}\",\"exit_code\":{},\"duration\":\"{}\",\"output\":\"{}\"}\n", status, exit_str, duration_str, escaped_out))
    } else {
      print(format("{\"status\":\"{}\",\"exit_code\":{},\"duration\":\"{}\"}\n", status, exit_str, duration_str))
    }
  } else {
    if exit_code == 0 {
      if end_color != "" {
        print(format("\e[{}m✓\e[0m", end_color))
      } else {
        print("\e[32m✓\e[0m")
      }
      if finish_msg != "" {
        if end_color != "" {
          print(format(" \e[{}m{}\e[0m", end_color, finish_msg))
        } else {
          print(format(" {}", finish_msg))
        }
      } else {
        print(" Done")
      }
      print(format(" ({})\n", duration_str))
      if show_output {
        dec result[string] output = exec(format("cat {}", outfile))
        if !output.is_err() {
          dec string out_text = output.result_unwrap().trim()
          if out_text != "" {
            print(format("{}\n", out_text))
          }
        }
      }
    } else {
      print("\e[31m✗\e[0m")
      if finish_msg != "" {
        if end_color != "" {
          print(format(" \e[{}m{}\e[0m", end_color, finish_msg))
        } else {
          print(format(" {}", finish_msg))
        }
      } else {
        print(" Failed")
      }
      print(format(" (exit code {}) ({})\n", exit_code, duration_str))
      dec result[string] output = exec(format("cat {}", outfile))
      if !output.is_err() {
        dec string out_text = output.result_unwrap().trim()
        if out_text != "" {
          print(format("\n{}\n", out_text))
        }
      }
    }
  }
  exec(format("rm -f {}", outfile))
  // notification
  if notify and exit_code == 0 {
    dec string notify_msg = "Command completed"
    if finish_msg != "" {
      notify_msg = finish_msg
    }
    exec(format("notify-send 'Spinner' '{}'", notify_msg))
  }
  exit(exit_code)
}

// --- infinitly print the array
//     of dots/animations when not
//     given a count (-n)
if count != -1 {
  if !quiet {
    print("\e[?25l")
  }
  dec int ci = 0
  while count > 0 {
    if !quiet {
      dec string fc = frame_color
      if color_random {
        fc = rainbow[mod(ci, rainbow.len()?)?]
      }
      dec string display_msg = message
      if animated_msgs.len()? > 0 {
        display_msg = animated_msgs[mod(ci, animated_msgs.len()?)?]
      }
      dec string fc_prefix = ""
      if fc != "" {
        fc_prefix = format("\e[{}m", fc)
      }
      dec string frame_out = format("{}{}\e[0m", fc_prefix, frames[mod(ci, frames.len()?)?])
      if display_msg != "" {
        dec string mc_prefix = ""
        if msg_color != "" {
          mc_prefix = format("\e[{}m", msg_color)
        }
        dec string msg_out = format(" {}{}\e[0m", mc_prefix, display_msg)
        print(format("\r{}{}{}\e[0m{}", prefix, frame_out, msg_out, suffix))
      } else {
        print(format("\r{}{}{}", prefix, frame_out, suffix))
      }
      term_flush()?
    }
    ci += 1
    count -= 1
    sleep(delay_ms)
  }
  if !quiet {
    print("\e[?25h")
    print("\e[2K\r")
  }
  dec string end_color = finish_color
  if end_color == "" {
    end_color = msg_color
  }
  if end_color != "" and finish_message != "" {
    print(format("\e[{}m{}\e[0m\n", end_color, finish_message))
  } else if finish_message != "" {
    print(format("{}\n", finish_message))
  } else {
    print("\n")
  }
} else {
  dec int ci = 0
  while true {
    if !quiet {
      dec string fc = frame_color
      if color_random {
        fc = rainbow[mod(ci, rainbow.len()?)?]
      }
      dec string display_msg = message
      if animated_msgs.len()? > 0 {
        display_msg = animated_msgs[mod(ci, animated_msgs.len()?)?]
      }
      dec string fc_prefix = ""
      if fc != "" {
        fc_prefix = format("\e[{}m", fc)
      }
      dec string frame_out = format("{}{}\e[0m", fc_prefix, frames[mod(ci, frames.len()?)?])
      if display_msg != "" {
        dec string mc_prefix = ""
        if msg_color != "" {
          mc_prefix = format("\e[{}m", msg_color)
        }
        dec string msg_out = format(" {}{}\e[0m", mc_prefix, display_msg)
        print(format("\r{}{}{}\e[0m{}", prefix, frame_out, msg_out, suffix))
      } else {
        print(format("\r{}{}{}", prefix, frame_out, suffix))
      }
      term_flush()?
    }
    ci += 1
    sleep(delay_ms)
  }
}
