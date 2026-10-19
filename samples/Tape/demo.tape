# VHS documentation
#
# Output:
#   Output <path>.gif               Create a GIF output at the given <path>
#   Output <path>.mp4               Create an MP4 output at the given <path>
#   Output <path>.webm              Create a WebM output at the given <path>
#
# Require:
#   Require <string>                Ensure a program is on the $PATH to proceed
#
# Settings:
#   Set FontSize <number>            Set the font size of the terminal
#   Set FontFamily <string>          Set the font family of the terminal
#   Set Height <number>              Set the height of the terminal
#   Set Width <number>               Set the width of the terminal
#   Set LetterSpacing <float>        Set the letter spacing of the terminal
#   Set LineHeight <float>           Set the line height of the terminal
#   Set LoopOffset <float>%          Set the starting frame offset of the GIF loop
#   Set Theme <json|string>          Set the theme of the terminal
#   Set Padding <number>             Set the padding of the terminal
#   Set Framerate <number>           Set the framerate of the recording
#   Set PlaybackSpeed <float>        Set the playback speed of the recording
#   Set MarginFill <file|#000000>    Set the file or color the margin will be filled with
#   Set Margin <number>              Set the size of the margin
#   Set BorderRadius <number>        Set the border radius of the terminal
#   Set WindowBar <string>           Set the window bar type
#   Set WindowBarSize <number>       Set the window bar size
#   Set TypingSpeed <time>           Set the typing speed of the terminal
#
# Sleep:
#   Sleep <time>                     Sleep for <time> seconds
#
# Type:
#   Type[@<time>] "<characters>"     Type characters with an optional delay
#
# Display:
#   Hide                             Hide the subsequent commands from the output
#   Show                             Show the subsequent commands from the output

Output examples/demo.gif

Require echo

Set Shell "bash"
Set FontSize 32
Set Width 1200
Set Height 600

Type "echo 'Welcome to VHS!'" Sleep 500ms Enter

Sleep 5s
