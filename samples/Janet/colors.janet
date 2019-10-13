# Ansi terminal colors

(def- colormap
  {:black 30
   :bg-black 40
   :red 31
   :bg-red 41
   :green 32
   :bg-green 42
   :yellow 33
   :bg-yellow 43
   :blue 34
   :bg-blue 44
   :magenta 35
   :bg-magenta 45
   :cyan 36
   :bg-cyan 46
   :white 37
   :bg-white 47

   :bright-black 90
   :bg-bright-black 100
   :bright-red 91
   :bg-bright-red 101
   :bright-green 92
   :bg-bright-green 102
   :bright-yellow 93
   :bg-bright-yellow 103
   :bright-blue 94
   :bg-bright-blue 104
   :bright-magenta 95
   :bg-bright-magenta 105
   :bright-cyan 96
   :bg-bright-cyan 106
   :bright-white 97
   :bg-bright-white 107})

(defn color
  "Take a string made by concatenating xs and colorize it for an ANSI terminal."
  [c & xs]
  (def code (get colormap c))
  (if (not code) (error (string "color " c " unknown")))
  (string "\e[" code "m" ;xs "\e[0m"))

# Print all colors

(loop [c :keys colormap] (print (color c c)))
