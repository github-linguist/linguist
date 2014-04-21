(* Copyright (C) 2010-2011,2013 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Main (S: MAIN_STRUCTS): MAIN =
struct

open S

structure Compile = Compile ()

structure Place =
   struct
      datatype t = Files | Generated | MLB | O | OUT | SML | TypeCheck

      val toInt: t -> int =
         fn MLB => 1
          | SML => 1
          | Files => 2
          | TypeCheck => 4
          | Generated => 5
          | O => 6
          | OUT => 7

      val toString =
         fn Files => "files"
          | SML => "sml"
          | MLB => "mlb"
          | Generated => "g"
          | O => "o"
          | OUT => "out"
          | TypeCheck => "tc"

      fun compare (p, p') = Int.compare (toInt p, toInt p')
   end

structure OptPred =
   struct
      datatype t =
         Target of string
       | Yes
   end

structure Show =
   struct
      datatype t = Anns | PathMap
   end

val gcc: string ref = ref "<unset>"
val arScript: string ref = ref "<unset>"
val asOpts: {opt: string, pred: OptPred.t} list ref = ref []
val ccOpts: {opt: string, pred: OptPred.t} list ref = ref []
val linkOpts: {opt: string, pred: OptPred.t} list ref = ref []

val buildConstants: bool ref = ref false
val debugRuntime: bool ref = ref false
datatype debugFormat = Dwarf | DwarfPlus | Dwarf2 | Stabs | StabsPlus
val debugFormat: debugFormat option ref = ref NONE
val expert: bool ref = ref false
val explicitAlign: Control.align option ref = ref NONE
val explicitChunk: Control.chunk option ref = ref NONE
datatype explicitCodegen = Native | Explicit of Control.codegen
val explicitCodegen: explicitCodegen option ref = ref NONE
val keepGenerated = ref false
val keepO = ref false
val output: string option ref = ref NONE
val profileSet: bool ref = ref false
val profileTimeSet: bool ref = ref false
val runtimeArgs: string list ref = ref ["@MLton"]
val show: Show.t option ref = ref NONE
val stop = ref Place.OUT

fun parseMlbPathVar (line: String.t) =
   case String.tokens (line, Char.isSpace) of
      [var, path] => SOME {var = var, path = path}
    | _ => NONE

fun readMlbPathMap (file: File.t) =
   if not (File.canRead file) then
      Error.bug (concat ["can't read MLB path map file: ", file])
   else
      List.keepAllMap
      (File.lines file, fn line =>
       if String.forall (line, Char.isSpace)
          then NONE
       else
          case parseMlbPathVar line of
             NONE => Error.bug (concat ["strange mlb path mapping: ",
                                        file, ":: ", line])
           | SOME v => SOME v)

val targetMap: unit -> {arch: MLton.Platform.Arch.t,
                        os: MLton.Platform.OS.t,
                        target: string} list =
   Promise.lazy
   (fn () =>
    let
       val targetsDir =
          OS.Path.mkAbsolute { path = "targets",
                               relativeTo = !Control.libDir }
       val potentialTargets = Dir.lsDirs targetsDir
       fun targetMap target =
          let
             val targetDir =
                OS.Path.mkAbsolute { path = target,
                                     relativeTo = targetsDir }
             val osFile =
                OS.Path.joinDirFile { dir = targetDir,
                                      file = "os" }
             val archFile =
                OS.Path.joinDirFile { dir = targetDir,
                                      file = "arch" }
             val os   = File.contents osFile
             val arch = File.contents archFile
             val os   = List.first (String.tokens (os,   Char.isSpace))
             val arch = List.first (String.tokens (arch, Char.isSpace))
             val os =
                case MLton.Platform.OS.fromString os of
                   NONE => Error.bug (concat ["strange os: ", os])
                 | SOME os => os
             val arch =
                case MLton.Platform.Arch.fromString arch of
                   NONE => Error.bug (concat ["strange arch: ", arch])
                 | SOME a => a
          in
             SOME { arch = arch, os = os, target = target }
          end
          handle _ => NONE
    in
       List.keepAllMap (potentialTargets, targetMap)
    end)

fun setTargetType (target: string, usage): unit =
   case List.peek (targetMap (), fn {target = t, ...} => target = t) of
      NONE => usage (concat ["invalid target: ", target])
    | SOME {arch, os, ...} =>
         let
            open Control
         in
            Target.arch := arch
            ; Target.os := os
         end

fun hasCodegen (cg) =
   let
      datatype z = datatype Control.Target.arch
      datatype z = datatype Control.Target.os
      datatype z = datatype Control.Format.t
      datatype z = datatype Control.codegen
   in
      case !Control.Target.arch of
         AMD64 => (case cg of
                      x86Codegen => false
                    | _ => true)
       | X86 => (case cg of
                    amd64Codegen => false
                  | x86Codegen =>
                      (* Darwin PIC doesn't work *)
                      !Control.Target.os <> Darwin orelse
                      !Control.format = Executable orelse
                      !Control.format = Archive
                  | _ => true)
       | _ => (case cg of
                  amd64Codegen => false
                | x86Codegen => false
                | _ => true)
   end
fun hasNativeCodegen () =
   let
      datatype z = datatype Control.codegen
   in
      hasCodegen amd64Codegen
      orelse hasCodegen x86Codegen
   end


fun defaultAlignIs8 () =
   let
      datatype z = datatype Control.Target.arch
   in
      case !Control.Target.arch of
         Alpha => true
       | AMD64 => true
       | ARM => true
       | HPPA => true
       | IA64 => true
       | MIPS => true
       | Sparc => true
       | S390 => true
       | _ => false
   end

fun makeOptions {usage} =
   let
      val usage = fn s => (ignore (usage s); raise Fail "unreachable")
      fun reportAnnotation (s, flag, e) =
         case e of
            Control.Elaborate.Bad =>
               usage (concat ["invalid -", flag, " flag: ", s])
          | Control.Elaborate.Deprecated ids =>
               if !Control.warnDeprecated
                  then
                     Out.output
                     (Out.error,
                      concat ["Warning: ", "deprecated annotation: ", s, ", use ",
                              List.toString Control.Elaborate.Id.name ids, ".\n"])
               else ()
          | Control.Elaborate.Good () => ()
          | Control.Elaborate.Other =>
               usage (concat ["invalid -", flag, " flag: ", s])
      open Control Popt
      datatype z = datatype MLton.Platform.Arch.t
      datatype z = datatype MLton.Platform.OS.t
      fun tokenizeOpt f opts =
         List.foreach (String.tokens (opts, Char.isSpace),
                       fn opt => f opt)
      fun tokenizeTargetOpt f (target, opts) =
         List.foreach (String.tokens (opts, Char.isSpace),
                       fn opt => f (target, opt))
   in
      List.map
      (
       [
       (Normal, "align", if defaultAlignIs8 () then " {8|4}" else " {4|8}",
        "object alignment",
        (SpaceString (fn s =>
                      explicitAlign
                      := SOME (case s of
                                  "4" => Align4
                                | "8" => Align8
                                | _ => usage (concat ["invalid -align flag: ",
                                                      s]))))),
       (Expert, "ar-script", " <ar>", "path to a script producing archives",
        SpaceString (fn s => arScript := s)),
       (Normal, "as-opt", " <opt>", "pass option to assembler",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "as-opt-quote", " <opt>", "pass (quoted) option to assembler",
        SpaceString
        (fn s => List.push (asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "build-constants", " {false|true}",
        "output C file that prints basis constants",
        boolRef buildConstants),
       (Expert, "cc", " <gcc>", "path to gcc executable",
        SpaceString (fn s => gcc := s)),
       (Normal, "cc-opt", " <opt>", "pass option to C compiler",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (ccOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "cc-opt-quote", " <opt>", "pass (quoted) option to C compiler",
        SpaceString
        (fn s => List.push (ccOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "chunkify", " {coalesce<n>|func|one}", "set chunkify method",
        SpaceString (fn s =>
                     explicitChunk
                     := SOME (case s of
                                 "func" => ChunkPerFunc
                               | "one" => OneChunk
                               | _ => let
                                         val usage = fn () =>
                                            usage (concat ["invalid -chunkify flag: ", s])
                                      in
                                         if String.hasPrefix (s, {prefix = "coalesce"})
                                            then let
                                                    val s = String.dropPrefix (s, 8)
                                                 in
                                                    if String.forall (s, Char.isDigit)
                                                       then (case Int.fromString s of
                                                                NONE => usage ()
                                                              | SOME n => Coalesce
                                                                          {limit = n})
                                                       else usage ()
                                                 end
                                            else usage ()
                                      end))),
       (Expert, "closure-convert-globalize", " {true|false}",
        "whether to globalize during closure conversion",
        Bool (fn b => (closureConvertGlobalize := b))),
       (Expert, "closure-convert-shrink", " {true|false}",
        "whether to shrink during closure conversion",
        Bool (fn b => (closureConvertShrink := b))),
       (Normal, "codegen",
        concat [" {",
                String.concatWith
                (List.keepAllMap
                 (Native :: (List.map (Control.Codegen.all, Explicit)),
                  fn cg =>
                  case cg of
                     Native => if hasNativeCodegen () then SOME "native" else NONE
                   | Explicit cg => if hasCodegen cg
                                       then SOME (Control.Codegen.toString cg)
                                    else NONE),
                 "|"),
                "}"],
        "which code generator to use",
        SpaceString (fn s =>
                     explicitCodegen
                     := SOME (if s = "native"
                                 then Native
                              else (case List.peek
                                         (Control.Codegen.all, fn cg =>
                                          s = Control.Codegen.toString cg) of
                                       SOME cg => Explicit cg
                                     | NONE => usage (concat ["invalid -codegen flag: ", s]))))),
       (Normal, "const", " '<name> <value>'", "set compile-time constant",
        SpaceString (fn s =>
                     case String.tokens (s, Char.isSpace) of
                        [name, value] =>
                           Compile.setCommandLineConstant {name = name,
                                                           value = value}
                      | _ => usage (concat ["invalid -const flag: ", s]))),
       (Expert, "contify-into-main", " {false|true}",
        "contify functions into main",
        boolRef contifyIntoMain),
       (Expert, "debug", " {false|true}", "produce executable with debug info",
        Bool (fn b => (debug := b
                       ; debugRuntime := b))),
       (Expert, "debug-runtime", " {false|true}", "produce executable with debug info",
        boolRef debugRuntime),
       (Expert, "debug-format", " {default|dwarf|dwarf+|drwaf2|stabs|stabs+}",
        "choose debug symbol format",
        SpaceString (fn s =>
                        debugFormat :=
                        (case s of
                            "default" => NONE
                          | "dwarf" => SOME Dwarf
                          | "dwarf+" => SOME DwarfPlus
                          | "dwarf2" => SOME Dwarf2
                          | "stabs" => SOME Stabs
                          | "stabs+" => SOME StabsPlus
                          | _ => usage (concat ["invalid -debug-format flag: ", s])))),
       let
          val flag = "default-ann"
       in
          (Normal, flag, " <ann>", "set annotation default for mlb files",
           SpaceString
           (fn s => reportAnnotation (s, flag,
                                      Control.Elaborate.processDefault s)))
       end,
       (Normal, "default-type", " '<ty><N>'", "set default type",
        SpaceString
        (fn s => (case s of
                     "char8" => Control.defaultChar := s
                   | "int8" => Control.defaultInt := s
                   | "int16" => Control.defaultInt := s
                   | "int32" => Control.defaultInt := s
                   | "int64" => Control.defaultInt := s
                   | "intinf" => Control.defaultInt := s
                   | "real32" => Control.defaultReal := s
                   | "real64" => Control.defaultReal := s
                   | "widechar16" => Control.defaultWideChar := s
                   | "widechar32" => Control.defaultWideChar := s
                   | "word8" => Control.defaultWord := s
                   | "word16" => Control.defaultWord := s
                   | "word32" => Control.defaultWord := s
                   | "word64" => Control.defaultWord := s
                   | _ => usage (concat ["invalid -default-type flag: ", s])))),
       (Expert, "diag-pass", " <pass>", "keep diagnostic info for pass",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let val re = Regexp.compileDFA re
                            in List.push (diagPasses, re)
                            end
           | NONE => usage (concat ["invalid -diag-pass flag: ", s])))),
       let
          val flag = "disable-ann"
       in
          (Normal, flag, " <ann>", "disable annotation in mlb files",
           SpaceString
           (fn s =>
            reportAnnotation (s, flag,
                              Control.Elaborate.processEnabled (s, false))))
       end,
       (Expert, "drop-pass", " <pass>", "omit optimization pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (dropPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -drop-pass flag: ", s])))),
       let
          val flag = "enable-ann"
       in
          (Expert, flag, " <ann>", "globally enable annotation",
           SpaceString
           (fn s =>
            reportAnnotation (s, flag,
                              Control.Elaborate.processEnabled (s, true))))
       end,
       (Expert, "error-threshhold", " <n>", "error threshhold (20)",
        intRef errorThreshhold),
       (Expert, "emit-main", " {true|false}", "emit main() startup function",
        boolRef emitMain),
       (Expert, "expert", " {false|true}", "enable expert status",
        boolRef expert),
       (Normal, "export-header", " <file>", "write C header file for _export's",
        SpaceString (fn s => exportHeader := SOME s)),
       (Expert, "format",
        concat [" {",
                String.concatWith
                (List.keepAllMap
                  (Control.Format.all, fn cg => SOME (Control.Format.toString cg)),
                 "|"),
                "}"],
        "generated output format",
        SpaceString (fn s =>
                     Control.format
                     := (case List.peek
                              (Control.Format.all, fn cg =>
                               s = Control.Format.toString cg) of
                            SOME cg => cg
                          | NONE => usage (concat ["invalid -format flag: ", s])))),
       (Expert, "gc-check", " {limit|first|every}", "force GCs",
        SpaceString (fn s =>
                     gcCheck :=
                     (case s of
                         "limit" => Limit
                       | "first" => First
                       | "every" => Every
                       | _ => usage (concat ["invalid -gc-check flag: ", s])))),
       (Normal, "ieee-fp", " {false|true}", "use strict IEEE floating-point",
        boolRef Native.IEEEFP),
       (Expert, "indentation", " <n>", "indentation level in ILs",
        intRef indentation),
       (Normal, "inline", " <n>", "set inlining threshold",
        Int (fn i => inlineNonRec := {small = i,
                                      product = #product (!inlineNonRec)})),
       (Expert, "inline-into-main", " {true|false}",
        "inline functions into main",
        boolRef inlineIntoMain),
       (Expert, "inline-leafa-loops", " {true|false}", "leaf inline loops",
        Bool (fn loops =>
              case !inlineLeafA of
                 {repeat, size, ...} =>
                    inlineLeafA :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafa-repeat", " {true|false}", "leaf inline repeat",
        Bool (fn repeat =>
              case !inlineLeafA of
                 {loops, size, ...} =>
                    inlineLeafA :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafa-size", " <n>", "set leaf inlining threshold (20)",
        SpaceString (fn s =>
                     case !inlineLeafA of
                        {loops, repeat, ...} =>
                           inlineLeafA :=
                           {loops = loops, repeat = repeat,
                            size = (if s = "inf"
                                       then NONE
                                    else if String.forall (s, Char.isDigit)
                                       then Int.fromString s
                                    else (usage o concat)
                                         ["invalid -inline-leaf-size flag: ", s])})),
       (Expert, "inline-leafb-loops", " {true|false}", "leaf inline loops",
        Bool (fn loops =>
              case !inlineLeafB of
                 {repeat, size, ...} =>
                    inlineLeafB :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafb-repeat", " {true|false}", "leaf inline repeat",
        Bool (fn repeat =>
              case !inlineLeafB of
                 {loops, size, ...} =>
                    inlineLeafB :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafb-size", " <n>", "set leaf inlining threshold (40)",
        SpaceString (fn s =>
                     case !inlineLeafB of
                        {loops, repeat, ...} =>
                           inlineLeafB :=
                           {loops = loops, repeat = repeat,
                            size = (if s = "inf"
                                       then NONE
                                    else if String.forall (s, Char.isDigit)
                                       then Int.fromString s
                                    else (usage o concat)
                                         ["invalid -inline-leaf-size flag: ", s])})),
       (Expert, "inline-nonrec-product", " <n>", "set inlining threshold (320)",
        Int (fn product =>
             case !inlineNonRec of
                {small, ...} =>
                   inlineNonRec := {small = small, product = product})),
       (Expert, "inline-nonrec-small", " <n>", "set inlining threshold (60)",
        Int (fn small =>
             case !inlineNonRec of
                {product, ...} =>
                   inlineNonRec := {small = small, product = product})),
       (Normal, "keep", " {g|o}", "save intermediate files",
        SpaceString (fn s =>
                     case s of
                        "core-ml" => keepCoreML := true
                      | "dot" => keepDot := true
                      | "g" => keepGenerated := true
                      | "machine" => keepMachine := true
                      | "o" => keepO := true
                      | "rssa" => keepRSSA := true
                      | "ssa" => keepSSA := true
                      | "ssa2" => keepSSA2 := true
                      | "sxml" => keepSXML := true
                      | "xml" => keepXML := true
                      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Expert, "keep-pass", " <pass>", "keep the results of pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (keepPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -keep-pass flag: ", s])))),
       (Expert, "libname", " <basename>", "the name of the generated library",
        SpaceString (fn s => libname := s)),
       (Normal, "link-opt", " <opt>", "pass option to linker",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (linkOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "link-opt-quote", " <opt>", "pass (quoted) option to linker",
        SpaceString
        (fn s => List.push (linkOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "loop-passes", " <n>", "loop optimization passes (1)",
        Int
        (fn i =>
         if i >= 1
            then loopPasses := i
            else usage (concat ["invalid -loop-passes arg: ", Int.toString i]))),
       (Expert, "mark-cards", " {true|false}", "mutator marks cards",
        boolRef markCards),
       (Expert, "max-function-size", " <n>", "max function size (blocks)",
        intRef maxFunctionSize),
       (Normal, "mlb-path-map", " <file>", "additional MLB path map",
        SpaceString (fn s => mlbPathVars := !mlbPathVars @ readMlbPathMap s)),
       (Normal, "mlb-path-var", " '<name> <value>'", "additional MLB path var",
        SpaceString
        (fn s => mlbPathVars := !mlbPathVars @
                                [case parseMlbPathVar s of
                                    NONE => Error.bug ("strange mlb path var: " ^ s)
                                  | SOME v => v])),
       (Expert, "native-commented", " <n>", "level of comments  (0)",
        intRef Native.commented),
       (Expert, "native-copy-prop", " {true|false}",
        "use copy propagation",
        boolRef Native.copyProp),
       (Expert, "native-cutoff", " <n>",
        "live transfer cutoff distance",
        intRef Native.cutoff),
       (Expert, "native-live-transfer", " {0,...,8}",
        "use live transfer",
        intRef Native.liveTransfer),
       (Expert, "native-live-stack", " {false|true}",
        "track liveness of stack slots",
        boolRef Native.liveStack),
       (Expert, "native-move-hoist", " {true|false}",
        "use move hoisting",
        boolRef Native.moveHoist),
       (Expert, "native-optimize", " <n>", "level of optimizations",
        intRef Native.optimize),
       (Expert, "native-split", " <n>", "split assembly files at ~n lines",
        Int (fn i => Native.split := SOME i)),
       (Expert, "native-shuffle", " {true|false}",
        "shuffle registers at C-calls",
        Bool (fn b => Native.shuffle := b)),
       (Expert, "opt-passes", " {default|minimal}", "level of optimizations",
        SpaceString (fn s =>
                     let
                        fun err s =
                           usage (concat ["invalid -opt-passes flag: ", s])
                     in
                        List.foreach
                        (!optimizationPasses, fn {il,set,...} =>
                         case set s of
                            Result.Yes () => ()
                          | Result.No s' => err (concat [s', "(for ", il, ")"]))
                     end)),
       (Normal, "output", " <file>", "name of output file",
        SpaceString (fn s => output := SOME s)),
       (Expert, "polyvariance", " {true|false}", "use polyvariance",
        Bool (fn b => if b then () else polyvariance := NONE)),
       (Expert, "polyvariance-hofo", " {true|false}", "duplicate higher-order fns only",
        Bool (fn hofo =>
              case !polyvariance of
                 SOME {product, rounds, small, ...} =>
                    polyvariance := SOME {hofo = hofo,
                                          product = product,
                                          rounds = rounds,
                                          small = small}
               | _ => ())),
       (Expert, "polyvariance-product", " <n>", "set polyvariance threshold (300)",
        Int (fn product =>
             case !polyvariance of
                SOME {hofo, rounds, small, ...} =>
                   polyvariance := SOME {hofo = hofo,
                                         product = product,
                                         rounds = rounds,
                                         small = small}
              | _ => ())),
       (Expert, "polyvariance-rounds", " <n>", "set polyvariance rounds (2)",
        Int (fn rounds =>
             case !polyvariance of
                SOME {hofo, product, small, ...} =>
                   polyvariance := SOME {hofo = hofo,
                                         product = product,
                                         rounds = rounds,
                                         small = small}
              | _ => ())),
       (Expert, "polyvariance-small", " <n>", "set polyvariance threshold (30)",
        Int (fn small =>
             case !polyvariance of
                SOME {hofo, product, rounds, ...} =>
                   polyvariance := SOME {hofo = hofo,
                                         product = product,
                                         rounds = rounds,
                                         small = small}
              | _ => ())),
       (Expert, "prefer-abs-paths", " {false|true}",
        "prefer absolute paths when referring to files",
        boolRef preferAbsPaths),
       (Expert, "prof-pass", " <pass>", "keep profile info for pass",
        SpaceString (fn s =>
                     (case Regexp.fromString s of
                         SOME (re,_) => let val re = Regexp.compileDFA re
                                        in
                                           List.push (profPasses, re)
                                        end
                       | NONE => usage (concat ["invalid -diag-pass flag: ", s])))),
       (Normal, "profile", " {no|alloc|count|time}",
        "produce executable suitable for profiling",
        SpaceString
        (fn s =>
         if !profileSet
            then usage "can't have multiple -profile switches"
         else
            (profileSet := true
             ; profile := (case s of
                              "no" => ProfileNone
                            | "alloc" => ProfileAlloc
                            | "call" => ProfileCallStack
                            | "count" => ProfileCount
                            | "drop" => ProfileDrop
                            | "label" => ProfileLabel
                            | "time" => (profileTimeSet := true
                                         ; ProfileTimeLabel)
                            | "time-field" => ProfileTimeField
                            | "time-label" => ProfileTimeLabel
                            | _ => usage (concat
                                          ["invalid -profile arg: ", s]))))),
       (Normal, "profile-branch", " {false|true}",
        "profile branches in addition to functions",
        boolRef profileBranch),
       (Expert, "profile-c", " <regexp>",
        "include C-calls in files matching <regexp> in profile",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let
                               open Regexp
                               val re = seq [anys, re, anys]
                               val re = compileDFA re
                            in List.push (profileC, re)
                            end
           | NONE => usage (concat ["invalid -profile-c flag: ", s])))),
       (Expert, "profile-exclude", " <regexp>",
        "exclude files matching <regexp> from profile",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let
                               open Regexp
                               val re = seq [anys, re, anys]
                               val re = compileDFA re
                            in List.push (profileInclExcl, (re, false))
                            end
           | NONE => usage (concat ["invalid -profile-exclude flag: ", s])))),
       (Expert, "profile-il", " {source}", "where to insert profile exps",
        SpaceString
        (fn s =>
         case s of
            "source" => profileIL := ProfileSource
          | "ssa" => profileIL := ProfileSSA
          | "ssa2" => profileIL := ProfileSSA2
          | _ => usage (concat ["invalid -profile-il arg: ", s]))),
       (Expert, "profile-include", " <regexp>",
        "include files matching <regexp> from profile",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let
                               open Regexp
                               val re = seq [anys, re, anys]
                               val re = compileDFA re
                            in List.push (profileInclExcl, (re, true))
                            end
           | NONE => usage (concat ["invalid -profile-include flag: ", s])))),
       (Expert, "profile-raise", " {false|true}",
        "profile raises in addition to functions",
        boolRef profileRaise),
       (Normal, "profile-stack", " {false|true}", "profile the stack",
        boolRef profileStack),
       (Normal, "profile-val", " {false|true}",
        "profile val bindings in addition to functions",
        boolRef profileVal),
       (Normal, "runtime", " <arg>", "pass arg to runtime via @MLton",
        SpaceString (fn s => List.push (runtimeArgs, s))),
       (Expert, "show", " {anns|path-map}", "print specified data and stop",
        SpaceString
        (fn s =>
         show := SOME (case s of
                          "anns" => Show.Anns
                        | "path-map" => Show.PathMap
                        | _ => usage (concat ["invalid -show arg: ", s])))),
       (Normal, "show-basis", " <file>", "write out the final basis environment",
        SpaceString (fn s => showBasis := SOME s)),
       (Normal, "show-def-use", " <file>", "write def-use information",
        SpaceString (fn s => showDefUse := SOME s)),
       (Expert, "show-types", " {true|false}", "show types in ILs",
        boolRef showTypes),
       (Expert, "ssa-passes", " <passes>", "ssa optimization passes",
        SpaceString
        (fn s =>
         case List.peek (!Control.optimizationPasses,
                         fn {il, ...} => String.equals ("ssa", il)) of
            SOME {set, ...} =>
               (case set s of
                   Result.Yes () => ()
                 | Result.No s' => usage (concat ["invalid -ssa-passes arg: ", s']))
          | NONE => Error.bug "ssa optimization passes missing")),
       (Expert, "ssa2-passes", " <passes>", "ssa2 optimization passes",
        SpaceString
        (fn s =>
         case List.peek (!Control.optimizationPasses,
                         fn {il, ...} => String.equals ("ssa2", il)) of
            SOME {set, ...} =>
               (case set s of
                   Result.Yes () => ()
                 | Result.No s' => usage (concat ["invalid -ssa2-passes arg: ", s']))
          | NONE => Error.bug "ssa2 optimization passes missing")),
       (Normal, "stop", " {f|g|o|tc}", "when to stop",
        SpaceString
        (fn s =>
         stop := (case s of
                     "f" => Place.Files
                   | "g" => Place.Generated
                   | "o" => Place.O
                   | "tc" => Place.TypeCheck
                   | _ => usage (concat ["invalid -stop arg: ", s])))),
       (Expert, "sxml-passes", " <passes>", "sxml optimization passes",
        SpaceString
        (fn s =>
         case List.peek (!Control.optimizationPasses,
                         fn {il, ...} => String.equals ("sxml", il)) of
            SOME {set, ...} =>
               (case set s of
                   Result.Yes () => ()
                 | Result.No s' => usage (concat ["invalid -sxml-passes arg: ", s']))
          | NONE => Error.bug "sxml optimization passes missing")),
       (Normal, "target",
        concat [" {",
                (case targetMap () of
                    [] => ""
                  | [x] => #target x
                  | x :: _ => concat [#target x, "|..."]),
                "}"],
        "platform that executable will run on",
        SpaceString
        (fn t =>
         (target := (if t = "self" then Self else Cross t);
          setTargetType (t, usage)))),
       (Normal, "target-as-opt", " <target> <opt>", "target-dependent assembler option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) =>
         List.push (asOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-as-opt-quote", " <target> <opt>", "target-dependent assembler option (quoted)",
        (SpaceString2
         (fn (target, opt) =>
          List.push (asOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-cc-opt", " <target> <opt>", "target-dependent C compiler option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) =>
         List.push (ccOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-cc-opt-quote", " <target> <opt>", "target-dependent C compiler option (quoted)",
        (SpaceString2
         (fn (target, opt) =>
          List.push (ccOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-link-opt", " <target> <opt>", "target-dependent linker option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) =>
         List.push (linkOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-link-opt-quote", " <target> <opt>", "target-dependent linker option (quoted)",
        (SpaceString2
         (fn (target, opt) =>
          List.push (linkOpts, {opt = opt, pred = OptPred.Target target})))),
       (Expert, #1 trace, " name1,...", "trace compiler internals", #2 trace),
       (Expert, "type-check", " {false|true}", "type check ILs",
        boolRef typeCheck),
       (Normal, "verbose", " {0|1|2|3}", "how verbose to be",
        SpaceString
        (fn s =>
         verbosity := (case s of
                          "0" => Silent
                        | "1" => Top
                        | "2" => Pass
                        | "3" => Detail
                        | _ => usage (concat ["invalid -verbose arg: ", s])))),
       (Expert, "warn-ann", " {true|false}",
        "unrecognized annotation warnings",
        boolRef warnAnn),
       (Expert, "warn-deprecated", " {true|false}",
        "deprecated feature warnings",
        boolRef warnDeprecated),
       (Expert, "xml-passes", " <passes>", "xml optimization passes",
        SpaceString
        (fn s =>
         case List.peek (!Control.optimizationPasses,
                         fn {il, ...} => String.equals ("xml", il)) of
            SOME {set, ...} =>
               (case set s of
                   Result.Yes () => ()
                 | Result.No s' => usage (concat ["invalid -xml-passes arg: ", s']))
          | NONE => Error.bug "xml optimization passes missing")),
       (Expert, "zone-cut-depth", " <n>", "zone cut depth",
        intRef zoneCutDepth)
       ],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage =
   "mlton [option ...] file.{c|mlb|o|sml} [file.{c|o|s|S} ...]"

val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => !expert}

val usage = fn s => (usage s; raise Fail "unreachable")

fun commandLine (args: string list): unit =
   let
      open Control
      datatype z = datatype MLton.Platform.Arch.t
      datatype z = datatype MLton.Platform.OS.t
      val args =
         case args of
            lib :: args =>
               (libDir := OS.Path.mkCanonical lib
                ; args)
          | _ => Error.bug "incorrect args from shell script"
      val () = setTargetType ("self", usage)
      val result = parse args

      val target = !target
      val targetStr =
         case target of
            Cross s => s
          | Self => "self"
      val targetsDir =
         OS.Path.mkAbsolute { path = "targets",
                              relativeTo = !libDir }
      val targetDir =
         OS.Path.mkAbsolute { path = targetStr,
                              relativeTo = targetsDir }
      val () = libTargetDir := targetDir
      val targetArch = !Target.arch
      val archStr = String.toLower (MLton.Platform.Arch.toString targetArch)
      val targetOS = !Target.os
      val OSStr = String.toLower (MLton.Platform.OS.toString targetOS)

      (* Determine whether code should be PIC (position independent) or not.
       * This decision depends on the platform and output format.
       *)
      val positionIndependent =
         case (targetOS, targetArch, !format) of
            (* Windows is never position independent *)
            (MinGW, _, _) => false
          | (Cygwin, _, _) => false
            (* Technically, Darwin should always be PIC.
             * However, PIC on i386/darwin is unimplemented so we avoid it.
             * PowerPC PIC is bad too, but the C codegen will use PIC behind
             * our back unless forced, so let's just admit that it's PIC.
             *)
          | (Darwin, X86, Executable) => false
          | (Darwin, X86, Archive) => false
          | (Darwin, _, _) => true
            (* On ELF systems, we only need PIC for LibArchive/Library *)
          | (_, _, Library) => true
          | (_, _, LibArchive) => true
          | _ => false
      val () = Control.positionIndependent := positionIndependent

      val stop = !stop

      val () =
         align := (case !explicitAlign of
                      NONE => if defaultAlignIs8 () then Align8 else Align4
                    | SOME a => a)
      val () =
         codegen := (case !explicitCodegen of
                        NONE =>
                           if hasCodegen (x86Codegen)
                              then x86Codegen
                           else if hasCodegen (amd64Codegen)
                               then amd64Codegen
                           else CCodegen
                      | SOME Native =>
                           if hasCodegen (x86Codegen)
                              then x86Codegen
                           else if hasCodegen (amd64Codegen)
                              then amd64Codegen
                           else usage (concat ["can't use native codegen on ",
                                               MLton.Platform.Arch.toString targetArch,
                                               " target"])
                      | SOME (Explicit cg) => cg)
      val () = MLton.Rusage.measureGC (!verbosity <> Silent)
      val () = if !profileTimeSet
                  then (case !codegen of
                           x86Codegen => profile := ProfileTimeLabel
                         | amd64Codegen => profile := ProfileTimeLabel
                         | _ => profile := ProfileTimeField)
                  else ()
      val () = if !exnHistory
                  then (case !profile of
                           ProfileNone => profile := ProfileCallStack
                         | ProfileCallStack => ()
                         | _ => usage "can't use -profile with Exn.keepHistory"
                        ; profileRaise := true)
               else ()

      val () =
         Compile.setCommandLineConstant
         {name = "CallStack.keep",
          value = Bool.toString (!Control.profile = Control.ProfileCallStack)}

      val () =
         let
            val sizeMap =
               List.map
               (File.lines (OS.Path.joinDirFile {dir = !Control.libTargetDir,
                                                 file = "sizes"}),
                fn line =>
                case String.tokens (line, Char.isSpace) of
                   [ty, "=", size] =>
                      (case Int.fromString size of
                          NONE => Error.bug (concat ["strange size: ", size])
                        | SOME size =>
                             (ty, Bytes.toBits (Bytes.fromInt size)))
                 | _ => Error.bug (concat ["strange size mapping: ", line]))
            fun lookup ty' =
               case List.peek (sizeMap, fn (ty, _) => String.equals (ty, ty')) of
                  NONE => Error.bug (concat ["missing size mapping: ", ty'])
                | SOME (_, size) => size
         in
            Control.Target.setSizes
            {cint = lookup "cint",
             cpointer = lookup "cpointer",
             cptrdiff = lookup "cptrdiff",
             csize = lookup "csize",
             header = lookup "header",
             mplimb = lookup "mplimb",
             objptr = lookup "objptr",
             seqIndex = lookup "seqIndex"}
         end

      fun tokenize l =
         String.tokens (concat (List.separate (l, " ")), Char.isSpace)

      (* When cross-compiling, use the named cross compiler.
       * Older gcc versions used -b for multiple targets.
       * If this is still needed, a shell script wrapper can hide this.
       *)
      val gcc =
         case target of
            Cross s =>
               let
                  val {dir = gccDir, file = gccFile} =
                     OS.Path.splitDirFile (!gcc)
               in
                  OS.Path.joinDirFile
                  {dir = gccDir,
                   file = s ^ "-" ^ gccFile}
               end
          | Self => !gcc
      val arScript = !arScript

      fun addTargetOpts opts =
         List.fold
         (!opts, [], fn ({opt, pred}, ac) =>
          if (case pred of
                 OptPred.Target s =>
                    let
                       val s = String.toLower s
                    in
                       s = archStr orelse s = OSStr
                    end
               | OptPred.Yes => true)
             then opt :: ac
          else ac)
      val asOpts = addTargetOpts asOpts
      val ccOpts = addTargetOpts ccOpts
      val ccOpts = concat ["-I",
                           OS.Path.mkAbsolute { path = "include",
                                                relativeTo = !libTargetDir }]
                   :: ccOpts
      val linkOpts =
         List.concat [[concat ["-L", !libTargetDir]],
                      if !debugRuntime then
                      ["-lmlton-gdb", "-lgdtoa-gdb"]
                      else if positionIndependent then
                      ["-lmlton-pic", "-lgdtoa-pic"]
                      else
                      ["-lmlton", "-lgdtoa"],
                      addTargetOpts linkOpts]
      val linkArchives =
         if !debugRuntime then
         [OS.Path.joinDirFile { dir = !libTargetDir, file = "libmlton-gdb.a" },
          OS.Path.joinDirFile { dir = !libTargetDir, file = "libgdtoa-gdb.a" }]
         else if positionIndependent then
         [OS.Path.joinDirFile { dir = !libTargetDir, file = "libmlton-pic.a" },
          OS.Path.joinDirFile { dir = !libTargetDir, file = "libgdtoa-pic.a" }]
         else
         [OS.Path.joinDirFile { dir = !libTargetDir, file =  "libmlton.a" },
          OS.Path.joinDirFile { dir = !libTargetDir, file =  "libgdtoa.a" }]
      val _ =
         if not (hasCodegen (!codegen))
            then usage (concat ["can't use ",
                                Control.Codegen.toString (!codegen),
                                " codegen on ",
                                MLton.Platform.Arch.toString targetArch,
                                " target"])
         else ()
      val () =
         Control.labelsHaveExtra_ := (case targetOS of
                                         Cygwin => true
                                       | Darwin => true
                                       | MinGW => true
                                       | _ => false)
      val _ =
         chunk :=
         (case !explicitChunk of
             NONE => (case !codegen of
                         amd64Codegen => ChunkPerFunc
                       | CCodegen => Coalesce {limit = 4096}
                       | x86Codegen => ChunkPerFunc
                       )
           | SOME c => c)
      val _ = if not (!Control.codegen = x86Codegen) andalso !Native.IEEEFP
                 then usage "must use x86 codegen with -ieee-fp true"
              else ()
      val _ =
         if !keepDot andalso List.isEmpty (!keepPasses)
            then keepSSA := true
         else ()
      val () =
         keepDefUse
         := (isSome (!showDefUse)
             orelse (Control.Elaborate.enabled Control.Elaborate.warnUnused)
             orelse (Control.Elaborate.default Control.Elaborate.warnUnused))
      val warnMatch =
          (Control.Elaborate.enabled Control.Elaborate.nonexhaustiveMatch)
          orelse (Control.Elaborate.enabled Control.Elaborate.redundantMatch)
          orelse (Control.Elaborate.default Control.Elaborate.nonexhaustiveMatch <>
                  Control.Elaborate.DiagEIW.Ignore)
          orelse (Control.Elaborate.default Control.Elaborate.redundantMatch <>
                  Control.Elaborate.DiagEIW.Ignore)
      val _ = elaborateOnly := (stop = Place.TypeCheck
                                andalso not (warnMatch)
                                andalso not (!keepDefUse))
      val _ =
         case targetOS of
            Darwin => ()
          | FreeBSD => ()
          | HPUX => ()
          | Linux => ()
          | MinGW => ()
          | NetBSD => ()
          | OpenBSD => ()
          | Solaris => ()
          | _ =>
               if !profile = ProfileTimeField
                  orelse !profile = ProfileTimeLabel
                  then usage (concat ["can't use -profile time on ",
                                      MLton.Platform.OS.toString targetOS])
               else ()
      fun printVersion (out: Out.t): unit =
         Out.output (out, concat [Version.banner, "\n"])
      val () =
         case !show of
            NONE => ()
          | SOME info =>
            (case info of
                Show.Anns =>
                Layout.outputl (Control.Elaborate.document {expert = !expert},
                                Out.standard)
              | Show.PathMap =>
                let
                   open Layout
                in
                   outputl (align
                            (List.map (Control.mlbPathMap (),
                                       fn {var, path, ...} =>
                                       str (concat [var, " ", path]))),
                            Out.standard)
                end
             ; let open OS.Process in exit success end)
   in
      case result of
      Result.No msg => usage msg
    | Result.Yes [] =>
         (inputFile := "<none>"
          ; if isSome (!showBasis)
               then (trace (Top, "Type Check SML")
                     Compile.elaborateSML {input = []})
            else if !buildConstants
               then Compile.outputBasisConstants Out.standard
            else if !verbosity = Silent orelse !verbosity = Top
               then printVersion Out.standard
            else outputHeader' (No, Out.standard))
    | Result.Yes (input :: rest) =>
         let
            val _ = inputFile := File.base (File.fileOf input)
            val (start, base) =
               let
                  val rec loop =
                     fn [] => usage (concat ["invalid file suffix on ", input])
                      | (suf, start, hasNum) :: sufs =>
                           if String.hasSuffix (input, {suffix = suf})
                              then (start,
                                    let
                                       val f = File.base input
                                    in
                                       if hasNum
                                          then File.base f
                                       else f
                                    end)
                           else loop sufs
                  datatype z = datatype Place.t
               in
                  loop [(".mlb", MLB, false),
                        (".sml", SML, false),
                        (".c", Generated, true),
                        (".o", O, true)]
               end
            val _ =
               List.foreach
               (rest, fn f =>
                if List.exists ([".c", ".o", ".s", ".S"], fn suffix =>
                                String.hasSuffix (f, {suffix = suffix}))
                   then File.withIn (f, fn _ => ())
                else usage (concat ["invalid file suffix: ", f]))
            val csoFiles = rest
         in
            case Place.compare (start, stop) of
               GREATER => usage (concat ["cannot go from ", Place.toString start,
                                         " to ", Place.toString stop])
             | EQUAL => usage "nothing to do"
             | LESS =>
                  let
                     val _ =
                        if !verbosity = Top
                           then printVersion Out.error
                        else ()
                     val tempFiles: File.t list ref = ref []
                     val tmpDir =
                        let
                           val (tmpVar, default) =
                              case MLton.Platform.OS.host of
                                 MinGW => ("TEMP", "C:/WINDOWS/TEMP")
                               | _ => ("TMPDIR", "/tmp")
                        in
                           case Process.getEnv tmpVar of
                              NONE => default
                            | SOME d => d
                        end
                     fun temp (suf: string): File.t =
                        let
                           val (f, out) =
                              File.temp {prefix = OS.Path.concat (tmpDir, "file"),
                                         suffix = suf}
                           val _ = Out.close out
                           val _ = List.push (tempFiles, f)
                        in
                           f
                        end
                     fun suffix s = concat [base, s]
                     fun maybeOut suf =
                        case !output of
                           NONE => suffix suf
                         | SOME f => f
                     fun maybeOutBase suf =
                        case !output of
                           NONE => suffix suf
                         | SOME f => if File.extension f = SOME "exe"
                                        then concat [File.base f, suf]
                                     else concat [f, suf]
                     val { base = outputBase, ext=_ } =
                        OS.Path.splitBaseExt (maybeOut ".ext")
                     val { file = defLibname, dir=_ } =
                        OS.Path.splitDirFile outputBase
                     val defLibname =
                        if String.hasPrefix (defLibname, {prefix = "lib"})
                        then String.extract (defLibname, 3, NONE)
                        else defLibname
                     fun toAlNum c = if Char.isAlphaNum c then c else #"_"
                     val () =
                        if !libname <> "" then () else
                        libname := CharVector.map toAlNum defLibname
                     (* Library output includes a header by default *)
                     val () =
                        case (!format, !exportHeader) of
                           (Executable, _) => ()
                         | (_, NONE) => exportHeader := SOME (!libname ^ ".h")
                         | _ => ()
                     val _ =
                        atMLtons :=
                        Vector.fromList
                        (maybeOut "" :: tokenize (rev ("--" :: (!runtimeArgs))))
                     (* The -Wa,--gstabs says to pass the --gstabs option to the
                      * assembler. This tells the assembler to generate stabs
                      * debugging information for each assembler line.
                      *)
                     val (gccDebug, asDebug) =
                        case !debugFormat of
                           NONE => (["-g"], "-Wa,-g")
                         | SOME Dwarf => (["-gdwarf", "-g2"], "-Wa,--gdwarf2")
                         | SOME DwarfPlus => (["-gdwarf+", "-g2"], "-Wa,--gdwarf2")
                         | SOME Dwarf2 => (["-gdwarf-2", "-g2"], "-Wa,--gdwarf2")
                         | SOME Stabs => (["-gstabs", "-g2"], "-Wa,--gstabs")
                         | SOME StabsPlus => (["-gstabs+", "-g2"], "-Wa,--gstabs")
                     fun compileO (inputs: File.t list): unit =
                        let
                           val output =
                              case (!format, targetOS) of
                                 (Archive, _) => maybeOut ".a"
                               | (Executable, _) => maybeOut ""
                               | (LibArchive, _) => maybeOut ".a"
                               | (Library, Darwin) => maybeOut ".dylib"
                               | (Library, Cygwin) => !libname ^ ".dll"
                               | (Library, MinGW)  => !libname ^ ".dll"
                               | (Library, _) => maybeOut ".so"
                           val libOpts =
                              case targetOS of
                                 Darwin => [ "-dynamiclib" ]
                               | Cygwin =>  [ "-shared",
                                              "-Wl,--out-implib," ^
                                                 maybeOut ".a",
                                              "-Wl,--output-def," ^
                                                 !libname ^ ".def"]
                               | MinGW =>  [ "-shared",
                                             "-Wl,--out-implib," ^
                                                maybeOut ".a",
                                             "-Wl,--output-def," ^
                                                !libname ^ ".def"]
                               | _ =>      [ "-shared" ]
                           val _ =
                              trace (Top, "Link")
                              (fn () =>
                               if !format = Archive orelse
                                  !format = LibArchive
                               then System.system
                                    (arScript,
                                     List.concat
                                      [[targetStr, OSStr, output],
                                       inputs,
                                       linkArchives])
                               else System.system
                                    (gcc,
                                     List.concat
                                      [["-o", output],
                                       if !format = Library then libOpts else [],
                                       if !debug then gccDebug else [],
                                       inputs,
                                       linkOpts]))
                              ()
                           (* gcc on Cygwin appends .exe, which I don't want, so
                            * move the output file to it's rightful place.
                            * Notice that we do not use targetOS here, since we
                            * care about the platform we're running on, not the
                            * platform we're generating for.
                            *
                            * We want to keep the .exe as is for MinGW/Win32.
                            *)
                           val _ =
                              if MLton.Platform.OS.host = Cygwin
                                 then
                                    if String.contains (output, #".")
                                       then ()
                                    else
                                       File.move {from = concat [output, ".exe"],
                                                  to = output}
                              else ()
                        in
                           ()
                        end
                  fun mkOutputO (c: Counter.t, input: File.t): File.t =
                     if stop = Place.O orelse !keepO
                        then
                           if File.dirOf input = File.dirOf (maybeOutBase ".o")
                              then
                                 concat [File.base input, ".o"]
                              else
                                 maybeOutBase
                                    (concat [".",
                                             Int.toString (Counter.next c),
                                             ".o"])
                        else temp ".o"
                  fun compileC (c: Counter.t, input: File.t): File.t =
                     let
                        val debugSwitches = gccDebug @ ["-DASSERT=1"]
                        val output = mkOutputO (c, input)

                        val _ =
                           System.system
                            (gcc,
                             List.concat
                             [[ "-std=gnu99", "-c" ],
                              if !format = Executable
                              then [] else [ "-DLIBNAME=" ^ !libname ],
                              if positionIndependent
                              then [ "-fPIC", "-DPIC" ] else [],
                              if !debug then debugSwitches else [],
                              ccOpts,
                              ["-o", output],
                              [input]])
                     in
                        output
                     end
                  fun compileS (c: Counter.t, input: File.t): File.t =
                     let
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                           (gcc,
                            List.concat
                            [["-c"],
                             if !debug then [asDebug] else [],
                             asOpts,
                             ["-o", output],
                             [input]])
                     in
                        output
                     end
                  fun compileCSO (inputs: File.t list): unit =
                     if List.forall (inputs, fn f =>
                                     SOME "o" = File.extension f)
                        then compileO inputs
                     else
                     let
                        val c = Counter.new 0
                        val oFiles =
                           trace (Top, "Compile and Assemble")
                           (fn () =>
                            List.fold
                            (inputs, [], fn (input, ac) =>
                             let
                                val extension = File.extension input
                             in
                                if SOME "o" = extension
                                   then input :: ac
                                else if SOME "c" = extension
                                   then (compileC (c, input)) :: ac
                                else if SOME "s" = extension
                                        orelse SOME "S" = extension
                                   then (compileS (c, input)) :: ac
                                else Error.bug
                                     (concat
                                      ["invalid extension: ",
                                       Option.toString (fn s => s) extension])
                             end))
                           ()
                     in
                        case stop of
                           Place.O => ()
                         | _ => compileO (rev oFiles)
                     end
                  fun mkCompileSrc {listFiles, elaborate, compile} input =
                     let
                        val outputs: File.t list ref = ref []
                        val r = ref 0
                        fun make (style: style, suf: string) () =
                           let
                              val suf = concat [".", Int.toString (!r), suf]
                              val _ = Int.inc r
                              val file = (if !keepGenerated
                                             orelse stop = Place.Generated
                                             then maybeOutBase
                                          else temp) suf
                              val _ = List.push (outputs, file)
                              val out = Out.openOut file
                              fun print s = Out.output (out, s)
                              val _ = outputHeader' (style, out)
                              fun done () = Out.close out
                           in
                              {file = file,
                               print = print,
                               done = done}
                           end
                        val _ =
                           case !verbosity of
                              Silent => ()
                            | Top => ()
                            | _ =>
                                 outputHeader
                                 (Control.No, fn l =>
                                  let val out = Out.error
                                  in Layout.output (l, out)
                                     ; Out.newline out
                                  end)
                        val _ =
                           case stop of
                              Place.Files =>
                                 Vector.foreach
                                 (listFiles {input = input}, fn f =>
                                  (print (String.translate
                                          (f, fn #"\\" => "/" | c => str c))
                                   ; print "\n"))
                            | Place.TypeCheck =>
                                 trace (Top, "Type Check SML")
                                 elaborate
                                 {input = input}
                            | _ =>
                                 trace (Top, "Compile SML")
                                 compile
                                 {input = input,
                                  outputC = make (Control.C, ".c"),
                                  outputS = make (Control.Assembly, ".s")}
                     in
                        case stop of
                           Place.Files => ()
                         | Place.TypeCheck => ()
                         | Place.Generated => ()
                         | _ =>
                              (* Shrink the heap before calling gcc. *)
                              (MLton.GC.pack ()
                               ; compileCSO (List.concat [!outputs, csoFiles]))
                     end
                  val compileSML =
                     mkCompileSrc {listFiles = fn {input} => Vector.fromList input,
                                   elaborate = Compile.elaborateSML,
                                   compile = Compile.compileSML}
                  val compileMLB =
                     mkCompileSrc {listFiles = Compile.sourceFilesMLB,
                                   elaborate = Compile.elaborateMLB,
                                   compile = Compile.compileMLB}
                  fun compile () =
                     case start of
                        Place.SML => compileSML [input]
                      | Place.MLB => compileMLB input
                      | Place.Generated => compileCSO (input :: csoFiles)
                      | Place.O => compileCSO (input :: csoFiles)
                      | _ => Error.bug "invalid start"
                  val doit
                    = trace (Top, "MLton")
                      (fn () =>
                       Exn.finally
                       (compile, fn () =>
                        List.foreach (!tempFiles, File.remove)))
               in
                  doit ()
               end
         end
   end

val commandLine = Process.makeCommandLine commandLine

val main = fn (_, args) => commandLine args

val mainWrapped = fn () => OS.Process.exit (commandLine (CommandLine.arguments ()))

end
