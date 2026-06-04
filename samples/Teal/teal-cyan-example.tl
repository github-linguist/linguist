--[[
Copied from: https://raw.githubusercontent.com/teal-language/cyan/refs/heads/main/src/cyan/commands/build.tl
--]]

---@nodoc

local argparse <const> = require("argparse")
local tl <const> = require("tl")

local command <const> = require("cyan.command")
local common <const> = require("cyan.tlcommon")
local config <const> = require("cyan.config")
local decoration <const> = require("cyan.decoration")
local fs <const> = require("cyan.fs")
local graph <const> = require("cyan.graph")
local invocation_context <const> = require("cyan.invocation-context")
local lexical_path <const> = require("lexical-path")
local log <const> = require("cyan.log")
local script <const> = require("cyan.script")
local util <const> = require("cyan.util")

local ivalues <const> = util.tab.ivalues

local function exists_and_is_dir(prefix: string, p: lexical_path.Path): boolean
   if not fs.exists(p) then
      log.err(prefix, " \"", decoration.file_name(p), "\" does not exist")
      return false
   end
   if not fs.is_directory(p) then
      log.err(prefix, " \"", decoration.file_name(p), "\" is not a directory")
      return false
   end
   return true
end

-- in the event that an out of project dependency has a type error, we should report it
local function report_dep_errors(env: tl.Env, source_dir: lexical_path.Path): boolean
   local ok = true
   for name in ivalues(env.loaded_order) do
      local res <const> = env.loaded[name]
      if not lexical_path.from_os(res.filename):is_in(source_dir) then
         if (res.syntax_errors and #res.syntax_errors > 0) or #res.type_errors > 0 then
            if (res.syntax_errors and #res.syntax_errors > 0) then
               common.report_errors(log.err, res.syntax_errors, res.filename, "(Out of project) syntax error")
            end
            if #res.type_errors > 0 then
               common.report_errors(log.err, res.type_errors, res.filename, "(Out of project) type error")
            end
            ok = false
         end
      end
   end
   return ok
end

local function build(args: command.Args, loaded_config: config.Config, context: invocation_context.InvocationContext): integer
   if not loaded_config.loaded_from then
      log.err(config.filename, " not found")
      return 1
   end

   local source_dir <const> = loaded_config.source_dir and loaded_config.source_dir:copy() or lexical_path.from_unix(".")
   if not exists_and_is_dir("Source dir", source_dir) then
      return 1
   end

   local build_dir <const> = loaded_config.build_dir and loaded_config.build_dir:copy() or lexical_path.from_unix(".")
   if not fs.exists(build_dir) then
      local succ <const>, err <const> = fs.make_directory(build_dir)
      if not succ then
         log.err("Failed to create build dir ", decoration.file_name(build_dir), ": ", err)
         return 1
      end
   elseif not fs.is_directory(build_dir) then
      log.err("Build dir \"", decoration.file_name(build_dir), "\" is not a directory")
      return 1
   end

   local current_dir <const> = fs.current_directory()
   local function ensure_abs_path(p: lexical_path.Path): lexical_path.Path
      if p.is_absolute then return p end
      return current_dir .. p
   end

   local abs_build_dir <const> = ensure_abs_path(build_dir)
   local abs_source_dir <const> = ensure_abs_path(source_dir)

   local env <const>, env_err <const> = common.init_env_from_config(loaded_config)
   if not env then
      log.err("Could not initialize Teal environment:\n", env_err)
      return 1
   end

   if not script.emit_hook(context, "pre") then
      return 1
   end

   local include <const> = loaded_config.include or {}
   local exclude <const> = loaded_config.exclude and { table.unpack(loaded_config.exclude) } or {}
   if abs_source_dir == context.initial_directory then
      table.insert(exclude, lexical_path.parse_pattern "tlconfig.lua")
   end
   local dont_write_lua_files <const> = source_dir == build_dir

   local dag <const>, cycles <const> = graph.scan_directory(source_dir, include, exclude)
   if not dag then
      log.err(
         "Circular dependency detected in the following files:\n   ",
         table.unpack(util.tab.intersperse(cycles, "\n   "))
      )
      return 1
   end
   local exit = 0

   if log.debug:should_log() then
      log.debug("Built dependency graph")
      for k, v in pairs(dag._nodes_by_filename) do
         if not next(v.dependents) then
            log.debug:cont("   ", k, " has no dependents")
         else
            log.debug:cont("   ", k, " has dependents:")
            for dependent in pairs(v.dependents) do
               log.debug:cont("      ", dependent.input:to_string())
            end
         end
      end
   end

   local function display_filename(f: lexical_path.Path, trailing_slash?: boolean): decoration.Decorated
      return decoration.file_name(assert(ensure_abs_path(f):relative_to(context.initial_directory)):to_string() .. (trailing_slash and fs.path_separator or ""))
   end

   local function get_output_name(src: lexical_path.Path): lexical_path.Path
      local out <const> = build_dir .. assert(src:relative_to(source_dir))
      local ext <const> = out:extension():lower()
      if ext:lower() == "tl" then
         out[#out] = out[#out]:sub(1, -#ext - 2) .. ".lua"
      end
      return out
   end

   local function source_is_newer(src: lexical_path.Path): boolean
      local target <const> = get_output_name(src)
      local newer: boolean
      if args.update_all then
         newer = true
      else
         local in_t <const>, out_t <const> = fs.mod_time(src), fs.mod_time(target) or -1
         newer = in_t > out_t
      end
      if newer then
         log.extra("Source ", display_filename(src), " is newer than target (", display_filename(target), ")")
         if not script.emit_hook(context, "file_updated", src:copy()) then
            exit = 1
            coroutine.yield()
         end
      end
      return newer
   end

   if not coroutine.wrap(function(): boolean
      dag:mark_each(source_is_newer)
      return true
   end)() then
      return exit
   end

   local type_check_options <const> = {
      feat_lax = "off",
      feat_arity = loaded_config.feat_arity,

      gen_compat = loaded_config.gen_compat,
      gen_target = loaded_config.gen_target,
   }

   local to_write <const> = {}
   local function process_node(n: graph.Node, compile: boolean)
      local path <const> = n.input:to_string()
      local disp_path <const> = display_filename(n.input)
      log.debug("processing node of ", disp_path, " for ", compile and "compilation" or "type check")
      local out <const> = get_output_name(n.input)
      n.output = out
      local parsed <const>, parse_err <const> = common.parse_file(path)
      if not parsed then
         log.err("Could not parse ", disp_path, ":\n   ", parse_err)
         exit = 1
         return
      end
      if #parsed.errs > 0 then
         common.report_errors(log.err, parsed.errs, path, "syntax error")
         exit = 1
         return
      end

      local result <const>, check_ast_err <const> = tl.check(parsed.ast, path, type_check_options, env)
      if not result then
         log.err("Could not type check ", disp_path, ":\n   ", check_ast_err)
         exit = 1
         return
      end

      if not common.report_result(result, loaded_config) then
         exit = 1
         return
      end

      log.info("Type checked ", disp_path)
      if args.check_only then
         return
      end

      local is_lua <const> = n.input:extension():lower() == "lua"
      if compile and not (is_lua and dont_write_lua_files) then
         local ok <const>, err <const> = fs.make_parent_directories(n.output)
         if ok then
            table.insert(to_write, {n, parsed.ast})
         else
            log.err("Unable to create parent dirs to ", display_filename(n.output), ":", err)
            exit = 1
         end
      end
   end

   for n in dag:marked_nodes() do
      process_node(n, n.mark == "compile")
   end

   if exit ~= 0 then
      report_dep_errors(env, source_dir)
      return exit
   end

   if args.check_only then
      return exit
   end

   for node_ast in ivalues(to_write) do
      local n <const>, ast <const> = node_ast[1], node_ast[2]
      local fh <const>, err <const> = io.open(n.output:to_string(), "w")
      if not fh then
         log.err("Error opening file ", display_filename(n.output), ": ", err)
         exit = 1
      else
         local generated <const>, gen_err <const> = tl.generate(ast, loaded_config.gen_target) -- TODO: gen_options
         if generated then
            fh:write(generated, "\n")
            fh:close()
            log.info("Wrote ", display_filename(n.output))
         else
            log.err("Error when generating lua for ", display_filename(n.output), "\n", gen_err)
            exit = 1
         end
      end
   end

   if #to_write > 0 then
      if not script.emit_hook(context, "post") then
         return 1
      end
   end

   if build_dir ~= source_dir then
      local expected_files <const> = {}
      for n in dag:nodes() do
         log.debug(display_filename(n.input), " -> ", display_filename(get_output_name(n.input)))
         local p <const> = assert(get_output_name(n.input):remove_leading(build_dir))
         expected_files[p:to_string()] = true
         for ancestor in p:ancestors() do
            expected_files[ancestor:to_string()] = true
         end
      end

      local unexpected_files <const>: {lexical_path.Path} = {}
      local unexpected_directories <const>: {lexical_path.Path} = {}
      local dont_prune <const> = util.tab.map(loaded_config.dont_prune or {}, lexical_path.parse_pattern)
      for p in fs.scan_directory(build_dir, nil, nil, true) do
         log.debug("checking if ", p:to_string(), " is expected...")
         local full <const> = build_dir .. p
         local pattern_index <const> = fs.match_any(full, dont_prune)
         if pattern_index then
            log.debug("   yes (ignored by pattern ", dont_prune[pattern_index], ")")
         elseif expected_files[p:to_string()] then
            log.debug("   yes")
         else
            log.debug("   no")
            table.insert(fs.is_directory(full) and unexpected_directories or unexpected_files, p)
         end
      end

      -- sort directory names by length so that more nested directories are removed first
      table.sort(unexpected_directories, function(a: lexical_path.Path, b: lexical_path.Path): boolean
         local a_str <const> = a:to_string()
         local b_str <const> = b:to_string()
         local a_len <const> = #a_str
         local b_len <const> = #b_str
         if a_len ~= b_len then
            return a_len > b_len
         end
         return a_str > b_str
      end)

      if #unexpected_files > 0 or #unexpected_directories > 0 then
         if args.prune then
            local cwd <const> = fs.current_directory()
            local function prune(p: lexical_path.Path, kind: string)
               local file <const> = abs_build_dir .. p
               local disp <const> = display_filename(file)
               local real <const> = assert(file:relative_to(cwd))
               local ok <const>, err <const> = os.remove(real:to_string())
               if ok then
                  log.info("Pruned ", kind, " ", disp)
               else
                  log.err("Unable to prune ", kind, " '", disp, "': ", err)
               end
            end
            for p in ivalues(unexpected_files) do
               prune(p, "file")
            end
            for p in ivalues(unexpected_directories) do
               prune(p, "directory")
            end
         else
            local strs <const>: {any} = {}
            for p in ivalues(unexpected_files) do
               table.insert(strs, "\n   ")
               table.insert(strs, display_filename(p))
            end
            for p in ivalues(unexpected_directories) do
               table.insert(strs, "\n   ")
               table.insert(strs, display_filename(p, true))
            end
            table.insert(strs, "\nhint: use `cyan build --prune` to automatically delete these files")
            log.warn("Unexpected files in build directory:", table.unpack(strs))
         end
      end
   end

   if not report_dep_errors(env, source_dir) then
      log.warn("There were errors in out of project files. Your project may not work as expected.")
   end

   return exit
end

command.new{
   name = "build",
   description = [[Build a project based on tlconfig.lua.]],
   exec = build,
   argparse = function(cmd: argparse.Command)
      cmd:flag("-u --update-all", "Force recompilation of every file in your project.")
      cmd:flag("-c --check-only", "Only type check files.")
      cmd:flag("-p --prune", "Remove any unexpected files in the build directory.")
   end,
   script_hooks = { "pre", "post", "file_updated" },
}