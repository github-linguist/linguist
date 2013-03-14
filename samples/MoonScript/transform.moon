
types = require "moonscript.types"
util = require "moonscript.util"
data = require "moonscript.data"

import reversed, unpack from util
import ntype, mtype, build, smart_node, is_slice, value_is_singular from types
import insert from table
import NameProxy, LocalName from require "moonscript.transform.names"

destructure = require "moonscript.transform.destructure"

local implicitly_return

class Run
  new: (@fn) =>
    self[1] = "run"

  call: (state) =>
    self.fn state

-- transform the last stm is a list of stms
-- will puke on group
apply_to_last = (stms, fn) ->
  -- find last (real) exp
  last_exp_id = 0
  for i = #stms, 1, -1
    stm = stms[i]
    if stm and mtype(stm) != Run
      last_exp_id = i
      break

  return for i, stm in ipairs stms
    if i == last_exp_id
      fn stm
    else
      stm

-- is a body a sindle expression/statement
is_singular = (body) ->
  return false if #body != 1
  if "group" == ntype body
    is_singular body[2]
  else
    true

find_assigns = (body, out={}) ->
  for thing in *body
    switch thing[1]
      when "group"
        find_assigns thing[2], out
      when "assign"
        table.insert out, thing[2] -- extract names
  out

hoist_declarations = (body) ->
  assigns = {}

  -- hoist the plain old assigns
  for names in *find_assigns body
    for name in *names
      table.insert assigns, name if type(name) == "string"

  -- insert after runs
  idx = 1
  while mtype(body[idx]) == Run do idx += 1

  table.insert body, idx, {"declare", assigns}

expand_elseif_assign = (ifstm) ->
  for i = 4, #ifstm
    case = ifstm[i]
    if ntype(case) == "elseif" and ntype(case[2]) == "assign"
      split = { unpack ifstm, 1, i - 1 }
      insert split, {
        "else", {
          {"if", case[2], case[3], unpack ifstm, i + 1}
        }
      }
      return split

  ifstm

constructor_name = "new"

with_continue_listener = (body) ->
  continue_name = nil
  {
    Run =>
      @listen "continue", ->
        unless continue_name
          continue_name = NameProxy"continue"
          @put_name continue_name
        continue_name

    build.group body

    Run =>
      return unless continue_name
      @put_name continue_name, nil
      @splice (lines) -> {
        {"assign", {continue_name}, {"false"}}
        {"repeat", "true", {
          lines
          {"assign", {continue_name}, {"true"}}
        }}
        {"if", {"not", continue_name}, {
          {"break"}
        }}
      }
  }


class Transformer
  new: (@transformers) =>
    @seen_nodes = setmetatable {}, __mode: "k"

  transform: (scope, node, ...) =>
    return node if @seen_nodes[node]
    @seen_nodes[node] = true
    while true
      transformer = @transformers[ntype node]
      res = if transformer
        transformer(scope, node, ...) or node
      else
        node
      return node if res == node
      node = res
    node

  bind: (scope) =>
    (...) -> @transform scope, ...

  __call: (...) => @transform ...

  can_transform: (node) =>
    @transformers[ntype node] != nil

construct_comprehension = (inner, clauses) ->
  current_stms = inner
  for _, clause in reversed clauses
    t = clause[1]
    current_stms = if t == "for"
      _, names, iter = unpack clause
      {"foreach", names, {iter}, current_stms}
    elseif t == "when"
      _, cond = unpack clause
      {"if", cond, current_stms}
    else
      error "Unknown comprehension clause: "..t
    current_stms = {current_stms}

  current_stms[1]

Statement = Transformer {
  root_stms: (body) =>
    apply_to_last body, implicitly_return @

  assign: (node) =>
    names, values = unpack node, 2
    -- bubble cascading assigns
    transformed = if #values == 1
      value = values[1]
      t = ntype value

      if t == "decorated"
        value = @transform.statement value
        t = ntype value

      if types.cascading[t]
        ret = (stm) ->
          if types.is_value stm
            {"assign", names, {stm}}
          else
            stm

        build.group {
          {"declare", names}
          @transform.statement value, ret, node
        }

    node = transformed or node

    if destructure.has_destructure names
      return destructure.split_assign node

    node

  continue: (node) =>
    continue_name = @send "continue"
    error "continue must be inside of a loop" unless continue_name
    build.group {
      build.assign_one continue_name, "true"
      {"break"}
    }

  export: (node) =>
    -- assign values if they are included
    if #node > 2
      if node[2] == "class"
        cls = smart_node node[3]
        build.group {
          {"export", {cls.name}}
          cls
        }
      else
        build.group {
          node
          build.assign {
            names: node[2]
            values: node[3]
          }
        }
    else
      nil

  update: (node) =>
    _, name, op, exp = unpack node
    op_final = op\match "^(.+)=$"
    error "Unknown op: "..op if not op_final
    exp = {"parens", exp} unless value_is_singular exp
    build.assign_one name, {"exp", name, op_final, exp}

  import: (node) =>
    _, names, source = unpack node

    stubs = for name in *names
      if type(name) == "table"
        name
      else
        {"dot", name}

    real_names = for name in *names
      type(name) == "table" and name[2] or name

    if type(source) == "string"
      build.assign {
        names: real_names
        values: [build.chain { base: source, stub} for stub in *stubs]
      }
    else
      source_name = NameProxy "table"
      build.group {
        {"declare", real_names}
        build["do"] {
          build.assign_one source_name, source
          build.assign {
            names: real_names
            values: [build.chain { base: source_name, stub} for stub in *stubs]
          }
        }
      }

  comprehension: (node, action) =>
    _, exp, clauses = unpack node

    action = action or (exp) -> {exp}
    construct_comprehension action(exp), clauses

  do: (node, ret) =>
    node[2] = apply_to_last node[2], ret if ret
    node

  decorated: (node) =>
    stm, dec = unpack node, 2

    wrapped = switch dec[1]
      when "if"
        cond, fail = unpack dec, 2
        fail = { "else", { fail } } if fail
        { "if", cond, { stm }, fail }
      when "unless"
        { "unless", dec[2], { stm } }
      when "comprehension"
        { "comprehension", stm, dec[2] }
      else
        error "Unknown decorator " .. dec[1]

    if ntype(stm) == "assign"
      wrapped = build.group {
        build.declare names: [name for name in *stm[2] when type(name) == "string"]
        wrapped
      }

    wrapped

  unless: (node) =>
    { "if", {"not", {"parens", node[2]}}, unpack node, 3 }

  if: (node, ret) =>
    -- expand assign in cond
    if ntype(node[2]) == "assign"
      _, assign, body = unpack node
      if destructure.has_destructure assign[2]
        name = NameProxy "des"

        body = {
          destructure.build_assign assign[2][1], name
          build.group node[3]
        }

        return build.do {
          build.assign_one name, assign[3][1]
          {"if", name, body, unpack node, 4}
        }
      else
        name = assign[2][1]
        return build["do"] {
          assign
          {"if", name, unpack node, 3}
        }

    node = expand_elseif_assign node

    -- apply cascading return decorator
    if ret
      smart_node node
      -- mutate all the bodies
      node['then'] = apply_to_last node['then'], ret
      for i = 4, #node
        case = node[i]
        body_idx = #node[i]
        case[body_idx] = apply_to_last case[body_idx], ret

    node

  with: (node, ret) =>
    _, exp, block = unpack node

    scope_name = NameProxy "with"

    named_assign = if ntype(exp) == "assign"
      names, values = unpack exp, 2
      assign_name = names[1]
      exp = values[1]
      values[1] = scope_name
      {"assign", names, values}

    build.do {
      Run => @set "scope_var", scope_name
      build.assign_one scope_name, exp
      build.group { named_assign }
      build.group block

      if ret
        ret scope_name
    }

  foreach: (node) =>
    smart_node node
    source = unpack node.iter

    destructures = {}
    node.names = for i, name in ipairs node.names
      if ntype(name) == "table"
        with proxy = NameProxy "des"
          insert destructures, destructure.build_assign name, proxy
      else
        name

    if next destructures
      insert destructures, build.group node.body
      node.body = destructures

    if ntype(source) == "unpack"
      list = source[2]

      index_name = NameProxy "index"
      list_name = NameProxy "list"

      slice_var = nil
      bounds = if is_slice list
        slice = list[#list]
        table.remove list
        table.remove slice, 1

        slice[2] = if slice[2] and slice[2] != ""
          max_tmp_name = NameProxy "max"
          slice_var = build.assign_one max_tmp_name, slice[2]
          {"exp", max_tmp_name, "<", 0
            "and", {"length", list_name}, "+", max_tmp_name
            "or", max_tmp_name }
        else
          {"length", list_name}

        slice
      else
        {1, {"length", list_name}}

      return build.group {
        build.assign_one list_name, list
        slice_var
        build["for"] {
          name: index_name
          bounds: bounds
          body: {
            {"assign", node.names, {list_name\index index_name}}
            build.group node.body
          }
        }
      }

    node.body = with_continue_listener node.body

  while: (node) =>
    smart_node node
    node.body = with_continue_listener node.body

  for: (node) =>
    smart_node node
    node.body = with_continue_listener node.body

  switch: (node, ret) =>
    _, exp, conds = unpack node
    exp_name = NameProxy "exp"

    -- convert switch conds into if statment conds
    convert_cond = (cond) ->
      t, case_exps, body = unpack cond
      out = {}
      insert out, t == "case" and "elseif" or "else"
      if  t != "else"
        cond_exp = {}
        for i, case in ipairs case_exps
          if i == 1
            insert cond_exp, "exp"
          else
            insert cond_exp, "or"

          case = {"parens", case} unless value_is_singular case
          insert cond_exp, {"exp", case, "==", exp_name}

        insert out, cond_exp
      else
        body = case_exps

      if ret
        body = apply_to_last body, ret

      insert out, body

      out

    first = true
    if_stm = {"if"}
    for cond in *conds
      if_cond = convert_cond cond
      if first
        first = false
        insert if_stm, if_cond[2]
        insert if_stm, if_cond[3]
      else
        insert if_stm, if_cond

    build.group {
      build.assign_one exp_name, exp
      if_stm
    }

  class: (node, ret, parent_assign) =>
    _, name, parent_val, body = unpack node

    -- split apart properties and statements
    statements = {}
    properties = {}
    for item in *body
      switch item[1]
        when "stm"
          insert statements, item[2]
        when "props"
          for tuple in *item[2,]
            if ntype(tuple[1]) == "self"
              insert statements, build.assign_one unpack tuple
            else
              insert properties, tuple

    -- find constructor
    constructor = nil
    properties = for tuple in *properties
      key = tuple[1]
      if key[1] == "key_literal" and key[2] == constructor_name
        constructor = tuple[2]
        nil
      else
        tuple

    parent_cls_name = NameProxy "parent"
    base_name = NameProxy "base"
    self_name = NameProxy "self"
    cls_name = NameProxy "class"

    if not constructor
      constructor = build.fndef {
        args: {{"..."}}
        arrow: "fat"
        body: {
          build["if"] {
            cond: parent_cls_name
            then: {
              build.chain { base: "super", {"call", {"..."}} }
            }
          }
        }
      }
    else
      smart_node constructor
      constructor.arrow = "fat"

    real_name = name or parent_assign and parent_assign[2][1]
    real_name = switch ntype real_name
      when "chain"
        last = real_name[#real_name]
        switch ntype last
          when "dot"
            {"string", '"', last[2]}
          when "index"
            last[2]
          else
            "nil"
      when "nil"
        "nil"
      else
        {"string", '"', real_name}

    cls = build.table {
      {"__init", constructor}
      {"__base", base_name}
      {"__name", real_name} -- "quote the string"
      {"__parent", parent_cls_name}
    }

    -- look up a name in the class object
    class_lookup = build["if"] {
      cond: {"exp", "val", "==", "nil", "and", parent_cls_name}
      then: {
        parent_cls_name\index"name"
      }
    }
    insert class_lookup, {"else", {"val"}}

    cls_mt = build.table {
      {"__index", build.fndef {
        args: {{"cls"}, {"name"}}
        body: {
          build.assign_one LocalName"val", build.chain {
            base: "rawget", {"call", {base_name, "name"}}
          }
          class_lookup
        }
      }}
      {"__call", build.fndef {
        args: {{"cls"}, {"..."}}
        body: {
          build.assign_one self_name, build.chain {
            base: "setmetatable"
            {"call", {"{}", base_name}}
          }
          build.chain {
            base: "cls.__init"
            {"call", {self_name, "..."}}
          }
          self_name
        }
      }}
    }

    cls = build.chain {
      base: "setmetatable"
      {"call", {cls, cls_mt}}
    }

    value = nil
    with build
      out_body = {
        Run =>
          -- make sure we don't assign the class to a local inside the do
          @put_name name if name

          @set "super", (block, chain) ->
            if chain
              slice = [item for item in *chain[3,]]
              new_chain = {"chain", parent_cls_name}

              head = slice[1]

              if head == nil
                return parent_cls_name

              switch head[1]
                -- calling super, inject calling name and self into chain
                when "call"
                  calling_name = block\get"current_block"
                  slice[1] = {"call", {"self", unpack head[2]}}

                  if ntype(calling_name) == "key_literal"
                    insert new_chain, {"dot", calling_name[2]}
                  else
                    insert new_chain, {"index", calling_name}

                -- colon call on super, replace class with self as first arg
                when "colon"
                  call = head[3]
                  insert new_chain, {"dot", head[2]}
                  slice[1] = { "call", { "self", unpack call[2] } }

              insert new_chain, item for item in *slice

              new_chain
            else
              parent_cls_name

        .assign_one parent_cls_name, parent_val == "" and "nil" or parent_val
        .assign_one base_name, {"table", properties}
        .assign_one base_name\chain"__index", base_name

        .if {
          cond: parent_cls_name
          then: {
            .chain {
              base: "setmetatable"
              {"call", {
                base_name,
                .chain { base: parent_cls_name,  {"dot", "__base"}}
              }}
            }
          }
        }

        .assign_one cls_name, cls
        .assign_one base_name\chain"__class", cls_name

        .group if #statements > 0 then {
          .assign_one LocalName"self", cls_name
          .group statements
        }

        -- run the inherited callback
        .if {
          cond: {"exp",
            parent_cls_name, "and", parent_cls_name\chain "__inherited"
          }
          then: {
            parent_cls_name\chain "__inherited", {"call", {
              parent_cls_name, cls_name
            }}
          }
        }

        .group if name then {
          .assign_one name, cls_name
        }

        if ret
          ret cls_name
      }

      hoist_declarations out_body

      value = .group {
        .group if ntype(name) == "value" then {
          .declare names: {name}
        }

        .do out_body
      }

    value
}

class Accumulator
  body_idx: { for: 4, while: 3, foreach: 4 }

  new: =>
    @accum_name = NameProxy "accum"
    @value_name = NameProxy "value"
    @len_name = NameProxy "len"

  -- wraps node and mutates body
  convert: (node) =>
    index = @body_idx[ntype node]
    node[index] = @mutate_body node[index]
    @wrap node

  -- wrap the node into a block_exp
  wrap: (node) =>
    build.block_exp {
      build.assign_one @accum_name, build.table!
      build.assign_one @len_name, 0
      node
      @accum_name
    }

  -- mutates the body of a loop construct to save last value into accumulator
  -- can optionally skip nil results
  mutate_body: (body, skip_nil=true) =>
    val = if not skip_nil and is_singular body
      with body[1]
        body = {}
    else
      body = apply_to_last body, (n) ->
        if types.is_value n
          build.assign_one @value_name, n
        else
          -- just ignore it
          build.group {
            {"declare", {@value_name}}
            n
          }
      @value_name

    update = {
      {"update", @len_name, "+=", 1}
      build.assign_one @accum_name\index(@len_name), val
    }

    if skip_nil
      table.insert body, build["if"] {
        cond: {"exp", @value_name, "!=", "nil"}
        then: update
      }
    else
      table.insert body, build.group update

    body

default_accumulator = (node) =>
  Accumulator!\convert node

implicitly_return = (scope) ->
  is_top = true
  fn = (stm) ->
    t = ntype stm

    -- expand decorated
    if t == "decorated"
      stm = scope.transform.statement stm
      t = ntype stm

    if types.cascading[t]
      is_top = false
      scope.transform.statement stm, fn
    elseif types.manual_return[t] or not types.is_value stm
      -- remove blank return statement
      if is_top and t == "return" and stm[2] == ""
        nil
      else
        stm
    else
      if t == "comprehension" and not types.comprehension_has_value stm
        stm
      else
        {"return", stm}

  fn

Value = Transformer {
  for: default_accumulator
  while: default_accumulator
  foreach: default_accumulator

  do: (node) =>
    build.block_exp node[2]

  decorated: (node) =>
    @transform.statement node

  class: (node) =>
    build.block_exp { node }

  string: (node) =>
    delim = node[2]

    convert_part = (part) ->
      if type(part) == "string" or part == nil
        {"string", delim, part or ""}
      else
        build.chain { base: "tostring", {"call", {part[2]}} }

    -- reduced to single item
    if #node <= 3
      return if type(node[3]) == "string"
        node
      else
        convert_part node[3]

    e = {"exp", convert_part node[3]}

    for i=4, #node
      insert e, ".."
      insert e, convert_part node[i]
    e

  comprehension: (node) =>
    a = Accumulator!
    node = @transform.statement node, (exp) ->
      a\mutate_body {exp}, false
    a\wrap node

  tblcomprehension: (node) =>
    _, explist, clauses = unpack node
    key_exp, value_exp = unpack explist

    accum = NameProxy "tbl"

    inner = if value_exp
      dest = build.chain { base: accum, {"index", key_exp} }
      { build.assign_one dest, value_exp }
    else
      -- If we only have single expression then
      -- unpack the result into key and value
      key_name, val_name = NameProxy"key", NameProxy"val"
      dest = build.chain { base: accum, {"index", key_name} }
      {
        build.assign names: {key_name, val_name}, values: {key_exp}
        build.assign_one dest, val_name
      }

    build.block_exp {
      build.assign_one accum, build.table!
      construct_comprehension inner, clauses
      accum
    }

  fndef: (node) =>
    smart_node node
    node.body = apply_to_last node.body, implicitly_return self
    node.body = {
      Run => @listen "varargs", -> -- capture event
      unpack node.body
    }

    node

  if: (node) => build.block_exp { node }
  unless: (node) =>build.block_exp { node }
  with: (node) => build.block_exp { node }
  switch: (node) =>
    build.block_exp { node }

  -- pull out colon chain
  chain: (node) =>
    stub = node[#node]

    -- escape lua keywords used in dot accessors
    for i=3,#node
      part = node[i]
      if ntype(part) == "dot" and data.lua_keywords[part[2]]
        node[i] = { "index", {"string", '"', part[2]} }

    if ntype(node[2]) == "string"
      -- add parens if callee is raw string
      node[2] = {"parens", node[2] }
    elseif type(stub) == "table" and stub[1] == "colon_stub"
      -- convert colon stub into code
      table.remove node, #node

      base_name = NameProxy "base"
      fn_name = NameProxy "fn"

      is_super = node[2] == "super"
      @transform.value build.block_exp {
        build.assign {
          names: {base_name}
          values: {node}
        }

        build.assign {
          names: {fn_name}
          values: {
            build.chain { base: base_name, {"dot", stub[2]} }
          }
        }

        build.fndef {
          args: {{"..."}}
          body: {
            build.chain {
              base: fn_name, {"call", {is_super and "self" or base_name, "..."}}
            }
          }
        }
      }

  block_exp: (node) =>
    _, body = unpack node

    fn = nil
    arg_list = {}

    fn = smart_node build.fndef body: {
      Run =>
        @listen "varargs", ->
          insert arg_list, "..."
          insert fn.args, {"..."}
          @unlisten "varargs"

      unpack body
    }

    build.chain { base: {"parens", fn}, {"call", arg_list} }
}

{ :Statement, :Value, :Run }
