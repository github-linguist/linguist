% This is an -*- erlang -*- file.
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

{application, compiler,
    [{description, "ERTS  CXC 138 10"},
        {vsn, "8.2.2"},
        {modules, [
            beam_a,
            beam_asm,
            beam_bounds,
            beam_block,
            beam_call_types,
            beam_clean,
            beam_dict,
            beam_digraph,
            beam_disasm,
            beam_flatten,
            beam_jump,
            beam_kernel_to_ssa,
            beam_listing,
            beam_opcodes,
            beam_ssa,
            beam_ssa_bc_size,
            beam_ssa_bool,
            beam_ssa_bsm,
            beam_ssa_codegen,
            beam_ssa_dead,
            beam_ssa_lint,
            beam_ssa_opt,
            beam_ssa_pp,
            beam_ssa_pre_codegen,
            beam_ssa_recv,
            beam_ssa_share,
            beam_ssa_throw,
            beam_ssa_type,
            beam_trim,
            beam_types,
            beam_utils,
            beam_validator,
            beam_z,
            cerl,
            cerl_clauses,
            cerl_inline,
            cerl_trees,
            compile,
            core_scan,
            core_lint,
            core_parse,
            core_pp,
            core_lib,
            erl_bifs,
            rec_env,
            sys_core_alias,
            sys_core_bsm,
            sys_core_fold,
            sys_core_fold_lists,
            sys_core_inline,
            sys_core_prepare,
            sys_messages,
            sys_pre_attributes,
            v3_core,
            v3_kernel,
            v3_kernel_pp
        ]},
        {registered, []},
        {applications, [kernel, stdlib]},
        {env, []},
        {runtime_dependencies, ["stdlib-4.0","kernel-8.4","erts-13.0",
"crypto-5.1"]}]}.
