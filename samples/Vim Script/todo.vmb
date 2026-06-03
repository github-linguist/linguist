" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/todo.txt	[[[1
50
Todo plugin for Vim:

This Vim plugin is based on vim-task (github.com/samsonw/vim-task).
It helps managing todo lists within Vim in a very basic but efficient way.

The latest version of todo can be found here: cdsoft.fr/todo

Contributions are possible on GitHub: https://github.com/CDSoft.fr/todo

Installation:

    Todo is distributed as a Vimball archive.
    + download todo.vmb
    + open this file with vim and type « :so % »

Usage:

    File type:

    ✓ file named *.todo or *todo.txt are open with the todo plugin.

    Syntax:

    ✓ lines ending with ':' are project titles
    + lines starting with '+' are urgent tasks
    - lines starting with '-' are pending tasks (less urgent)
    ✓ lines starting with '✓' are completed tasks
    ? lines starting with '?' are questions
    Some "todo" words are highlighted
    Other lines remain unformated

    Shortcuts:

    Shortcuts are key sequences starting with <Leader>. The <Leader> key is
    '\' by default but I personnally prefer '²' which makes Todo shortcuts
    easier to type on a french keyboard.
    You can add « let mapleader="²" » to the .vimrc file or define any
    other key that suits you best.

    ✓ \TAB toggles the line status between "pending" and "completed"
    ✓ \& toggles the line status between "urgent" and "completed"
    ✓ \q toggles the line status between "question" and "unformated"

License:

    Copyright © 2013, 2016 Christophe Delord (cdsoft.fr)
    This work is free. You can redistribute it and/or modify it under the
    terms of the Do What The Fuck You Want To Public License, Version 2,
    as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.

syntax/todo.vim	[[[1
46
" Copyright © 2013, 2016 Christophe Delord (cdsoft.fr)
" This work is free. You can redistribute it and/or modify it under the
" terms of the Do What The Fuck You Want To Public License, Version 2,
" as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.

if exists("b:current_syntax")
  finish
endif

syntax keyword taskKeyword New new Working working Done done Todo TODO todo bug Bug TBC TBD

syntax match taskWorkingIcon "^-" contained
syntax match taskWorkingIcon "^\s*-" contained
syntax match taskUrgentIcon "^+" contained
syntax match taskUrgentIcon "^\s*+" contained
syntax match taskDoneIcon "^✓" contained
syntax match taskDoneIcon "^\s*✓" contained
syntax match taskQuestionIcon "^?" contained
syntax match taskQuestionIcon "^\s*?" contained

syntax match taskWorkingItem "^-.*" contains=taskWorkingIcon,taskKeyword
syntax match taskWorkingItem "^\s*-.*" contains=taskWorkingIcon,taskKeyword
syntax match taskUrgentItem "^+.*" contains=taskUrgentIcon,taskKeyword
syntax match taskUrgentItem "^\s*+.*" contains=taskUrgentIcon,taskKeyword
syntax match taskDoneItem "^✓.*" contains=taskDoneIcon,taskKeyword
syntax match taskDoneItem "^\s*✓.*" contains=taskDoneIcon,taskKeyword
syntax match taskQuestionItem "^?.*" contains=taskQuestionIcon,taskKeyword
syntax match taskQuestionItem "^\s*?.*" contains=taskQuestionIcon,taskKeyword

highlight taskKeyword guifg=black guibg=yellow gui=NONE ctermfg=black ctermbg=yellow cterm=NONE

highlight taskWorkingItem guifg=red guibg=NONE gui=NONE ctermfg=red ctermbg=NONE cterm=NONE
highlight taskUrgentItem guifg=red guibg=NONE gui=bold ctermfg=red ctermbg=NONE cterm=bold
highlight taskDoneItem guifg=green guibg=NONE gui=NONE ctermfg=green ctermbg=NONE cterm=NONE
highlight taskQuestionItem guifg=black guibg=yellow gui=NONE ctermfg=black ctermbg=yellow cterm=NONE

highlight taskWorkingIcon guifg=red guibg=NONE gui=bold ctermfg=red ctermbg=NONE cterm=bold
highlight taskUrgentIcon guifg=red guibg=NONE gui=bold ctermfg=red ctermbg=NONE cterm=bold
highlight taskDoneIcon guifg=green guibg=NONE gui=bold ctermfg=green ctermbg=NONE cterm=bold
highlight taskQuestionIcon guifg=black guibg=NONE gui=bold ctermfg=black ctermbg=NONE cterm=bold

syntax match sectionTitleLine "^.*:\s*$" contains=sectionTitle
syntax match sectionTitle "\S.*:\s*$"
highlight sectionTitle guifg=blue guibg=NONE gui=bold,underline ctermfg=blue ctermbg=NONE cterm=bold,underline

let b:current_syntax = "todo"
plugin/todo.vim	[[[1
73
" Copyright © 2013, 2016 Christophe Delord (cdsoft.fr)
" This work is free. You can redistribute it and/or modify it under the
" terms of the Do What The Fuck You Want To Public License, Version 2,
" as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.

if (exists("g:loaded_todo"))
  finish
endif
let g:loaded_todo = 1

let s:cpo_save = &cpo
set cpo&vim

function! Toggle_task_status()
  let line = getline('.')
  if match(line, '^\(\s*\)-') == 0
    let line = substitute(line, '^\(\s*\)-', '\1✓', '')
  elseif match(line, '^\(\s*\)+') == 0
    let line = substitute(line, '^\(\s*\)+', '\1-', '')
  elseif match(line, '^\(\s*\)✓') == 0
    let line = substitute(line, '^\(\s*\)✓', '\1-', '')
  elseif match(line, '^\(\s*\)?') == 0
    let line = substitute(line, '^\(\s*\)?', '\1-', '')
  else
    let line = substitute(line, '^\(\s\{-}\)\(\s\=\)\<', '\2\1- ', '')
  endif
  call setline('.', line)
endfunction

function! Toggle_urgent_task_status()
  let line = getline('.')
  if match(line, '^\(\s*\)+') == 0
    let line = substitute(line, '^\(\s*\)+', '\1✓', '')
  elseif match(line, '^\(\s*\)-') == 0
    let line = substitute(line, '^\(\s*\)-', '\1+', '')
  elseif match(line, '^\(\s*\)✓') == 0
    let line = substitute(line, '^\(\s*\)✓', '\1+', '')
  elseif match(line, '^\(\s*\)?') == 0
    let line = substitute(line, '^\(\s*\)?', '\1+', '')
  else
    let line = substitute(line, '^\(\s\{-}\)\(\s\=\)\<', '\2\1+ ', '')
  endif
  call setline('.', line)
endfunction

function! Toggle_question()
  let line = getline('.')
  if match(line, '^\(\s*\)-') == 0
    let line = substitute(line, '^\(\s*\)-', '\1?', '')
  elseif match(line, '^\(\s*\)+') == 0
    let line = substitute(line, '^\(\s*\)-', '\1?', '')
  elseif match(line, '^\(\s*\)✓') == 0
    let line = substitute(line, '^\(\s*\)✓', '\1?', '')
  elseif match(line, '^\(\s*\)?') == 0
    let line = substitute(line, '^\(\s*\)?\s*', '\1', '')
  else
    let line = substitute(line, '^\(\s\{-}\)\(\s\=\)\<', '\2\1? ', '')
  endif
  call setline('.', line)
endfunction

function SetupTodo()
    inoremap <silent> <buffer> <Leader><TAB> <ESC>:call Toggle_task_status()<CR>i
    noremap <silent> <buffer> <Leader><TAB> :call Toggle_task_status()<CR>
    inoremap <silent> <buffer> <Leader>& <ESC>:call Toggle_urgent_task_status()<CR>i
    noremap <silent> <buffer> <Leader>& :call Toggle_urgent_task_status()<CR>
    inoremap <silent> <buffer> <Leader>q <ESC>:call Toggle_question()<CR>i
    noremap <silent> <buffer> <Leader>q :call Toggle_question()<CR>
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

ftdetect/todo.vim	[[[1
9
" Copyright © 2013, 2016 Christophe Delord (cdsoft.fr)
" This work is free. You can redistribute it and/or modify it under the
" terms of the Do What The Fuck You Want To Public License, Version 2,
" as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.

autocmd BufRead,BufNewFile *.todo,*todo.txt set filetype=todo | call SetupTodo()
augroup filetypedetect
  au BufRead,BufNewFile *.todo,*todo.txt setfiletype todo | call SetupTodo()
augroup END
