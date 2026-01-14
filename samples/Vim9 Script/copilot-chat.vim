vim9script
scriptencoding utf-8

import autoload 'copilot_chat/auth.vim' as auth
import autoload 'copilot_chat/buffer.vim' as _buffer
import autoload 'copilot_chat/api.vim' as api

export def OpenChat(): void
  if _buffer.HasActiveChat() && g:copilot_reuse_active_chat == 1
    _buffer.FocusActiveChat()
  else
    _buffer.Create()
  endif
  timer_start(10, (_) => auth.VerifySignin())
enddef

export def StartChat(message: string): void
  OpenChat()
  _buffer.AppendMessage(message)
  api.AsyncRequest([{'content': message, 'role': 'user'}], [])
enddef

export def ResetChat(): void
  if g:copilot_chat_active_buffer == -1 || !bufexists(g:copilot_chat_active_buffer)
    echom 'No active chat window to reset'
    return
  endif

  var current_buf = bufnr('%')

  # Switch to the active chat buffer if not already there
  if current_buf != g:copilot_chat_active_buffer
    execute 'buffer ' .. g:copilot_chat_active_buffer
  endif

  deletebufline('%', 1, '$')

  _buffer.WelcomeMessage()

  if current_buf != g:copilot_chat_active_buffer && bufexists(current_buf)
    execute 'buffer ' .. current_buf
  endif
enddef

export def SubmitMessage(): void
  auth.GetTokens()
  var messages = []
  var pattern = ' ━\+$'
  var all_file_lists = []
  cursor(1, 1)

  while search(pattern, 'W') > 0
    var header_line = getline('.')
    var role = 'user'
    # Check separator icon to determine message role
    # Separator with  icon indicates assistant response, otherwise user message
    if stridx(header_line, ' ') != -1
      role = 'assistant'
    endif
    var start_line: number = line('.') + 1
    var end_line: number = search(pattern, 'W')
    if end_line == 0
      end_line = line('$')
    else
      end_line -= 1
      cursor(line('.') - 1, col('.'))
    endif

    var lines: list<string> = getline(start_line, end_line)
    var file_list: list<string> = []

    for i in range(len(lines))
      var line: string = lines[i]
      if line =~? '^> \(\w\+\)'
        var text: string = matchstr(line, '^> \(\w\+\)')
        text = substitute(text, '^> ', '', '')
        if has_key(g:copilot_chat_prompts, text)
          lines[i] = g:copilot_chat_prompts[text]
        endif
      elseif line =~? '^#file: '
        var filename: string = matchstr(line, '^#file: \s*\zs.*\ze$')
        add(file_list, filename)
      endif
    endfor
    var message: string = join(lines, "\n")

    add(messages, {'content': message, 'role': role})
    add(all_file_lists, file_list)
    cursor(line('.'), col('.') + 1)
  endwhile

  # Limit message history to improve performance
  # Only send the most recent messages based on configuration
  var limit: number = g:copilot_chat_message_history_limit
  if len(messages) > limit && limit > 0
    var start_idx: number = len(messages) - limit
    messages = messages[start_idx : ]
    all_file_lists = all_file_lists[start_idx : ]
  endif

  # Consolidate file lists from recent messages
  # O(n) consolidation using dictionary for O(1) duplicate detection (improved from O(n²))
  var consolidated_files: list<string> = []
  var seen: dict<any> = {}
  for files in all_file_lists
    for file in files
      if !has_key(seen, file)
        seen[file] = 1
        add(consolidated_files, file)
      endif
    endfor
  endfor

  api.AsyncRequest(messages, consolidated_files)
enddef
