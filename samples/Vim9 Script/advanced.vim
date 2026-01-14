vim9script noclear
# Advanced Vim9 Script Example
# Demonstrates modern Vim9 features for the Linguist repository

# Strict typing and namespace
import autoload 'knack.vim' as knack
import autoload './utils.vim' as utils

# Constants and typed variables
const BUFFER_SIZE: number = 1024
const MAX_RETRIES: number = 3
const CONFIG_DIR: string = $HOME .. '/.config/vim'

# Class definitions (Vim9 feature)
class Cache
  var entries: dict<any> = {}
  var max_size: number

  def new(size: number = 100)
    this.max_size = size
  enddef

  def set(key: string, value: any): void
    if len(this.entries) >= this.max_size
      var first_key = keys(this.entries)[0]
      unlet this.entries[first_key]
    endif
    this.entries[key] = value
  enddef

  def get(key: string): any
    return get(this.entries, key, null)
  enddef

  def clear(): void
    this.entries = {}
  enddef
endclass

# Modern function syntax with type annotations
def ProcessBuffer(buf: number, opts: dict<any>): dict<string>
  var result: dict<string> = {}
  var lines: list<string> = getbufline(buf, 1, '$')

  # Process with early returns
  if empty(lines)
    result['status'] = 'empty'
    result['count'] = '0'
    return result
  endif

  var count: number = 0
  for line in lines
    if line =~# '^\s*#.*autocmd'
      count += 1
    endif
  endfor

  result['status'] = 'success'
  result['count'] = string(count)
  result['size'] = string(len(lines))

  return result
enddef

# Lambda functions with strict typing
var multiply: func(number, number): number = (a, b) => a * b
var filter_nums: func(list<number>): list<number> =
  (nums) => nums->filter('v:val > 10')

# Object methods and method chaining
def ApplyFormatting(content: string): string
  return content
    ->substitute('  ', ' ', 'g')
    ->trim()
    ->tolower()
enddef

# Enumerate with type safety
def AnalyzeSettings(settings: dict<string>): list<dict<any>>
  var results: list<dict<any>> = []

  for [key, value] in items(settings)
    var item: dict<any> = {
      'key': key,
      'value': value,
      'type': typename(value),
      'length': strlen(value)
    }
    results->add(item)
  endfor

  return results
enddef

# Recursive function example
def Factorial(n: number): number
  if n <= 1
    return 1
  else
    return n * Factorial(n - 1)
  endif
enddef

# Error handling with try-catch
def SafeFileRead(filepath: string): list<string>
  var lines: list<string> = []

  try
    lines = readfile(filepath)
  catch /E484/
    echomsg 'File not found: ' .. filepath
    lines = []
  catch
    echomsg 'Error reading file: ' .. v:exception
    lines = []
  endtry

  return lines
enddef

# Variadic function
def JoinPaths(...parts: list<string>): string
  return parts->join('/')
enddef

# Command definitions using new syntax
command! -nargs=? -complete=file MyCommand {
  var arg = <q-args>
  var result = ProcessBuffer(bufnr(), {'name': arg})
  echomsg 'Result: ' .. string(result)
}

# Augroup with modern syntax
augroup MyAutocmds
  autocmd!
  autocmd BufWritePost *.vim {
    var buf = expand('<abuf>')
    echomsg 'Wrote buffer ' .. buf
  }
  autocmd FileType python {
    setlocal shiftwidth=4
    setlocal tabstop=4
  }
augroup END

# Using the cache class
var cache: Cache = Cache.new(50)
cache.set('user_prefs', {'theme': 'dark', 'font_size': 12})

def GetUserPrefs(): any
  var prefs = cache.get('user_prefs')
  if prefs == null
    prefs = {'theme': 'light', 'font_size': 10}
    cache.set('user_prefs', prefs)
  endif
  return prefs
enddef

# Advanced list comprehension patterns
var numbers: list<number> = range(1, 100)
var evens: list<number> = numbers->filter('v:val % 2 == 0')
var squares: list<number> = numbers->map('v:val * v:val')

# String interpolation (Vim9)
def FormatMessage(name: string, count: number): string
  return $'Found {count} items for {name}'
enddef

# Abort on first error (Vim9 behavior by default with vim9script)
if !exists('g:loaded_myplugin')
  finish
endif

g:loaded_myPlugin = 1

# Export public interface
export def PublicAPI(input: string): string
  return 'Processed: ' .. input
enddef

# Internal helper (not exported)
def PrivateHelper(): void
  echomsg 'This is internal'
enddef
