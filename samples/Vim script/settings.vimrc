set encoding=utf-8
silent! set modelineexpr
filetype on
syntax enable

" Configure Vundle
if getftype("~/.vim/bundle/Vundle.vim") ==? "dir"
	filetype off
	set nocompatible
	set rtp+=~/.vim/bundle/Vundle.vim
	call vundle#begin()
	Plugin "VundleVim/Vundle.vim"
	call vundle#end()
	filetype on
endif

" Configure Pathogen
if exists("g:loaded_pathogen")
	execute pathogen#infect()
	syntax on
	filetype plugin indent on
endif

" Use Solarized theme if running graphically
if has("gui_running")
	set background=dark
	colorscheme solarized
endif

"======================================================================*
set number
set autoread
set backspace=indent,eol,start
set tabstop=4 softtabstop=0 noexpandtab shiftwidth=4
set hlsearch
set ignorecase
set incsearch
set modelines=50
set showmatch

highlight OverLength ctermbg=red ctermfg=white guibg=#592929
fun! UpdateMatch()
	if exists("b:current_syntax")
		if b:current_syntax =~ "gitcommit"
			match OverLength /\%>72v.\+/
		else
			match NONE
		endif
	endif
endfun
autocmd BufEnter,BufWinEnter * call UpdateMatch()


"======================================================================*
" Status line colours
highlight statusline ctermbg=DarkGreen ctermfg=Black

" Display highlighting group at cursor
fun! StatusLineHighlightGroup()
	return synIDattr(synID(line("."), col("."), 1), "name")
endfun

" Name of currently-active filetype
fun! StatusLineFileType()
	return strlen(&ft) ? &ft : "none"
endfun

" Character encoding and byte-order mark indicators
fun! StatusLineEncoding()
	let encoding = toupper (&fenc == "" ? &enc : &fenc)
	if exists("+bomb") && &bomb
	 	encoding += "(BOM)"
	endif
	return encoding
endfun

" Line-ending style
fun! StatusLineEOLStyle()
	if     (&fileformat ==? "unix") | return "LF"
	elseif (&fileformat ==? "dos")  | return "CRLF"
	elseif (&fileformat ==? "mac")  | return "CR"
	endif; return type
endfun

if has("statusline")
	set laststatus=2
	set statusline=%#Question#%-2.2n\               " Buffer number
	set statusline+=%#WarningMsg#%<%f\ %#Question#  " Filename
	set statusline+=\ %l:%c                         " Line:column
	set statusline+=\ %{StatusLineHighlightGroup()} " Highlighting group at cursor
	set statusline+=%=                              " Left/right divider
	set statusline+=%{StatusLineEOLStyle()}\ \      " What line terminators are used
	set statusline+=%{StatusLineEncoding()}\ \      " Encoding + BOM
	set statusline+=%{StatusLineFileType()}\ \      " Filetype
	set statusline+=%h%m%r%w\                       " Flags
endif
