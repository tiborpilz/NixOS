scriptencoding utf-8
set encoding=utf-8
" let base16colorpsace=256

"" Basic Settings
set nocompatible
set shiftwidth=2 tabstop=2 expandtab
set wrap mouse=a
set dir=$HOME/.vim/tmp backupdir=$HOME/.vim/tmp
set ignorecase smartcase shiftround smartindent
set noerrorbells
set autoread
set modeline
set modelines=5
set nofoldenable

"" Set Hybrid Line Numbers (Current line number is absolute, rest are relative)
set relativenumber
set number
"" Make sure to enable line numbers when entering a buffer
autocmd BufEnter * setlocal relativenumber
autocmd BufLeave * setlocal number

"" Terminal Colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" Enable syntax highlighting 
syntax enable
" Enable 256 colors palette 
set t_Co=256
" Important!! 
if has('termguicolors') 
    set termguicolors 
endif

"" Smoother update
set updatetime=1000

" Space as Leader
let mapleader = "\<Space>"

" Load plugins
lua require('plugins')

set signcolumn=yes:1
highlight clear SignColumn
set fillchars=eob:\ 
