scriptencoding utf-8
set encoding=utf-8

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

" Enable syntax highlighting 
syntax enable

"" Smoother update
set updatetime=1000

" Space as Leader
let mapleader = "\<Space>"

" Load plugins
lua require('plugins')

set signcolumn=yes:1
highlight clear SignColumn
set fillchars=eob:\ 
