set macligatures
set guifont=Fira\ Code:h14

let macvim_skip_colorscheme=1

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

Bundle 'https://github.com/freeo/vim-kalisi'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
"


colorscheme kalisi
set background=light

let g:airline_theme='kalisi'
let g:airline_powerline_fonts = 1

set fenc=utf-8
set noswapfile

set expandtab
set tabstop=4
set shiftwidth=4

set autoindent

set lines=25
set cmdheight=0

syntax on

" key bindings

inoremap <silent> <C-b> <Left>
inoremap <silent> <C-f> <Right>
inoremap <silent> <C-p> <Up>
inoremap <silent> <C-n> <Down>

inoremap <silent> <C-a> <Home>
inoremap <silent> <C-e> <End>

inoremap <silent> <C-t> <ESC>xpi
inoremap <silent> <C-z> <ESC>ui

inoremap <silent> <C-x><C-s> <ESC>:w<CR>i
inoremap <silent> <C-x><C-c> <ESC>:q
