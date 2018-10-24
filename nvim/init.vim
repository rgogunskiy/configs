let g:python3_host_prog = $HOME.'/.pyenv/versions/nvim3/bin/python'
let g:python_host_prog = $HOME.'/.pyenv/versions/nvim2/bin/python'
"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=$HOME/.nvim/repos/github.com/Shougo/dein.vim

" Required:
call dein#begin('$HOME/.nvim')

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Add or remove your plugins here:
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/deoplete.nvim')
call dein#add('zchee/deoplete-jedi')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/denite.nvim')
call dein#add('avakhov/vim-yaml')
call dein#add('pearofducks/ansible-vim')
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')
call dein#add('scrooloose/nerdtree')
call dein#add('majutsushi/tagbar')
call dein#add('fatih/vim-go')
call dein#add('vim-pandoc/vim-pandoc')
call dein#add('vim-pandoc/vim-pandoc-syntax')
call dein#add('ledger/vim-ledger')
call dein#add('hashivim/vim-vagrant')
" call dein#add('tpope/vim-commentary')
call dein#add('tomtom/tcomment_vim')

" Required:
call dein#end()

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

" UI
set laststatus=2
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
" colors zenburn
set number
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan

nnoremap <leader>t :tabnew <bar> :TW<CR>
map <C-n> :NERDTreeToggle<CR>


" GO Lang
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Denite
call denite#custom#map('insert', '<C-j>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-k>', '<denite:move_to_previous_line>', 'noremap')
call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')
 
nnoremap <leader>b :Denite buffer<cr>
nnoremap <leader>f :Denite file_rec<cr>
nnoremap <leader>g :Denite grep:.<cr>
nnoremap <leader>t :Denite tag<cr>
nnoremap <leader>y :Denite history/yank<cr>
" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#show_docstring = 0

" setlocal spell spelllang=ru_ru,en_us
