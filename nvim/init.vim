let g:python3_host_prog = $HOME.'/.pyenv/versions/nvim3/bin/python'
let g:python_host_prog = $HOME.'/.pyenv/versions/nvim2/bin/python'
" If installed using Homebrew
set rtp+=/usr/local/opt/fzf
" fix issue with spurious q's appearing 
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=0 
set guicursor=
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
" call dein#add('Shougo/deoplete.nvim')
" call dein#add('zchee/deoplete-go', {'build': 'make'})
" call dein#add('zchee/deoplete-jedi')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/denite.nvim')
" call dein#add('junegunn/fzf', { 'build': './install', 'merged': 0 })
" call dein#add('junegunn/fzf.vim')

" call dein#add('avakhov/vim-yaml')
" call dein#add('pearofducks/ansible-vim')
" call dein#add('vim-airline/vim-airline')
" call dein#add('vim-airline/vim-airline-themes')
" call dein#add('scrooloose/nerdtree')
" call dein#add('scrooloose/nerdcommenter')
" call dein#add('majutsushi/tagbar')
" call dein#add('mdempsky/gocode')
" call dein#add('vim-pandoc/vim-pandoc')
" call dein#add('vim-pandoc/vim-pandoc-syntax')
" call dein#add('ledger/vim-ledger')
" call dein#add('hashivim/vim-vagrant')
" " call dein#add('tpope/vim-commentary')
call dein#add('tomtom/tcomment_vim')
call dein#add('rakr/vim-one')
" call dein#add('tpope/vim-fugitive')
" call dein#add('hashivim/vim-terraform')
" " call dein#add('neoclide/coc.nvim', {'merge':0, 'build': './install.sh nightly'})
" call dein#add('fatih/vim-go')
" " call dein#add('jiangmiao/auto-pairs')
" Required:
call dein#end()

" Required:
filetype plugin on
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

" UI
set termguicolors
set nohlsearch
set laststatus=2
" show existing tab with 4 spaces width
set tabstop=2
" when indenting with '>', use 4 spaces width
set shiftwidth=2
" On pressing tab, insert 4 spaces
set expandtab
" colors zenburn
let g:airline_theme='one'
colorscheme one
set background=light " for the dark version
" set background=light " for the light version
set cursorline
set cursorcolumn

set number
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan

map <C-n> :NERDTreeToggle<CR>


" GO Lang
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

" -------------------------------------------------------------------------------------------------
" coc.nvim default settings
" -------------------------------------------------------------------------------------------------

" " if hidden is not set, TextEdit might fail.
" set hidden
" " Better display for messages
" set cmdheight=2
" " Smaller updatetime for CursorHold & CursorHoldI
" set updatetime=300
" " don't give |ins-completion-menu| messages.
" set shortmess+=c
" " always show signcolumns
" set signcolumn=yes
"
" " Use tab for trigger completion with characters ahead and navigate.
" " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
"
" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~# '\s'
" endfunction
"
" " Use <c-space> to trigger completion.
" inoremap <silent><expr> <c-space> coc#refresh()
"
" " Use `[c` and `]c` to navigate diagnostics
" nmap <silent> [c <Plug>(coc-diagnostic-prev)
" nmap <silent> ]c <Plug>(coc-diagnostic-next)
"
" " Remap keys for gotos
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)
"
" " Use U to show documentation in preview window
" nnoremap <silent> U :call <SID>show_documentation()<CR>
"
" " Remap for rename current word
" nmap <leader>rn <Plug>(coc-rename)
"
" " Remap for format selected region
" vmap <leader>f  <Plug>(coc-format-selected)
" nmap <leader>f  <Plug>(coc-format-selected)
" " Show all diagnostics
" nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" " Manage extensions
" nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" " Show commands
" nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" " Find symbol of current document
" nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
"
" " Search workspace symbols
" nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
" nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" " Do default action for previous item.
" nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" " Resume latest coc list
" nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
" " disable vim-go :GoDef short cut (gd)
" " this is handled by LanguageClient [LC]
" let g:go_def_mapping_enabled = 0
"
"
" " Tagbar
nmap <F8> :TagbarToggle<CR>


" " Wrap in try/catch to avoid errors on initial install before plugin is available
" try
" " === Denite setup ==="
" " Use ripgrep for searching current directory for files
" " By default, ripgrep will respect rules in .gitignore
" "   --files: Print each file that would be searched (but don't search)
" "   --glob:  Include or exclues files for searching that match the given glob
" "            (aka ignore .git files)
" "
" call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])
"
" " Use ripgrep in place of "grep"
" call denite#custom#var('grep', 'command', ['rg'])
"
" " Custom options for ripgrep
" "   --vimgrep:  Show results with every match on it's own line
" "   --hidden:   Search hidden directories and files
" "   --heading:  Show the file name above clusters of matches from each file
" "   --S:        Search case insensitively if the pattern is all lowercase
" call denite#custom#var('grep', 'default_opts', ['--hidden', '--vimgrep', '--heading', '-S'])
"
" " Recommended defaults for ripgrep via Denite docs
" call denite#custom#var('grep', 'recursive_opts', [])
" call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
" call denite#custom#var('grep', 'separator', ['--'])
" call denite#custom#var('grep', 'final_opts', [])
"
" " Remove date from buffer list
" call denite#custom#var('buffer', 'date_format', '')
"
" " Open file commands
" call denite#custom#map('insert,normal', "<C-t>", '<denite:do_action:tabopen>')
" call denite#custom#map('insert,normal', "<C-v>", '<denite:do_action:vsplit>')
" call denite#custom#map('insert,normal', "<C-h>", '<denite:do_action:split>')
"
" " Custom options for Denite
" "   auto_resize             - Auto resize the Denite window height automatically.
" "   prompt                  - Customize denite prompt
" "   direction               - Specify Denite window direction as directly below current pane
" "   winminheight            - Specify min height for Denite window
" "   highlight_mode_insert   - Specify h1-CursorLine in insert mode
" "   prompt_highlight        - Specify color of prompt
" "   highlight_matched_char  - Matched characters highlight
" "   highlight_matched_range - matched range highlight
" let s:denite_options = {'default' : {
" \ 'auto_resize': 1,
" \ 'prompt': 'λ:',
" \ 'direction': 'rightbelow',
" \ 'winminheight': '10',
" \ 'highlight_mode_insert': 'Visual',
" \ 'highlight_mode_normal': 'Visual',
" \ 'prompt_highlight': 'Function',
" \ 'highlight_matched_char': 'Function',
" \ 'highlight_matched_range': 'Normal'
" \ }}
"
" " Loop through denite options and enable them
" function! s:profile(opts) abort
"   for l:fname in keys(a:opts)
"     for l:dopt in keys(a:opts[l:fname])
"       call denite#custom#option(l:fname, l:dopt, a:opts[l:fname][l:dopt])
"     endfor
"   endfor
" endfunction
"
" call s:profile(s:denite_options)
" catch
"   echo 'Denite not installed. It should work after running :PlugInstall'
" endtry
"
"
" " Use deoplete.
" let g:deoplete#enable_at_startup = 1
" let g:deoplete#sources#jedi#show_docstring = 0
" " setlocal spell spelllang=ru_ru,en_us

" " ============================================================================ "
" " ===                             KEY MAPPINGS                             === "
" " ============================================================================ "
nmap ; :Denite buffer -split=floating -winrow=1<CR>
nmap <leader>t :Denite file/rec -split=floating -winrow=1<CR>
nnoremap <leader>g :<C-u>Denite grep:. -no-empty<CR>
nnoremap <leader>j :<C-u>DeniteCursorWord grep:.<CR>
