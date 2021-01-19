let g:python3_host_prog = $HOME.'/.pyenv/versions/nvim3/bin/python'
let g:python_host_prog = $HOME.'/.pyenv/versions/nvim2/bin/python'
let g:ruby_host_prog = '/usr/local/lib/ruby/gems/2.7.0/bin/neovim-ruby-host'
" If installed using Homebrew
set rtp+=/usr/local/opt/fzf
" fix issue with spurious q's appearing 
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=0 
set guicursor=
"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

set clipboard+=unnamedplus

" Required:
set runtimepath+=$HOME/.nvim/repos/github.com/Shougo/dein.vim

" Required:
call dein#begin('$HOME/.nvim')

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Add or remove your plugins here:
call dein#add('Shougo/denite.nvim')
call dein#add('raghur/fruzzy', {'hook_post_source': 'call fruzzy#install()'})
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')
call dein#add('scrooloose/nerdtree')
" call dein#add('scrooloose/nerdcommenter')
call dein#add('tpope/vim-commentary')
call dein#add('tpope/vim-fugitive')
call dein#add('hashivim/vim-terraform')
call dein#add('neoclide/coc.nvim', {'merged':0, 'rev': 'release'})
call dein#add('fatih/vim-go')
call dein#add('jiangmiao/auto-pairs')
call dein#add('NLKNguyen/papercolor-theme')
call dein#add('liuchengxu/vista.vim')
call dein#add('airblade/vim-gitgutter')
call dein#add('tpope/vim-surround')
call dein#add('lifepillar/vim-solarized8')
call dein#add('blueyed/vim-diminactive')
call dein#add('towolf/vim-helm')
call dein#add('vimwiki/vimwiki', {'branch': 'dev'})
call dein#add('dracula/vim', {'as': 'dracule'})
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

call map(dein#check_clean(), "delete(v:val, 'rf')")
"End dein Scripts-------------------------

" UI
set termguicolors
set nohlsearch
set laststatus=2
" show existing tab with 2 spaces width
set tabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2
" On pressing tab, insert 4 spaces
set expandtab
"colors zenburn
"let g:airline_theme='one'
" colorscheme dracula
 set background=light " for the light version
" colorscheme dracula
" colorscheme PaperColor
colorscheme solarized8_high
" highlight ColorColumn ctermbg=0 guibg=#626262

set cursorline
set cursorcolumn

set number
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan



" ============================================================================ "
" ===                           PLUGIN SETUP                               === "
" ============================================================================ "

" === NERDTree === "
map <C-n> :NERDTreeToggle<CR>


" === raghur/fruzzy === "
" optional - but recommended - see below
let g:fruzzy#usenative = 1

" When there's no input, fruzzy can sort entries based on how similar they are to the current buffer
" For ex: if you're on /path/to/somefile.h, then on opening denite, /path/to/somefile.cpp
" would appear on the top of the list.
" Useful if you're bouncing a lot between similar files.
" To turn off this behavior, set the variable below  to 0

let g:fruzzy#sortonempty = 1 " default value

" tell denite to use this matcher by default for all sources
call denite#custom#source('_', 'matchers', ['matcher/fruzzy'])

" === vista.vim === "
let g:vista_default_executive = 'coc'
let g:vista#renderer#enable_icon = 1
nmap <F8> :Vista<CR>

" === vim-go ==="
" disable vim-go :GoDef short cut (gd)
" this is handled by LanguageClient [LC]
let g:go_def_mapping_enabled = 0 
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

" === coc.vim === "
" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Use <tab> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>


" === yaml-language-server === "
let g:LanguageClient_serverCommands = {
\ 'yaml': ['yaml-language-server', '--stdio']                                                                                                                                                                              
\ }
augroup LanguageClient_config
  autocmd!
  autocmd User LanguageClientStarted call UserName#yaml#SetSchema()
augroup END

" === Denite ===
" Define mappings
nmap ; :Denite buffer -start-filter grep:::!<CR>
nmap <leader>t :DeniteProjectDir file/rec -start-filter grep:::!<<CR>
nnoremap <silent> <leader>a :Denite -start-filter grep:::!<CR>
nnoremap <leader>g :<C-u>Denite grep:. -no-empty<CR>
nnoremap <leader>j :<C-u>DeniteCursorWord grep:.<CR>

" Define mappings while in denite window
"   <CR>        - Opens currently selected file
"   q or <Esc>  - Quit Denite window
"   d           - Delete currenly selected file
"   p           - Preview currently selected file
"   <C-o> or i  - Switch to insert mode inside of filter prompt
"   <C-t>       - Open currently selected file in a new tab
"   <C-v>       - Open currently selected file a vertical split
"   <C-h>       - Open currently selected file in a horizontal split
autocmd FileType denite call s:denite_my_settings()
	function! s:denite_my_settings() abort
	  nnoremap <silent><buffer><expr> <CR>
	  \ denite#do_map('do_action')
	  nnoremap <silent><buffer><expr> d
	  \ denite#do_map('do_action', 'delete')
	  nnoremap <silent><buffer><expr> p
	  \ denite#do_map('do_action', 'preview')
	  nnoremap <silent><buffer><expr> q
	  \ denite#do_map('quit')
	  nnoremap <silent><buffer><expr> i
	  \ denite#do_map('open_filter_buffer')
	  nnoremap <silent><buffer><expr> <Space>
	  \ denite#do_map('toggle_select').'j'
    nnoremap <silent><buffer><expr> <C-o>
    \ denite#do_map('open_filter_buffer')
    nnoremap <silent><buffer><expr> <C-t>
    \ denite#do_map('do_action', 'tabopen')
    nnoremap <silent><buffer><expr> <C-v>
    \ denite#do_map('do_action', 'vsplit')
    nnoremap <silent><buffer><expr> <C-h>
    \ denite#do_map('do_action', 'split')
	endfunction

" Define mappings while in 'filter' mode
"   <C-o>         - Switch to normal mode inside of search results
"   <Esc>         - Exit denite window in any mode
"   <CR>          - Open currently selected file in any mode
"   <C-t>         - Open currently selected file in a new tab
"   <C-v>         - Open currently selected file a vertical split
"   <C-h>         - Open currently selected file in a horizontal split
	autocmd FileType denite-filter call s:denite_filter_my_settings()
	function! s:denite_filter_my_settings() abort
	  imap <silent><buffer> <C-o> <Plug>(denite_filter_quit)
    nnoremap <silent><buffer><expr> <Esc>
    \ denite#do_map('quit')
    inoremap <silent><buffer><expr> <CR>
    \ denite#do_map('do_action', 'open')
    inoremap <silent><buffer><expr> <C-t>
    \ denite#do_map('do_action', 'tabopen')
    inoremap <silent><buffer><expr> <C-v>
    \ denite#do_map('do_action', 'vsplit')
    inoremap <silent><buffer><expr> <C-h>
    \ denite#do_map('do_action', 'split')
	endfunction

	" Change file/rec command.
	" call denite#custom#var('file/rec', 'command',
	" \ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
  call denite#custom#var('file/rec', 'command',
	\ ['rg', '--files', '--glob', '!.git', '--color', 'never'])
	" For python script scantree.py
	" Read bellow on this file to learn more about scantree.py
	" call denite#custom#var('file/rec', 'command',
	" \ ['scantree.py', '--path', ':directory'])

	" Change matchers.
	call denite#custom#source(
	\ 'file_mru', 'matchers', ['matcher/fuzzy', 'matcher/project_files'])

	" Change sorters.
	call denite#custom#source(
	\ 'file/rec', 'sorters', ['sorter/sublime'])

	" Change default action.
	call denite#custom#kind('file', 'default_action', 'split')

	" Add custom menus
	let s:menus = {}

	let s:menus.zsh = {
		\ 'description': 'Edit your import zsh configuration'
		\ }
	let s:menus.zsh.file_candidates = [
		\ ['zshrc', '~/.config/zsh/.zshrc'],
		\ ['zshenv', '~/.zshenv'],
		\ ]

	let s:menus.my_commands = {
		\ 'description': 'Example commands'
		\ }
	let s:menus.my_commands.command_candidates = [
		\ ['Split the window', 'vnew'],
		\ ['Open zsh menu', 'Denite menu:zsh'],
		\ ['Format code', 'FormatCode', 'go,python'],
		\ ]

	call denite#custom#var('menu', 'menus', s:menus)
	" Ripgrep command on grep source
	call denite#custom#var('grep', {
		\ 'command': ['rg'],
		\ 'default_opts': ['-i', '--vimgrep', '--no-heading'],
		\ 'recursive_opts': [],
		\ 'pattern_opt': ['--regexp'],
		\ 'separator': ['--'],
		\ 'final_opts': [],
		\ })
	" " Ag command on grep source
	" call denite#custom#var('grep', 'command', ['ag'])
	" call denite#custom#var('grep', 'default_opts',
	" 		\ ['-i', '--vimgrep'])
	" call denite#custom#var('grep', 'recursive_opts', [])
	" call denite#custom#var('grep', 'pattern_opt', [])
	" call denite#custom#var('grep', 'separator', ['--'])
	" call denite#custom#var('grep', 'final_opts', [])

	" " Ripgrep command on grep source
	" call denite#custom#var('grep', 'command', ['rg'])
	" call denite#custom#var('grep', 'default_opts',
	" 		\ ['-i', '--vimgrep', '--no-heading'])
	" call denite#custom#var('grep', 'recursive_opts', [])
	" call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
	" call denite#custom#var('grep', 'separator', ['--'])
	" call denite#custom#var('grep', 'final_opts', [])

	" Specify multiple paths in grep source
	"call denite#start([{'name': 'grep',
	"      \ 'args': [['a.vim', 'b.vim'], '', 'pattern']}])

	" Define alias
	call denite#custom#alias('source', 'file/rec/git', 'file/rec')
	call denite#custom#var('file/rec/git', 'command',
	      \ ['git', 'ls-files', '-co', '--exclude-standard'])

	" Change ignore_globs
	call denite#custom#filter('matcher/ignore_globs', 'ignore_globs',
	      \ [ '.git/', '.ropeproject/', '__pycache__/',
	      \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/'])

	" Custom action
	" Note: lambda function is not supported in Vim8.
	call denite#custom#action('file', 'test',
	      \ {context -> execute('let g:foo = 1')})
	call denite#custom#action('file', 'test2',
	      \ {context -> denite#do_action(
	      \  context, 'open', context['targets'])})



""" KEYBINDING 
" Next/Prev tab
nnoremap H gT
nnoremap L gt



function! s:ScratchGenerator()
  echom "Creating scratchy..."
  exe "new" . "__Scratchy__"
  echom "Scratchy created!"
endfunction

function! s:ScratchMarkBuffer()
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
endfunction

autocmd BufNewFile __Scratchy__ call s:ScratchMarkBuffer()
command! Scratchy call s:ScratchGenerator()

nnoremap <C-s> :Scratchy<CR>


let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

"""autocmd FileType vimwiki set ft=markdown

