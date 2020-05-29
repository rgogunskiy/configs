
" call denite#custom#var('file/rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
" call denite#custom#option('default', 'prompt', 'λ')
" call denite#custom#var('grep', 'command', ['ag'])
" call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep'])
" call denite#custom#var('grep', 'recursive_opts', [])
" call denite#custom#var('grep', 'pattern_opt', [])
" call denite#custom#var('grep', 'separator', ['--'])
" call denite#custom#var('grep', 'final_opts', [])
" call denite#custom#source('file_rec', 'sorters', ['sorter_sublime'])
" call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
"      \ [ '.git/', '.ropeproject/', '__pycache__/*', '*.pyc', 'node_modules/',
"      \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/', '*.png'])
call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])

" Use ripgrep in place of "grep"
call denite#custom#var('grep', 'command', ['rg'])

" Custom options for ripgrep
"   --vimgrep:  Show results with every match on it's own line
"   --hidden:   Search hidden directories and files
"   --heading:  Show the file name above clusters of matches from each file
"   --S:        Search case insensitively if the pattern is all lowercase
call denite#custom#var('grep', 'default_opts', ['--hidden', '--vimgrep', '--heading', '-S'])

" Recommended defaults for ripgrep via Denite docs
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])"

nmap <LEADER>p :Denite -start-filter file/rec<CR>
nmap <LEADER>b :Denite buffer<CR>
nnoremap \ :Denite grep<CR>


nnoremap <silent><buffer><expr> <CR>
      \ denite#do_map('do_action')
nnoremap <silent><buffer><expr> d
      \ denite#do_map('do_action', 'delete')
nnoremap <silent><buffer><expr> p
      \ denite#do_map('do_action', 'preview')
nnoremap <silent><buffer><expr> <C-v>
      \ denite#do_map('do_action', 'vsplit')
nnoremap <silent><buffer><expr> <C-x>
      \ denite#do_map('do_action', 'split')
nnoremap <silent><buffer><expr> <Esc>
      \ denite#do_map('quit')
nnoremap <silent><buffer><expr> i
      \ denite#do_map('open_filter_buffer')
nnoremap <silent><buffer><expr> <Space>
      \ denite#do_map('toggle_select').'j'
