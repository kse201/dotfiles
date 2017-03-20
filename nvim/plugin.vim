augroup MyAutoCmd
    autocmd!
augroup END

" dein.vim{{{
filetype plugin indent off
let g:bundle_dir = $VIMFILE_DIR.'/dein/'
let g:dein_dir = g:bundle_dir.'repos/github.com/Shougo/dein.vim/'
if has('vim_starting')
    if !isdirectory(expand(g:dein_dir))
        echo 'install dein.vim...'
        :call system('git clone git://github.com/Shougo/dein.vim '.g:dein_dir)
    endif
    exe 'set rtp+='.g:dein_dir
endif
call dein#begin(g:bundle_dir)
unlet g:bundle_dir
unlet g:dein_dir

call dein#add('Shougo/dein.vim')
" unite {{{
call dein#add('Shougo/unite.vim')
call dein#add('Shougo/neomru.vim')
" }}}
" align {{{
call dein#add('h1mesuke/vim-alignta')
call dein#add('The-NERD-Commenter')
" }}}
" feature {{{
call dein#add('itchyny/lightline.vim')
call dein#add('glidenote/memolist.vim')
call dein#add('kana/vim-smartinput')
call dein#add('vim-scripts/scratch-utility')
" }}}
" colorscheme & syntax {{{
call dein#add('mrk21/yaml-vim')
call dein#add('cocopon/iceberg.vim')
call dein#add('tomasr/molokai')
" }}}
call dein#add('fatih/vim-go', {'autoload': {'filetypes': ['go']}})

call dein#add('pangloss/vim-javascript', {'autoload': {'filetypes': ['javascript']}})
call dein#add('jelera/vim-javascript-syntax', {'autoload': {'filetypes': ['javascript']}})

call dein#add('kchmck/vim-coffee-script', {'autoload': {'filetypes': ['coffee']}})
call dein#add('moll/vim-node', {'autoload': {'filetypes': ['coffee']}})
call dein#add('heavenshell/vim-jsdoc', {'autoload': {'filetypes': ['javascript', 'coffee']}})
call dein#add('w0rp/ale')

call dein#add('Shougo/deoplete.nvim')
call dein#add('fishbullet/deoplete-ruby')

let g:loaded_matchparen = 1

call dein#end()
filetype plugin indent on
syntax on

" install plugins
if dein#check_install()
    call dein#install()
endif

call map(dein#check_clean(), "delete(v:val, 'rf')")
" }}}

colorscheme iceberg

" unite.vim{{{
let g:unite_split_rule                      = 'botright'
let g:unite_source_file_mru_filename_format = ''
let g:unite_enable_start_insert             = 1
let g:unite_source_file_mru_limit           = 100
let g:unite_enable_ignore_case              = 1
let g:unite_enable_smart_case               = 1

" Prefix-Key
nmap <Leader>a <SID>[unite]
nnoremap <SID>[unite] <Nop>

augroup UniteKeymap
    autocmd!
    au FileType unite nmap     <silent><buffer>  <ESC><ESC> <Plug>(unite_exit)
    au FileType unite imap     <silent><buffer>  <ESC><ESC> <Plug>(unite_exit)
    au FileType unite nmap     <silent><buffer>  a <Plug>(unite_append_end)
    au FileType unite imap     <buffer> <C-w>    <Plug>(unite_delete_backward_path)
    au FileType unite nnoremap <silent><buffer> <expr> <C-Enter> unite#do_action('vsplit')
    au FileType unite inoremap <silent><buffer> <expr> <C-Enter> unite#do_action('vsplit')
    au FileType unite setlocal tw=0
augroup END

nnoremap <silent> <SID>[unite]b :<C-u>Unite buffer<CR>
nnoremap <silent> <SID>[unite]f :<C-u>Unite file<CR>
nnoremap <silent> <SID>[unite]m :<C-u>Unite file_mru<CR>
nnoremap <silent> <SID>[unite]r :<C-u>Unite register<CR>
nnoremap <silent> <SID>[unite]/ :<C-u>Unite -buffer-name=search line<CR>
nnoremap <silent> <SID>[unite]: :<C-u>Unite command<CR>
" }}}

let g:deoplete#enable_at_startup = 1

" NERD Commenter{{{
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims           = 1
nmap <Leader>c <Plug>NERDCommenterToggle
vmap <Leader>c <Plug>NERDCommenterToggle
" }}}

" alignta.vim{{{
let g:Align_xstrlen=3
vnoremap <Leader>a :Alignta<Space>
xmap <silent><expr> as mode() !=# 'v' ? ':Alignta \S\+'."\<CR>" : 'as'
" }}}

" memolist.vim {{{
let g:memolist_memo_suffix = 'txt'
let g:memolist_unite = 1
nnoremap <Leader>mn :MemoNew<CR>
nnoremap <Leader>ml :MemoList<CR>
nnoremap <Leader>mg :MemoList<CR>
" }}}
"" ================================================================================
" vim: set tw=0 tabstop=4 shiftwidth=4  fdm=marker fdl=0 :
