augroup MyAutoCmd
    autocmd!
augroup END

" dein.vim{{{
filetype plugin indent off
let s:dein_dir = $VIMFILE_DIR.'/dein'
let s:dein_repo_dir = s:dein_dir.'/repos/github.com/Shougo/dein.vim/'
let s:dein_toml = $VIMFILE_DIR.'/dein.toml'
if has('vim_starting')
    if !isdirectory(expand(s:dein_repo_dir))
        echo 'install dein.vim...'
        :call system('git clone git://github.com/Shougo/dein.vim '.s:dein_repo_dir)
    endif
    exe 'set rtp+='.s:dein_repo_dir
endif

if dein#load_state(s:dein_dir)
    call dein#begin(s:dein_dir)
    call dein#load_toml(s:dein_toml)
    call dein#end()
    call dein#save_state()
endif

unlet s:dein_dir
unlet s:dein_repo_dir
unlet s:dein_toml

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
