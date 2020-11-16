UsePlugin 'ctrlp.vim'
" let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|build\|venv|.venv'
let g:ctrp_cmd='CtrlPMixed'

let g:ctrlp_lazy_update = 1

let g:ctrlp_max_depth = 10
let g:ctrlp_custom_ignore = {
            \ 'dir':  '\v[\/](.git|.hg|.svn|.venv|venv|cover|build)$',
            \ 'file': '\v\.(exe|so|dll|pyc)$',
            \ 'link': 'some_bad_symbolic_links',
            \ }

let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0

let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:10,results:50'
