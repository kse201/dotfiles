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
let g:dein#install_process_timeout=600

if dein#load_state(s:dein_dir)
    call dein#begin(s:dein_dir)
    call dein#load_toml(s:dein_toml)
    call dein#end()
    call dein#save_state()
endif

filetype plugin indent on
syntax on

" install plugins
if dein#check_install()
    call dein#install()
endif

call map(dein#check_clean(), "delete(v:val, 'rf')")
" }}}

"" ================================================================================
" vim: set tw=0 tabstop=4 shiftwidth=4  fdm=marker fdl=0 :
