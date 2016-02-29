".gvimrc
colorscheme iceberg
set guioptions-=b
set guioptions-=T
" changing cursor colow by insert-mode adn IME ON/OFF{{{
augroup InsertHook
    autocmd!
    autocmd InsertEnter * highlight Cursor guibg=#44DDFF
    autocmd InsertLeave * highlight Cursor guibg=#00FF00
    autocmd InsertEnter * highlight StatusLine guifg=#ccdc90 guibg=red ctermfg=cyan
    autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=white ctermfg=white
    highlight Cursor guibg=#00FF00
    highlight CursorIM guibg=#FF0000
    highlight Pmenu guibg=#666666
    highlight PmenuSel guibg=#8cd0d3 guifg=#666666

augroup END
" }}}
"Font{{{
if has('win32') || has('win64')
    set guifont=Ricty\ Diminished:h13
else
    set guifont=Ricty\ Diminished:h16
    " set guifont=Ricty\ Reguler:h16
    set linespace=1
endif
set ambiwidth=double
"}}}
" vim: set tabstop=4 shiftwidth=4  fdm=marker fdl=0 :
