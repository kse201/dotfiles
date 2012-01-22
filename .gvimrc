".gvimrc
"last change: 20-Jan-2012.
" vim: set tabstop=4 shiftwidth=4  fdm=marker fdl=0 :

colorscheme molokai
if has('mac')
    set guioptions-=T
endif
" insert-mode, IME ON/OFFに応じたカラー変更
augroup InsertHook
    autocmd!
    "カーソル
    autocmd InsertEnter * highlight Cursor guibg=#44DDFF
    autocmd InsertLeave * highlight Cursor guibg=#00FF00
    "ステータスライン
    autocmd InsertEnter * highlight StatusLine guifg=#ccdc90 guibg=red ctermfg=cyan
    autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=white ctermfg=white
    "IME ON/OFF
    highlight Cursor guibg=#00FF00
    highlight CursorIM guibg=#FF0000
    " ポップアップメニューのカラーを設定
    highlight Pmenu guibg=#666666
    highlight PmenuSel guibg=#8cd0d3 guifg=#666666
augroup END

"Font
if has('win32') || has('win64')
    set guifont=Inconsolata:h12:cANSI
    set guifontwide=MS_Gothic:h12
else
    set guifont=Inconsolata:h16
    set guifontwide=Migu\ 1M\ regular:h14
    set linespace=1
endif
set ambiwidth=double
" set lines=50 columns=100
set guioptions+=b
" 透過処理{{{
if has('gui_macvim')
    set imdisable
    autocmd MyAutoCmd FocusGained * set transparency=0
    autocmd MyAutoCmd FocusLost * set transparency=70
endif
" }}}
