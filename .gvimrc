".gvimrc
" last change: 26-Oct-2013.

colorscheme desertEx

set guioptions-=b
set guioptions-=T
" insert-mode, IME ON/OFFに応じたカラー変更{{{
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

    " IME制御
    " autocmd InsertEnter,CmdwinEnter * set noimdisable
    " autocmd InsertLeave,CmdwinLeave * set imdisable
augroup END
" }}}
"Font{{{
if has('win32') || has('win64')
    set guifont=Ricty\ Diminished:h14
else
    set guifont=Ricty\ Diminished:h16
    " set guifont=Ricty\ Reguler:h16
    set linespace=1
endif
set ambiwidth=double
"}}}
" 透過処理{{{
if has('gui_macvim')
    set imdisable
    autocmd MyAutoCmd FocusGained * set transparency=0
    autocmd MyAutoCmd FocusLost * set transparency=80
endif
" }}}
" vim: set tabstop=4 shiftwidth=4  fdm=marker fdl=0 :
