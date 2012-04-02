"============================================================
"                      *** .vimrc ***                       |
"                 Last Change: 02-Apr-2012.                 |
"============================================================
" 基礎的な設定 {{{
" OS毎の各種ディレクトリの設定
if has('win32') || has('win64')
    let $VIMFILE_DIR = $HOME . 'vimfiles'
    let $DROPBOX_DIR = $HOME . '\Documents\My Dropbox'
    set backupdir=$HOME/_vimbackup
elseif has('mac')
    let $VIMFILE_DIR = $HOME . '/.vim'
    let $DROPBOX_DIR = $HOME . '/Dropbox'
    set backupdir=$HOME/.vimbackup
else 
    let $VIMFILE_DIR = $HOME . '/.vim'
    let $DROPBOX_DIR = $HOME . '/Documents\My Dropbox'
    set backupdir=$HOME/.vimbackup
endif
augroup MyAutoCmd
    autocmd!
augroup END
let mapleader=','
set shortmess+=I
set nocompatible "Vi非互換
set modeline
set noswapfile
set complete+=k
set noequalalways
set history=1000
set vb t_vb=
set autowrite
set backspace=indent,eol,start
let g:netrw_sort_by="time"
let g:netrw_sort_direction="reverse"
set diffopt=filler,vertical
set showcmd
" viminfoファイルの設定
" 参考 http://d.hatena.ne.jp/yuroyoro/20101104/1288879591
set mouse=a
set showtabline=2
set guioptions+=c
set guioptions-=e
set formatoptions-=ro " 改行後の自動コメントアウト禁止
set tw=0 " 自動改行無効
" MacVimでMetaキー
if exists('+macmeta')
    set macmeta
endif
" undo履歴保存して再開させる{{{
if has('persistent_undo')
    set undodir=~/.vimundo
    set undofile
endif 
" }}}
" 設定ファイル{{{
let $MYVIMRCPLUGIN= $HOME."/.vimrc.plugin"
command! EditVimrc :edit $MYVIMRC
command! EV :edit $MYVIMRC
command! EditGVimrc :edit $MYGVIMRC
command! EG :edit $MYGVIMRC
command! EditPlugin :edit $MYVIMRCPLUGIN
command! EP :edit $MYVIMRCPLUGIN
command! Memo :edit $DROPBOX_DIR/documents/memo
command! Mm :edit $DROPBOX_DIR/documents/memo/memo.memo
nnoremap <Leader>em :edit $DROPBOX_DIR/documents/memo/memo.memo<CR>
" }}}
" Auto Loading .vimrc,.gvimrc {{{
if has("autocmd")
    autocmd MyAutoCmd BufReadPost *
                \if line("'\"") > 0 && line("'\"") <= line("$") |
                \exe "normal! g'\"" |
                \endif
endif

command! ReloadVimrc source $MYVIMRC
command! ReloadGVimrc source $MYGVIMRC
command! ReloadPlugin source $MYVIMRCPLUGIN

if !has('gui_running')
    " .vimrcの再読込時にも色が変化するようにする
    autocmd MyAutoCmd BufWritePost $MYVIMRC nested source $MYVIMRC
else
    " .vimrcの再読込時にも色が変化するようにする
    autocmd MyAutoCmd BufWritePost $MYVIMRC source $MYVIMRC |
                \ if has('gui_running') |
                \ source $MYGVIMRC
    autocmd MyAutoCmd BufWritePost $MYGVIMRC if has('gui_running') |
                \ source $MYGVIMRC
endif
" }}}
" Auto delete line-end Space{{{
augroup Autoplace
    autocmd!
    autocmd BufWritePre *.[^{mkd}] :%s/\s\+$//ge
augroup END
" }}}
" \ -> ¥{{{
if has('mac')
    inoremap ¥ \
    cnoremap ¥ \
endif
" }}}
" Terminal用{{{
if !has('gui') && has('mac')
    set t_Co=256
    colorscheme desert
endif
" }}}
" Auto Change dir{{{
if has("autochdir")
    set autochdir
    set tags=tags
else
    set tags=./tags,./../tags,./*/tags,./../../tags,./../../../tags,./../../../../tags,./../../../../../tags
    if has('win32') || has('win64')
        au MyAutoCmd BufEnter * execute ":lcd " . escape(expand("%:p:h")," #")
    else
        au MyAutoCmd BufEnter * execute ":lcd " . escape(expand("%:p:h")," #¥") 
    endif
endif
" }}}
" }}}
" ファイル形式関連{{{
" Auto encoding{{{
if has('gui_running') && (has('win32') || has('win64'))
    set enc=utf-8
    " set fenc=utf-8
    set fencs=iso-2011-jp,enc-jp,sjis,cp932,utf-8
    scriptencoding cp932
    set termencoding=utf-8
else
    if &encoding !=# 'utf-8'
        set encoding=japan
        set fileencoding=japan
    endif
    if has('iconv')
        let s:enc_euc = 'euc-jp'
        let s:enc_jis = 'iso-2022-jp'
        " iconvがeucJP-msに対応しているかをチェック
        if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
            let s:enc_euc = 'eucjp-ms'
            let s:enc_jis = 'iso-2022-jp-3'
            " iconvがJISX0213に対応しているかをチェック
        elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
            let s:enc_euc = 'euc-jisx0213'
            let s:enc_jis = 'iso-2022-jp-3'
        endif
        " fileencodingsを構築
        if &encoding ==# 'utf-8'
            let s:fileencodings_default = &fileencodings
            let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
            let &fileencodings = &fileencodings .','. s:fileencodings_default
            unlet s:fileencodings_default
        else
            let &fileencodings = &fileencodings .','. s:enc_jis
            set fileencodings+=utf-8,ucs-2le,ucs-2
            if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
                set fileencodings+=cp932
                set fileencodings-=euc-jp
                set fileencodings-=euc-jisx0213
                set fileencodings-=eucjp-ms
                let &encoding = s:enc_euc
                let &fileencoding = s:enc_euc
            else
                let &fileencodings = &fileencodings .','. s:enc_euc
            endif
        endif
        " 定数を処分
        unlet s:enc_euc
        unlet s:enc_jis
    endif
endif
" }}}
" 日本語を含まない場合は fileencoding に encoding を使うようにする{{{
if has('autocmd')
    function! AU_ReCheck_FENC()
        if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
            let &fileencoding=&encoding
        endif
    endfunction
    autocmd MyAutoCmd BufReadPost * call AU_ReCheck_FENC()
endif
" }}}
" 改行コードの自動認識{{{
set fileformats=unix,dos,mac
" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
    set ambiwidth=double
endif
" }}}
" }}}
" 外観{{{
" いろいろ{{{
set notitle
set display=uhex
set scrolloff=1
set wildmenu
set wildmode=list:full
set wildchar=<TAB>
" set wildignore=*.*~
set showbreak=+++
set display=lastline
set laststatus=2
set linebreak
set report=0
" 
" ステータスライン設定 (vim-powerlineで用なしに)
" autocmd MyAutoCmd BufEnter *   if winwidth(0) >= 60 |
" \ set statusline=[%n]\ %t\ %m%R%H%W%y\ %([%{&fenc}][%{&ff}]%)\ %([%l(%p%%),%v]%)(%B)\ |
" " \ set statusline=[%n]\ %t\ %m%R%H%W%y\ %([%{&fenc}][%{&ff}]%)%=\ %([%l(%p%%),%v]%)(%B)\ |
" \ else |
" \ set statusline=[%n]%t |
" \ endif
set wrap
" }}}
" カレントウィンドウのみ罫線を引く{{{
augroup cch
    autocmd! cch
    autocmd! WinLeave * set nocursorline
    autocmd WinEnter,BufRead * set cursorline
augroup END
hi clear Cursorline
hi CursorLine gui=underline
" }}}
" 作業中断中,window移動時のみcursorlineを有効にする{{{
augroup vimrc-auto-cursorline
    autocmd!
    autocmd CursorMoved,CursorMovedI * call s:auto_cursorline('CursorMoved')
    autocmd CursorHold,CursorHoldI * call s:auto_cursorline('CursorHold')
    autocmd WinEnter * call s:auto_cursorline('WinEnter')
    autocmd WinLeave * call s:auto_cursorline('WinLeave')

    let s:cursorline_lock = 0
    function! s:auto_cursorline(event)
        if a:event ==# 'WinEnter'
            setlocal cursorline
            let s:cursorline_lock = 2
        elseif a:event ==# 'WinLeave'
            setlocal nocursorline
        elseif a:event ==# 'CursorMoved'
            if s:cursorline_lock
                if 1 < s:cursorline_lock
                    let s:cursorline_lock = 1
                else
                    setlocal nocursorline
                    let s:cursorline_lock = 0
                endif
            endif
        elseif a:event ==# 'CursorHold'
            setlocal cursorline
            let s:cursorline_lock = 1
        endif
    endfunction
augroup END
" }}}
" 全角スペースを可視化{{{
if has('syntax')
    augroup ZenkakuSpace
        autocmd!
        autocmd VimEnter,WinEnter,BufEnter * match ZenkakuSpace /　/
        if has('gui_running')
            autocmd ColorScheme * highlight ZenkakuSpace term=underline ctermfg=Red gui=underline guifg=Red guibg=#666666
        else
            highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=#666666
        endif
        " autocmd BufNewFile,BufRead * match ZenkakuSpace /　/
    augroup END
endif
" }}}
" tabline周り{{{
" 参考:http://d.hatena.ne.jp/thinca/20111204/1322932585
function! MakeTabLine()
    let titles = map(range(1, tabpagenr('$')),'s:tabpage_label(v:val)' )
    let sep = '|'
    let tabpages = join(titles , sep) . sep . '%#TabLineFill#%T'
    let path = fnamemodify(getcwd(),":~")
    let time = strftime("%H:%M") 
    return   tabpages . '%=' .path .' '. time 
endfunction
set tabline=%!MakeTabLine()
function! s:tabpage_label(n)
    " t:titleという変数があったらそれを使う
    let title = gettabvar(a:n, 'title')
    if title !=# ''
        return title
    endif
    " タブページ内のバッファのリスト
    let bufnrs = tabpagebuflist(a:n)
    " カレントタブページかどうかでハイライトを切り替える
    let hi = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'
    " バッファが複数あったらバッファ数を表示
    let no = len(bufnrs)
    if no is 1
        let no = ''
    endif
    " タブページ内に変更ありのバッファがあったら'+'をつける
    let mod = len(filter(copy(bufnrs), 'getbufvar(v:val, "&modified")')) ? '[+]' : ''
    let sp = (no . mod) ==# '' ? '' : ' ' " 隙間を空ける
    " カレントバッファ
    let curbufnr = bufnrs[tabpagewinnr(a:n) - 1] " tabpagewinnr()は1 origin
    let fname = pathshorten(bufname(curbufnr))
    if fname == ''
        let fname = ' '
    endif
    let label = no. sp . fname . mod 
    return '%' . a:n . 'T' . hi . label .  '%T%#TabLineFill#'
endfunction
" }}}
" Indent {{{
filetype indent on
set autoindent
set smartindent
set smarttab
set expandtab
set shiftwidth=4
set tabstop=4
" }}}
" }}}
" Backup{{{
set backup
if exists("*strftime")
    au MyAutoCmd BufWritePre * let &bex = '-' . strftime("%y%m%d") . '~'
elseif
    au MyAutoCmd BufWritePre * let &bex = '-' . localtime("%y%m%d") . '~'
endif
" }}}
" 検索 {{{
set ignorecase
set smartcase
set incsearch
set showmatch
set nowrapscan
" }}}
" キーマップ{{{
" swap ; :{{{
nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;
" }}}
nnoremap j gj
nnoremap k gk
" +/-キーで画面サイズ変更{{{
noremap + <C-w>+
noremap - <C-w>-
" }}}
" vimhack#106
command! Big wincmd _ | wincmd |
" kana's useful tab function {{{
function! s:move_window_into_tab_page(target_tabpagenr)
    " Move the current window into a:target_tabpagenr.
    " If a:target_tabpagenr is 0, move into new tab page.
    if a:target_tabpagenr < 0  " ignore invalid number.
        return
    endif
    let original_tabnr = tabpagenr()
    let target_bufnr = bufnr('')
    let window_view = winsaveview()
    if a:target_tabpagenr == 0
        tabnew
        tabmove  " Move new tabpage at the last.
        execute target_bufnr 'buffer'
        let target_tabpagenr = tabpagenr()
    else
        execute a:target_tabpagenr 'tabnext'
        let target_tabpagenr = a:target_tabpagenr
        topleft new  " FIXME: be customizable?
        execute target_bufnr 'buffer'
    endif
    call winrestview(window_view)
    execute original_tabnr 'tabnext'
    if 1 < winnr('$')
        close
    else
        enew
    endif
    execute target_tabpagenr 'tabnext'
endfunction
" }}}
" <Leader>to move current buffer into a new tab.
nnoremap <silent> <Leader>to :<C-u>call <SID>move_window_into_tab_page(0)<CR>
" Yank/Past to the OS clipboard{{{
nmap <Leader>y "+y
nmap <Leader>Y "+yy
nmap <Leader>p "+p
nmap <Leader>P "+p
" }}}
" タブまわり{{{
nnoremap <Leader>tl :tabnext<CR>
nnoremap <Leader>th :tabprevious<CR>
nnoremap <Leader>tn :tabnew<CR>
" command! TL :tabnext 
" command! TH :tabprevious
" command! TN :tabnew
" }}}
inoremap jj <ESC>
" ページスクロール{{{
nmap <Space> <C-f>
nmap <S-Space> <C-b>
" }}}
" }}}
" 検索周辺{{{
" 検索移動を見やすく{{{
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
nnoremap G Gzz
" }}}
" <ESC> 関連 {{{
" IME
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
" <ESC> or <C-c> key reset Highlight
nnoremap <silent> <ESC> <ESC>:<C-u>nohlsearch<CR>:<C-u>set iminsert=0<CR>
" }}}
" help{{{
nnoremap <C-i><Space> :<C-u>h<Space>
nnoremap <C-i><C-i> :<C-u>h<Space><C-r><C-w><Enter>
" }}}
nnoremap R gR
" Kでカーソル位置の単語をヘルプ検索{{{
inoreabbrev <expr> dl* repeat('*','80')
inoreabbrev <expr> dl- repeat('-','80')
" }}}
" gb:最後の編集位置へ移動{{{
" 参考::https://sites.google.com/site/fudist/Home/vim-nihongo-ban/tips
nnoremap gb `.zz
" nnoremap gi gbz<Enter>
" <C-g><M-g>:編集位置を順に巡る
nnoremap <C-g> g;
nnoremap <M-g> g,
" vb:最後の編集箇所を選択
nnoremap vb `[v`]
" }}}
" window分割していないとき、<C-w><C-w>で裏バッファへ切り替え{{{
" https://sites.google.com/site/fudist/Home/vim-nihongo-ban/tips
nnoremap <silent> <C-w><C-w> :<C-u>call MyWincmdW()<CR>
nnoremap <silent> <C-w>w :<C-u>call MyWincmdW()<CR>
function! MyWincmdW()
    let pn = winnr()
    silent! wincmd w
    if pn == winnr()
        silent! b#
    endif
endfunction
" }}}
" Emacsに倣った挙動{{{
cnoremap <C-f> <Right>
inoremap <C-f> <Right>
cnoremap <C-b> <Left>
inoremap <C-b> <Left>
cnoremap <C-a> <Home>
inoremap <C-a> <Home>
cnoremap <C-e> <End>
inoremap <C-h> <Backspace>
" }}}
" ウィンドウ移動簡略化 & サイズ調整{{{
nnoremap <C-j> <C-w>j:call <SID>good_height()<CR>
nnoremap <C-k> <C-w>k:call <SID>good_height()<CR>
nnoremap <C-h> <C-w>h:call <SID>good_width()<CR>
nnoremap <C-l> <C-w>l:call <SID>good_width()<CR>

function! s:good_width()
    if winwidth(0) < 44
        vertical resize 44
    endif
endfunction

function! s:good_height()
    if winheight(0) < 10
        resize 10
    endif
endfunction
" }}}
" Emacsに倣ったウィンドウ操作{{{
nnoremap <silent> <C-x>1 :<C-u>only<CR>
nnoremap <silent> <C-x>2 :<C-u>sp<CR>
nnoremap <silent> <C-x>3 :<C-u>vsp<CR>
" }}}
" Buff{{{
nnoremap <Leader>bn :<C-u>bn<CR>
nnoremap <Leader>bp :<C-u>bp<CR>
nnoremap <Leader>bd :<C-u>bdelete<CR>
" }}}
" Tab Page{{{
" nnoremap <S-t> :<C-u>tabnew<CR>
" nnoremap <S-h> :<C-u>tabp<CR>
" nnoremap <S-l> :<C-u>tabn<CR>

nnoremap <Leader>k H
nnoremap <Leader>j L
" }}}
" クリップボード風コピペ{{{
vnoremap <C-c> "*y
imap <C-v> <ESC>"*pa
vmap <C-v> d"*P
cmap <C-v> <C-r>*
" }}}
" 選択文字列を検索 <- visualstarでお役御免?{{{
vnoremap <silent> // y/<C-R>=escape(@", '\\/.*$^~[]')<CR><CR>
" 選択した文字列を置換
vnoremap /r "xy;%s/<C-R>=escape(@x, '\\/.*$^~[]')<CR>//gc<Left><Left><Left>"
" }}}
" ヤンクした文字列とカーソル位置の単語を置換する{{{
" vimバイブル4-6
nnoremap <silent>  cy  ce<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
vnoremap <silent>  cy   c<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
nnoremap <silent> ciy ciw<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
" }}}
" 範囲選択によるインデントを連続して行う{{{
vnoremap > >gv
vnoremap < <gv
" }}}
" 選択範囲をvimscriptとして実行{{{
nnoremap <Leader>do   Vy:@"<Enter>
vnoremap <Leader>eval y:@"<Enter>
nnoremap <C-x><C-e> Vy:@"<Enter>
" }}}
" }}}
" Plugin{{{
if filereadable(expand('~/.vimrc.plugin'))
    source ~/.vimrc.plugin
endif
" }}}
" 言語別設定 {{{
" Prefix{{{
augroup programLanguage
    au!
    autocmd Filetype vim        call VimrcSettings()
    autocmd Filetype tex        call LaTeXSettings()
    autocmd Filetype c,cpp,objc call CSettings()
    autocmd Filetype cpp        call CppSettings()
    autocmd Filetype objc       call ObjCSettings()
    autocmd Filetype java       call JavaSettings()
    autocmd Filetype perl       call PerlSettings()
    autocmd Filetype ruby       call RubySettings()
    autocmd Filetype lisp       call LispSettings()
    autocmd Filetype sh         call ShellScriptSettings()
    autocmd Filetype snippet    call SnippetSettings()
    autocmd Filetype xml        call XMLtSettings()
    autocmd Filetype html       call HTMLSettings()
    autocmd Filetype php       call PHPSettings()
augroup END
" }}}
" vimrc{{{
function! VimrcSettings()
    inoremap ' ''<Left>
    inoremap " ""<left>
    inoremap ( ()<Left>
endfunction
" }}}
" C{{{
function! CSettings()
    set shiftwidth=4
    set tabstop=4
    set cindent
    set cinkeys+=;
    set dictionary=$HOME/.vim/dict/c.dict
    inoremap /* /**/<Left><Left>
    inoremap , ,<Space>
    set fdm=indent
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap <buffer> <  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
    inoremap <buffer><expr>; GetSemicolonForC()
    "最寄りの中括弧内を選択
    nnoremap <buffer>v} ?{<CR>%v%0
    call SmartCharForC()
endfunction
" }}}
" C++{{{
function! CppSettings()
    set dictionary+=$HOME/.vim/dict/cpp.dict
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap<buffer> <  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
endfunction
" }}}
" tex{{{
function! LaTeXSettings()
    set dictionary=$HOME/.vim/dict/tex.dict
    inoremap <buffer> { {}<Left>
    inoremap <buffer> [ []<Left>
    inoremap <buffer> ( ()<Left>
    inoremap <buffer> $ $$<Left>
    inoremap <buffer> \<= \leq
    inoremap <buffer> \>= \geq
    inoremap <buffer> \<< \ll
    inoremap <buffer> \>> \gg
    inoremap <buffer> \+- \pm
    inoremap <buffer> \-+ \mp
    nnoremap <buffer> <F5> :<C-u>!platex-utf8 %<CR>
    nnoremap <buffer> <F6> :<C-u>!dvipdfmx %<<CR>
    nnoremap <buffer> <F7> :<C-u>!open %<.pdf<CR>
endfunction
" }}}
" opjc{{{
function! ObjCSettings()
    ""set dictionary=$HOME/.vim/dict/cocoa.dict
    set iskeyword+=:
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
    inoremap <buffer><expr>; GetSemicolonForC()
    nnoremap <buffer><silent> ga :call <SID>AlternateFile()<CR>
    call SmartCharForC()
    call SmartCharForObjc()
    "inoremap <  <><Left>
endfunction
" }}}
" Java{{{
function! JavaSettings()
    set dictionary=$HOME/.vim/dict/java.dict
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap <buffer><  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
    inoremap <buffer><expr>; GetSemicolonForC()
endfunction
" }}}
" Ruby{{{
function! RubySettings()
    set dictionary=$HOME/.vim/dict/ruby.dict
    inoremap  <buffer> ( ()<Left>
    inoremap  <buffer> [ []<Left>
    inoremap  <buffer> <> <><Left>
    "inoremap <buffer> <  <><Left>
    inoremap  <buffer> " ""<Left>
    inoremap  <buffer> ' ''<Left>
    set shiftwidth=2
    set tabstop=2
endfunction
" }}}
" Lisp{{{
function! LispSettings()
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap <buffer><  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
    set shiftwidth=3
    set tabstop=3
endfunction
" }}}
" snipptfile{{{
function! SnippetSettings()
    inoremap $ ${}<Left>
    inoremap { {}<Left>
endfunction
" Shell Script

function! ShellScriptSettings()
    inoremap $ ${}<Left>
    inoremap { {}<Left>
    inoremap ` ``<Left>
endfunction
" }}}
" XML {{{
function! XMLtSettings()
    inoremap <buffer> </ </<C-X><C-o>
endfunction
" }}}
" HTML{{{
function! HTMLSettings()
    inoremap , ,<Space>
    inoremap <buffer>{ {}<Left>
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
endfunction
" }}}
" PHP{{{
function! PHPSettings()
    set dictionary=$HOME/.vim/dict/PHP.dict
    inoremap , ,<Space>
    set fdm=indent
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap <buffer> <  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
endfunction
" }}}
" その他{{{
" 指定ファイルに対応する雛形を読む{{{
augroup SkeletonAu
    autocmd!
    " autocmd BufNewFile *.html 0r $HOME/.vim/skeleton/skel.html
    autocmd BufNewFile *.pl  0r $VIMFILE_DIR/skeleton/skel.pl
    autocmd BufNewFile *.pm  0r $VIMFILE_DIR/skeleton/skel.pm
    autocmd BufNewFile *.c   0r $VIMFILE_DIR/skeleton/skel.c
    autocmd BufNewFile *.tex 0r $VIMFILE_DIR/skeleton/skel.tex
    autocmd BufNewFile *.rb  0r $VIMFILE_DIR/skeleton/skel.rb
augroup END
" }}}
" 任意の文字数ずつUndo{{{
function! s:is_changed() "{{{
    try
        " When no `b:vimrc_changedtick` variable
        " (first time), not changed.
        return exists('b:vimrc_changedtick')
                    \   && b:vimrc_changedtick < b:changedtick
    finally
        let b:vimrc_changedtick = b:changedtick
    endtry
endfunction "}}}
augroup Change
    autocmd!
    autocmd CursorMovedI * if s:is_changed() | doautocmd User changed-text | endif
augroup END
let s:current_changed_times = 0
let s:max_changed_times = 20 "任意の文字数
function! s:changed_text() "{{{
    if s:current_changed_times >= s:max_changed_times - 1
        call feedkeys("\<C-g>u", 'n')
        let s:current_changed_times = 0
    else
        let s:current_changed_times += 1
    endif
endfunction "}}}
augroup Change
    au!
    autocmd  User changed-text call s:changed_text()
augroup END
" }}}
" 純粋な(コメント,空行を除いた)vimrc戦闘力を図る {{{
" :Scouter
function! Scouter(file, ...)
    let pat = '^\s*$\|^\s*"'
    let lines = readfile(a:file)
    if !a:0 || !a:1
        let lines = split(substitute(join(lines, "\n"), '\n\s*\\', '', 'g'), "\n")
    endif
    return len(filter(lines,'v:val !~ pat'))
endfunction
command! -bar -bang -nargs=? -complete=file Scouter
            \        echo Scouter(empty(<q-args>) ? $MYVIMRC : expand(<q-args>), <bang>0)
command! -bar -bang -nargs=? -complete=file GScouter
            \        echo Scouter(empty(<q-args>) ? $MYGVIMRC : expand(<q-args>), <bang>0)
" }}}
" vimでモーション(or選択範囲)をレジスタの内容で置き換えるオペレータ{{{
function! ReplaceMotion(type, ...)
    let sel_save = &selection
    let &selection = "inclusive"
    let reg_save = @@
    let mark_save = getpos("'a")

    if a:0 " visual mode
        silent exe "normal! '>$"
        if getpos("'>") == getpos('.')
            silent exe 'normal! `<"_d`>"_d$"0p`<'
        else
            silent exe 'normal! `>lma`<"_d`a"0P`<'
        endif
    elseif a:type == 'char' " char motion
        silent exe "normal! ']$"
        if getpos("']") == getpos('.')
            silent exe 'normal! `["_d`]"_d$"0p`['
        else
            silent exe 'normal! `]lma`["_d`a"0P`['
        endif
    endif

    let &selection = sel_save
    let @@ = reg_save
    call setpos("'a", mark_save)
endfunction
" }}}
" Rename コマンド:編集中のバッファのファイル名を変更する{{{
" jigokuno.com
command! -nargs=+ -bang -complete=file Rename let pbnr=fnamemodify(bufname('%'), ':p')|exec 'f '.escape(<q-args>, ' ')|w<bang>|call delete(pbnr)
" }}}
" DiffOrig コマンド:現バッファの差分表示。{{{
" Diff コマンド:ファイルまたはバッファ番号を指定して差分表示。#なら裏バッファと比較
command! -nargs=? -complete=file Diff if '<args>'=='' | browse vertical diffsplit|else| vertical diffsplit <args>|endif
" }}}
" 指定した文字コードで開き直すコマンド群{{{
" http://zudolab.net/blog/?p=132
command! ChgEncCp932         edit ++enc=cp932
command! ChgEncEucjp         edit ++enc=euc-jp
command! ChgEncIso2022jp     edit ++enc=iso-2022-jp
command! ChgEncJis Iso2022jp
command! ChgEncUtf8          edit ++enc=utf-8
command! ChgEncSjis          edit ++enc=cp932
" }}}
" 開いているファッファの文字コードを変えるコマンド群{{{
" http://zudolab.net/blog/?p=132
" change encoding commands
command! ChgFencCp932               set fenc=cp932
command! ChgFencEucjp               set fenc=euc-jp
command! ChgFencIso2022jp           set fenc=iso-2202-jp
command! ChgFencJis ChgencIso2022jp
command! ChgFencUtf8                set fenc=utf-8
command! ChgFencSjis                set fenc=cp932
" }}}
" スペルチェックトリガー{{{
command! CheckSpell :set spell!
" }}}
" フォルダ展開を楽に{{{
" https://gist.github.com/1240267
nnoremap <expr> h
            \   col('.') == 1 && foldlevel(line('.')) > 0 ? 'zc' : 'h'

nnoremap <expr> l
            \   foldclosed(line('.')) != -1 ? 'zo' : 'l'
" }}}
" :AllMaps{{{
command!
            \   -nargs=* -complete=mapping
            \   AllMaps
            \   map <args> | map! <args> | lmap <args>
" }}}
" 日付入力簡易マクロ{{{
inoremap <Leader>date <C-R>=strftime('%Y/%m/%d (%a)')<CR>
inoremap <Leader>time <C-R>=strftime('%H:%M')<CR>
inoremap <Leader>w3cd <C-R>=strftime('%Y-%m-%dT%H:%M:%S+09:00')<CR>
inoremap <expr> <Leader>df strftime('%Y/%m/%d %H:%M:%S')
inoremap <expr> <Leader>dd strftime('%Y/%m/%d')
inoremap <expr> <Leader>dt strftime('%H:%M:%S')
" }}}
" 文字数カウント
command! -range=% Count :<line1>,<line2>s/.//gn
" }}}
"============================================================
" vim:set tabstop=4 shiftwidth=4 fdm=marker fdl=0: 
" undo履歴保存して再開させる{{{
if has('persistent_undo')
    set undodir=~/.vimundo
    set undofile
endif 
" }}}
