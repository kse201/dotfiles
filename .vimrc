"============================================================
"                      *** .vimrc ***                       |
"                 Last Change: 07-Nov-2013.                 |
"============================================================

" basic setting{{{
augroup MyAutoCmd
    autocmd!
augroup END

let mapleader=','
set shortmess+=I
set nocompatible
set modeline
set noswapfile
set complete+=k
set noequalalways
set history=1000
set vb t_vb=
set autowrite
set backspace=indent,eol,start
set diffopt=filler,vertical
set showcmd
set showtabline=2
set guioptions+=c
set guioptions-=e
set formatoptions-=ro 
set report=0
set formatexpr=
set autoread
set clipboard=unnamed
set hidden
set timeout timeoutlen=500 ttimeoutlen=75
" \ -> ¥{{{
if has('mac')
    inoremap ¥ \
    cnoremap ¥ \
endif
" }}}

" Meta in MacVim {{{
if exists('+macmeta')
    set macmeta
endif
" }}}

" reuse undo history{{{
if has('persistent_undo')
    set undodir=~/.vimundo
    set undofile
endif 
" }}}

" configs and dirctorys {{{
if has('win32') || has('win64') 
    let $VIMFILE_DIR   = $HOME . '/dotfiles/.vim'
    let $MYVIMRC       = "~/dotfiles/.vimrc"
    let $MYGVIMRC      = "~/dotfiles/.gvimrc"
    let $MYVIMRCPLUGIN = "~/dotfiles/.vimrc.plugin"
    set backupdir=$HOME/_vimbackup
    set dir=$HOME/AppData/Local/Temp
elseif has('win32unix')
    let $VIMFILE_DIR   = $HOME . '/vimfiles'
    let $MYVIMRC       = "~/vimfiles/_vimrc"
    let $MYGVIMRC      = "~/vimfiles/_gvimrc"
    let $MYVIMRCPLUGIN = "~/vimfiles/_vimrc.plugin"
    set backupdir=$HOME/_vimbackup
    set dir=$HOME/AppData/Local/Temp
elseif has('mac')
    let $VIMFILE_DIR   = $HOME . '/.vim'
    let $MYVIMRCPLUGIN = $HOME . "/.vimrc.plugin"
    let $MYVIMRC       = "~/.vimrc"
    let $MYGVIMRC      = "~/.gvimrc"
    let $MYVIMRCPLUGIN = "~/.vimrc.plugin"
    set backupdir=$HOME/.vimbackup
else
    let $VIMFILE_DIR   = $HOME . '/.vim'
    let $MYVIMRC       = "~/.vimrc"
    let $MYGVIMRC      = "~/.gvimrc"
    let $MYVIMRCPLUGIN = $HOME . "/.vimrc.plugin"
    set backupdir=$HOME/.vimbackup
endif
" }}}

" dropbox dir {{{
if has('win32') || has('win64') 
    let $DROPBOX_DIR   = $HOME . '\Documents\My Dropbox'
elseif has('win32unix')
    let $DROPBOX_DIR   = $HOME . '/Documents/My Dropbox'
elseif has('mac')
    let $DROPBOX_DIR   = $HOME . '/Dropbox'
else
    let $DROPBOX_DIR   = $HOME . '/Documents\My Dropbox'
endif
" }}}

" edit configs {{{
nnoremap <Leader>ev :edit $MYVIMRC<CR>
nnoremap <Leader>eg :edit $MYGVIMRC<CR>
nnoremap <Leader>ep :edit $MYVIMRCPLUGIN<CR>
nnoremap <Leader>em :edit $DROPBOX_DIR/documents/memo<CR>
" }}}

" Auto Loading .vimrc,.gvimrc {{{
if has("autocmd")
    filetype plugin indent on
    autocmd MyAutoCmd BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \ exe "normal! g'\"" |
                \ endif
    autocmd BufEnter * :cd %:p:h
endif

command! ReloadVimrc source $MYVIMRC
command! ReloadGVimrc source $MYGVIMRC
command! ReloadPlugin source $MYVIMRCPLUGIN

" auto changeing color when reload .vimrc 
autocmd MyAutoCmd BufWritePost $MYVIMRC  source $MYVIMRC  |
            \ if has('gui_running') |
            \ source $MYGVIMRC
autocmd MyAutoCmd BufWritePost $MYGVIMRC source $MYGVIMRC
autocmd MyAutoCmd BufWritePost $MYVIMRCPLUGIN source $MYGVIMRC
" }}}

" Auto delete line-end Space{{{
augroup Autoplace
    autocmd!
    autocmd BufWritePre *.[^{mkd}] :%s/\s\+$//ge
augroup END 

" }}}

" Terminal cursor{{{
if !has('gui')
    set t_Co=256
    " export TERM=xterm-256color
    colorscheme desert
    inoremap 0D <Left>
    inoremap 0B <Down>
    inoremap 0C <Right>
    inoremap 0A <Up>
endif
" }}}

" auto ctags {{{
set tags+=~/.tags,**/tags
if filereadable(expand('./tags'))
    au MyAutoCmd FileWritePre :!ctasg -R
endif
" }}}

" Auto Change dir{{{
if has('win32') || has('win64')
    au MyAutoCmd BufEnter * execute ":lcd " . escape(expand("%:p:h")," #")
else
    au MyAutoCmd BufEnter * execute ":lcd " . escape(expand("%:p:h")," #¥") 
endif
" }}}

" clipboard {{{
if has('win32') || has('win64')
    set clipboard+=unnamed
endif
" }}}

" IME settings
if has('multi_byte_ime') || has('xim') || has('gui_macvim')
    "insert mode :lmap off, IME ON
    set iminsert=2
    "Search mode : lmap off, IME ON
    set imsearch=2
    "Normal mode : IME off
    inoremap <silent> <ESC> <ESC>:set iminsert=0
endif
" }}}

" file encoding {{{
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
        " eucJP-ms support check
        if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
            let s:enc_euc = 'eucjp-ms'
            let s:enc_jis = 'iso-2022-jp-3'
            " JISX0213 support check
        elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
            let s:enc_euc = 'euc-jisx0213'
            let s:enc_jis = 'iso-2022-jp-3'
        endif
        " fileencodings
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
        unlet s:enc_euc
        unlet s:enc_jis
    endif
endif
" }}}

" use encoding as fileencoding when file NOT contan japanese {{{
if has('autocmd')
    function! AU_ReCheck_FENC()
        if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
            let &fileencoding=&encoding
        endif
    endfunction
    autocmd MyAutoCmd BufReadPost * call AU_ReCheck_FENC()
endif
" }}}

" auto recogitation fileformats {{{
set fileformats=unix,dos,mac
" support  code such as □ , ○ and etc.
if exists('&ambiwidth')
    set ambiwidth=double
endif
" }}}
" }}}

" surface {{{
set notitle
set display=uhex
set scrolloff=1
set showbreak=>\ >\ >
set display=lastline
set laststatus=2
set wrap

set wildmenu
set wildmode=list:full
set wildchar=<TAB>

" Indent {{{
filetype indent on
set autoindent
set smartindent
set smarttab
set expandtab
set shiftwidth=4
set tabstop=4
" }}}

" status line 
autocmd MyAutoCmd BufEnter *   if winwidth(0) >= 60 |
            \ set statusline=[%n]\ %t\ %m%R%H%W%y\ %([%{&fenc}][%{&ff}]%)\ %([%l(%p%%),%v]%)(%B)\ |
            \ else |
            \ set statusline=[%n]%t |
            \ endif

" draw cursorline onry current window{{{
augroup cch
    autocmd! cch
    autocmd! WinLeave * set nocursorline
    autocmd WinEnter,BufRead * set cursorline
augroup END
hi clear Cursorline
hi CursorLine gui=underline

augroup auto-cursorline
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

" change Zenkaku blank and line-end space to visible {{{
if has('syntax')
    augroup SpaceHilights
        autocmd!
        autocmd VimEnter,WinEnter,BufEnter * match ZenkakuSpace /　/
        if has('gui_running')
            autocmd ColorScheme * highlight ZenkakuSpace term=underline ctermfg=Red gui=underline guifg=Red guibg=#666666
        else
            highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=#666666
        endif
        " autocmd BufNewFile,BufRead * match ZenkakuSpace /　/
        autocmd ColorScheme * highlight link TrailingSpaces Error
        autocmd Syntax + syntax match TrailingSpaces containedin=ALL /\s\+$/
    augroup END
endif
" }}}

" tabline{{{
" ref:http://d.hatena.ne.jp/thinca/20111204/1322932585
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
    if v:version >= 703 
        let title = gettabvar(a:n, 'title')
    else
        let title = ''
    endif

    if title !=# ''
        return title
    endif

    " タブページ内のバッファのリスト
    let bufnrs = tabpagebuflist(a:n)
    " カレントタブページかどうかでハイライトを切り替える
    let hi = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

    " タブページ内に変更ありのバッファがあったら'+'をつける
    let mod = len(filter(copy(bufnrs), 'getbufvar(v:val, "&modified")')) ? '[+]' : ''
    " let sp = (no . mod) ==# '' ? '' : ' ' " 隙間を空ける
    " カレントバッファ
    let curbufnr = bufnrs[tabpagewinnr(a:n) - 1] " tabpagewinnr()は1 origin
    let fname = pathshorten(bufname(curbufnr))

    if fname == ''
        let fname = ' '
    endif

    let label = a:n .":" .  fname . mod 

    return '%' . a:n . 'T' . hi . label .  '%T%#TabLineFill#'

endfunction

" }}}
"
" change colorscheme airily{{{
" ref:http://vim-users.jp/2011/09/hack228/
let ColorRoller = {}
let ColorRoller.colors = [
            \ 'eclipse',
            \ 'vc',
            \ 'print_bw',
            \ 'desert' ,
            \ 'wombat256mod',
            \ 'hickop',
            \ 'molokai'
            \]

function! ColorRoller.change()
    let color = get(self.colors, 0)
    " " tabpagecolorscheme を使用している場合は↓の "colorscheme" を "Tcolorscheme" に変える。
    silent exe "colorscheme " . color
    redraw
    echo self.colors
endfunction

function! ColorRoller.roll()
    let item = remove(self.colors, 0)
    call insert(self.colors, item, len(self.colors))
    call self.change()
endfunction

function! ColorRoller.unroll()
    let item = remove(self.colors, -1)
    call insert(self.colors, item, 0)
    call self.change()
endfunction

nnoremap <silent><F9>   :<C-u>call ColorRoller.roll()<CR>
nnoremap <silent><S-F9> :<C-u>call ColorRoller.unroll()<CR>
" endif
" }}}
" }}}

" Backup{{{
set backup
if exists("*strftime")
    au MyAutoCmd BufWritePre * let &bex = '-' . strftime("%y%m%d%H%M") . '~'
elseif
    au MyAutoCmd BufWritePre * let &bex = '-' . localtime("%y%m%d%H%M") . '~'
endif
" }}}

" search {{{
set ignorecase
set smartcase
set incsearch
set showmatch
set nowrapscan

augroup Help
    autocmd!
    autocmd Filetype vim nnoremap <buffer><silent> K :<C-u>help<Space><C-r><C-w><CR> " カーソル下の単語をhelp検索
    autocmd filetype vim vnoremap <buffer><silent> K "vy:<C-u>help<Space><C-r>=substitute(escape(@v,'\/'),"\n",'\\n','g')<CR><CR> "選択中単語をhelp検索
augroup END
" }}}

" <ESC>  {{{
" IME
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
" <ESC> or <C-c> key reset Highlight
if has("gui")
    nnoremap <silent> <ESC> <ESC>:<C-u>nohlsearch<CR>:<C-u>set iminsert=0<CR>
endif
" }}}

" Language setting {{{
" Prefix{{{
let g:FileTypeSettings = [
            \ "c", 
            \ "cpp", 
            \ "cs", 
            \ "java", 
            \ "vim", 
            \ "objc", 
            \ "ruby", 
            \ "perl", 
            \ "lisp", 
            \ "sh", 
            \ "snippet", 
            \ "xml", 
            \ "html", 
            \ "php",
            \ "markdown",
            \ "tex"
            \]
for MyFileType in g:FileTypeSettings
    execute "autocmd MyAutoCmd FileType " . MyFileType . " call My" . MyFileType . "Settings()"
endfor
" }}}
" vim{{{
function! MyvimSettings()
    inoremap ' ''<Left>
    inoremap " ""<left>
    inoremap ( ()<Left>
endfunction
" }}}
" C{{{
function! MycSettings()
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
    nnoremap <buffer>v} ?{<CR>%v%0
    nnoremap <Leader>c :make<CR>
endfunction
" }}}
" C++{{{
function! MycppSettings()
    call MycSettings()
    set dictionary+=$HOME/.vim/dict/cpp.dict
endfunction
" }}}
" cs {{{
function! MycsSettings()
    call MycSettings()
    if has("folding")
        set fdl = 3
    endif
endfunction
"}}}
" tex{{{
function! MytexSettings()
    set dictionary=$HOME/.vim/dict/tex.dict
    set sw=2
    set tw=0
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
    nnoremap <buffer> <F5> :<C-u>!platex-euc %<CR>
    nnoremap <buffer> <F6> :<C-u>!dvipdfmx %<<CR>
    nnoremap <buffer> <F7> :<C-u>!open %<.pdf<CR>
    nmap <buffer> <F10> <F5><F6><F7>
    nmap <buffer> <Leader>make <F5><F6><F7>
    function! ChangePunctuation()
        %s/、/，/ge
        %s/。/./ge
    endfunction
    augroup TeX
        autocmd!
        autocmd BufWritePre *.tex call ChangePunctuation()
    augroup  END

endfunction
" }}}
" opjc{{{
function! MyobjcSettings()
    ""set dictionary=$HOME/.vim/dict/cocoa.dict
    set iskeyword+=:
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
    nnoremap <buffer><silent> ga :call <SID>AlternateFile()<CR>
    " call SmartCharForC()
    " call SmartCharForObjc()
    "inoremap <  <><Left>
endfunction
" }}}
" Java{{{
function! MyjavaSettings()
    set dictionary=$HOME/.vim/dict/java.dict
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap <buffer><  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
endfunction
" }}}
" Ruby{{{
function! MyrubySettings()
    set dictionary=$HOME/.vim/dict/ruby.dict
    inoremap  <buffer> ( ()<Left>
    inoremap  <buffer> [ []<Left>
    inoremap  <buffer> <> <><Left>
    "inoremap <buffer> <  <><Left>
    inoremap  <buffer> " ""<Left>
    inoremap  <buffer> ' ''<Left>
    set shiftwidth=2
    set tabstop=2
    let g:ref_use_vimproc=1
    let g:ref_refe_version=2
    nmap ,rr :<C-u>Ref refe<Space>
    let g:rsenseUseOmniFunc=1
    let g:rsenseHome = "/usr/local/Cellar/rsense/0.3/libexec"
    if filereadable(expand('~/rtags'))
        au FileType ruby,eruby setl tags+=~/rtags,~/gtags
    endif
    compiler ruby
    let ruby_space_errors=1 
endfunction
" }}}
" Lisp{{{
function! MylispSettings()
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
function! MysnippetSettings()
    inoremap $ ${}<Left>
    inoremap { {}<Left>
endfunction
" Shell Script

function! MyshSettings()
    inoremap $ ${}<Left>
    inoremap { {}<Left>
    inoremap ` ``<Left>
endfunction
" }}}
" XML {{{
function! MyxmlSettings()
    inoremap <buffer> </ </<C-X><C-o>
endfunction
" }}}
" HTML{{{
function! MyhtmlSettings()
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
function! MyphpSettings()
    set dictionary=$HOME/.vim/dict/PHP.dict
    inoremap , ,<Space>
    if has("folding")
        set fdm=indent
    endif
    inoremap <buffer>{ {}<Left><CR><Up><ESC>o
    inoremap <buffer>( ()<Left>
    inoremap <buffer>[ []<Left>
    inoremap <buffer><> <><Left>
    "inoremap <buffer> <  <><Left>
    inoremap <buffer>" ""<Left>
    inoremap <buffer>' ''<Left>
endfunction
" }}}
" markdown {{{
function! MymarkdownSettings()
    if has("folding")
        setlocal foldmethod=expr
    endif 
    if has ("folding") &&has("eval")
        setlocal foldexpr=Markdown(v:lnum)
    endif

    function! Markdown(lnum)
        let level = matchend(getline(a:lnum), '^#\+')
        return level > 0 ? '>' . level : '='
    endfunction
endfunction
" }}}
" }}}

" other{{{
" auto read templates{{{
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

" undo each unite words{{{
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
let s:max_changed_times = 20 "  arbitrary value
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

" Rename new_filename {{{
" jigokuno.com
command! -nargs=+ -bang -complete=file Rename let pbnr=fnamemodify(bufname('%'), ':p')|exec 'f '.escape(<q-args>, ' ')|w<bang>|call delete(pbnr)
" }}}

" DiffOrig diff with current buffer {{{
" Diff [filanem|buff num]
command! -nargs=? -complete=file Diff if '<args>'=='' | browse vertical diffsplit|else| vertical diffsplit <args>|endif
" }}}

" commands which reopen with encodings{{{
" ref: http://zudolab.net/blog/?p=132
command! ChgEncCp932         edit ++enc=cp932
command! ChgEncEucjp         edit ++enc=euc-jp
command! ChgEncIso2022jp     edit ++enc=iso-2022-jp
command! ChgEncJis Iso2022jp
command! ChgEncUtf8          edit ++enc=utf-8
command! ChgEncSjis          edit ++enc=cp932
" }}}

" commands which change encodings{{{
" ref: http://zudolab.net/blog/?p=132
" change encoding commands
command! ChgFencCp932               set fenc=cp932
command! ChgFencEucjp               set fenc=euc-jp
command! ChgFencIso2022jp           set fenc=iso-2202-jp
command! ChgFencJis ChgencIso2022jp
command! ChgFencUtf8                set fenc=utf-8
command! ChgFencSjis                set fenc=cp932
" }}}

" spell check{{{
command! CheckSpell :set spell!
" }}}

" easy fold expanding {{{
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

" date iput Macro {{{
if exists("*strftime")
    inoremap <Leader>date <C-R>=strftime('%Y/%m/%d (%a)')<CR>
    inoremap <Leader>time <C-R>=strftime('%H:%M')<CR>
    inoremap  <Leader>rr <C-R>=strftime('%H%M%S_%d%b')<CR>
    inoremap <expr> <Leader>df strftime('%Y/%m/%d %H:%M:%S')
    inoremap <expr> <Leader>dd strftime('%Y/%m/%d')
    inoremap <expr> <Leader>dt strftime('%H:%M:%S')
endif
" }}}

" auto insert filename at *.memo{{{ 
augroup AutoMemo
    au! 
    au BufNewFile *.memo call MyMemoSetting()
augroup END

function! MyMemoSetting ()
    call append(0,expand('%:t:r'))
    call append(1,repeat('=',len(expand('%:t:r'))))
endfunction

" }}}

" count characters{{{
command! -range=% Count :<line1>,<line2>s/.//gn
" }}}
" count words {{{
command! -range=% Word :<line1>,<line2>s/\i\+//gn
" }}}

" smart split window{{{
" ref http://qiita.com/items/392be95a195067d84fd8
command! -nargs=? -complete=command SmartSplit call <SID>smart_split(<q-args>)
nnoremap <silent><C-w><Space> :<C-u>SmartSplit<CR>
function! s:smart_split(cmd)
    if winwidth(0)/2  >= winheight(0) * 1.6
        vsplit 
    else 
        split
    endif

    if !empty(a:cmd)
        execute a:cmd
    endif
endfunction
" }}}

" rotate point line-head(with blank) -> line-head -> line-end {{{
" ref: http://qiita.com/items/ee4bf64b1fe2c0a32cbd
nnoremap <silent>^ :<C-u>call <SID>rotate_in_line()<CR>
function! s:rotate_in_line()
    let c = col('.')

    let cmd = c ==1 ? '^' : '$'
    execute "normal! ".cmd

    if c == col('.')
        if cmd == '^'
            normal! $
        else 
            normal! 0
        endif
    endif
endfunction
" }}}
"
" smart block region{{{
"http://labs.timedia.co.jp/2012/10/vim-more-useful-blockwise-insertion.html
vnoremap <expr> I <SID>force_blockwise_visual('I')
vnoremap <expr> A <SID>force_blockwise_visual('A')

function! s:force_blockwise_visual(next_key)
    if mode() ==# 'v'
        return "\<C-v>" . a:next_key
    elseif mode() ==# 'V'
        return "\<C-v>0o$" . a:next_key
    else 
        return a:next_key
    endif
endfunction
" }}}

" follow mode{{{
function! MyFollowMode()
    :vsplit
    normal! <C-b>
    :set scrollbind
    normal! <C-w>w
    :set noscrollbind
    normal! L
    normal! z<CR>
    :set scrollbind
endfunction
" }}}

" insert sequence number{{{
" ref: http://mba-hack.blogspot.jp/2013/01/vim.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+Mba-hack+(MBA-HACK)
nnoremap <silent> co :ContinuousNumber <C-a><CR>
vnoremap <silent> co :ContinuousNumber <C-a><CR>
command! -count -nargs=1 ContinuousNumber let c = col('.')|for n in range(1, <count>?<count>-line('.'):1)|exec 'normal! j' . n . <q-args>|call cursor('.', c)|endfor
" }}}

" function comment {{{
function! FuncComment ()
    normal 40a/
    normal o// Function Name:
    normal o// argument     :
    normal o// return value :
    normal o
    normal 40a/
endfunction
nnoremap <Leader>cmt :call FuncComment()<CR>
" }}}

command! Sudowrite :w !sudo tee %

" easy open file in current dirctory{{{
" ref: http://qiita.com/ukitazume/items/c1d2814497bc036f1a82
cnoremap %% <C-R>=expand('%:h').'/'<cr>                                                                                                              
cmap <leader>e :edit %% 
" }}}

" Writing NIPPO{{{
function! WriteDairyReport()
    if filereadable($VIMFILE_DIR."/skeleton/dairyreport.txt")
      :0r $VIMFILE_DIR/skeleton/dairyreport.txt
  else 
      echo "Don't exiest NIPPO Template!"
  endif
endfunction

function! WriteMonthlyReport()
    if filereadable($VIMFILE_DIR."/skeleton/monthlyreport.txt")
      :0r $VIMFILE_DIR/skeleton/monthlyreport.txt
  else 
      echo "Don't exiest GEPPO Template!"
  endif
endfunction
command!  Nippo :call WriteDairyReport()
command!  Geppo :call WriteMonthlyReport()
" }}}

" }}}

" keymap{{{

nnoremap j gj
nnoremap k gk

nnoremap zl zL
nnoremap zh zH

" http://d.hatena.ne.jp/vimtaku/touch/20121117/1353138802
nnoremap <S-J> gJ
vnoremap <S-J> gJ
nnoremap gJ <S-J>
nnoremap gJ <S-J>


" nnoremap : q:a
" nnoremap / q/a
" window resize +/-{{{
nnoremap + <C-w>+
nnoremap - <C-w>-
nnoremap <silent> <S-Left>  :5wincmd <<CR>
nnoremap <silent> <S-Right> :5wincmd ><CR>
nnoremap <silent> <S-Up>    :5wincmd -<CR>
nnoremap <silent> <S-Down>  :5wincmd +<CR>
" }}}

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
nmap <Leader>pp "+p
nmap <Leader>PP "+p
" }}}

" tab{{{
nnoremap <Leader>tl gt
nnoremap <Leader>th gT
nnoremap gl gt
nnoremap gh gT
nnoremap <Leader>tn :tabnew<CR>
for i in range(1,9)
    execute "nnoremap " . i . "<Leader>t " . i ."gt"
endfor
" command! TL :tabnext 
" command! TH :tabprevious
" command! TN :tabnew
" }}}

inoremap jj <ESC>
inoremap kk <ESC>

" easy date input in filename {{{
" ref:vim tech bible 4-1
cnoremap <expr> <Leader>date strftime('%Y%m%d')
cnoremap <expr> <Leader>time strftime('%Y%m%d%H%M')
" }}}

nnoremap <silent> <C-h> 10h
nnoremap <silent> <C-l> 10l

" easy movin window & fix size{{{
augroup GoodWindowSize
    autocmd!
    autocmd WinEnter * call<SID>good_height()
    autocmd WinEnter * call<SID>good_width()
augroup END
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

nnoremap R gR

" draw line{{{
inoreabbrev <expr> dl* repeat('*','80')
inoreabbrev <expr> dl- repeat('-','80')
" }}}

" gb:Jump last edit point{{{
" ref::https://sites.google.com/site/fudist/Home/vim-nihongo-ban/tips
nnoremap gb `.zz

" nnoremap gi gbz<Enter>
" <C-g><M-g>:jump edit poit forward direction 
nnoremap <C-b> g;
nnoremap <M-b> g,

" vb: select last select region
nnoremap vb `[v`]

" }}}

" change background buffer when no split window{{{
" ref: https://sites.google.com/site/fudist/Home/vim-nihongo-ban/tips
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

" Emacs's cursor moving {{{
cnoremap <C-f> <Right>
inoremap <C-f> <Right>
cnoremap <C-b> <Left>
inoremap <C-b> <Left>
cnoremap <C-a> <Home>
inoremap <C-a> <Home>
cnoremap <C-e> <End>
inoremap <C-h> <Backspace>
" }}}

" Emacs's window handling {{{
nnoremap <silent> <C-x>1 :<C-u>only<CR>
nnoremap <silent> <C-x>2 :<C-u>sp<CR>
nnoremap <silent> <C-x>3 :<C-u>vsp<CR>
nnoremap <silent> <C-w>1 :<C-u>only<CR>
" }}}

" Buff {{{
nnoremap <Leader>bn :<C-u>bn<CR>
nnoremap <Leader>bp :<C-u>bp<CR>
nnoremap <Leader>bd :<C-u>bdelete<CR>
" }}}

" Tab Page{{{
" nnoremap <S-t> :<C-u>tabnew<CR>
" nnoremap <S-h> :<C-u>tabp<CR>
" nnoremap <S-l> :<C-u>tabn<CR>

" nnoremap <Leader>k H
" nnoremap <Leader>j L
" }}}

" search selecting string {{{
vnoremap <silent> // y/<C-R>=escape(@", '\\/.*$^~[]')<CR><CR>
" replace selecting string 
vnoremap /r "xy;%s/<C-R>=escape(@x, '\\/.*$^~[]')<CR>//gc<Left><Left><Left>"
" }}}

" replace a word at cursor and yank string{{{
" ref: vim tech bible 4-6
nnoremap <silent>  cy  ce<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
vnoremap <silent>  cy   c<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
nnoremap <silent> ciy ciw<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
" }}}

" search{{{
" advanceable search {{{
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
nnoremap G Gzz
" }}}

" Grep {{{
set grepprg=grep\ -rnIH\ --exclude-dir=.svn\ --exclude-dir=.git
autocmd QuickfixCmdPost vimgrep copen
autocmd QuickfixCmdPost grep copen

" insert grep format
nnoremap <expr> <Space>g ':vimgrep /\<' . expand('<cword>') . '\>/j **/*.' . expand('%:e')
nnoremap <expr> <Space>G ':sil grep! ' . expand('<cword>') . ' *'
" }}}
" }}}


" repeatable indent handling{{{
vnoremap > >gv
vnoremap < <gv
" }}}

" eval region as Vimscript{{{
nnoremap <Leader>do   Vy:@"<Enter>
vnoremap <Leader>eval y:@"<Enter>
nnoremap <C-x><C-e> Vy:@"<Enter>
" }}}
" }}}

" Plugin{{{
" if filereadable(expand('~/.vimrc.plugin'))
if filereadable(expand($MYVIMRCPLUGIN))
    source $MYVIMRCPLUGIN
endif
" }}}

"============================================================
" vim:set tw=0 tabstop=4 shiftwidth=4 fdm=marker fdl=0: 
