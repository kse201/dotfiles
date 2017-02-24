"============================================================
"                      *** .vimrc ***                       |
"============================================================

" basic setting {{{
augroup MyAutoCmd
    autocmd!
augroup END

let g:mapleader=','
set shortmess+=I
  \ nocompatible
  \ modeline
  \ swapfile
  \ complete+=k
  \ noequalalways
  \ history=100
  \ vb t_vb=
  \ autowrite
  \ autochdir
  \ backspace=indent,eol,start
  \ diffopt=filler,vertical
  \ showcmd
  \ showtabline=2
  \ guioptions+=c
  \ guioptions-=e
  \ formatoptions-=ro
  \ report=0
  \ formatexpr=
  \ autoread
  \ hidden
  \ synmaxcol=200
  \ timeout timeoutlen=500 ttimeoutlen=75
" \ -> \ {{{
if has('mac')
    inoremap \ \
    cnoremap \ \
endif
" }}}

" Meta in MacVim {{{
if exists('+macmeta')
    set macmeta
endif
" }}}

" reuse undo history {{{
if has('persistent_undo')
    set undodir=~/.vimundo undofile
endif
" }}}

" configs and dirctorys {{{
set backupdir=$HOME/.vimbackup
let $VIMFILE_DIR   = $HOME.'/.vim'
let $VIMRC         = $HOME.'/.vimrc'
let $GVIMRC        = $HOME.'/.gvimrc'
let $VIMRC_PLUGING = $HOME.'/.vimrc.plugin'
" }}}

" edit configs {{{
nnoremap <Leader>ev :edit $VIMRC<CR>
nnoremap <Leader>eg :edit $GVIMRC<CR>
nnoremap <Leader>ep :edit $VIMRC_PLUGING<CR>
" }}}
" }}}

" Auto delete line-end Space {{{
augroup Autoplace
    autocmd!
    autocmd BufWritePre * if &filetype != "markdown" | :%s/\s\+$//ge
augroup END
" }}}

" Terminal cursor {{{
if !has('gui')
    set t_Co=256
    inoremap 0D <Left>
    inoremap 0B <Down>
    inoremap 0C <Right>
    inoremap 0A <Up>
endif
" }}}

set tags+=tags;

" IME settings
if has('multi_byte_ime') && has('xim') && has('gui_macvim') " TODO: vefify this equation
    set iminsert=2 imsearch=2
    inoremap <silent> <ESC> <ESC>:set iminsert=0
endif

let g:loaded_gzip              = 1
let g:loaded_tar               = 1
let g:loaded_tarPlugin         = 1
let g:loaded_zip               = 1
let g:loaded_zipPlugin         = 1
let g:loaded_rrhelper          = 1
let g:loaded_2html_plugin      = 1
let g:loaded_vimball           = 1
let g:loaded_vimballPlugin     = 1
let g:loaded_getscript         = 1
let g:loaded_getscriptPlugin   = 1
let g:loaded_netrw             = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_netrwSettings     = 1
let g:loaded_netrwFileHandlers = 1
" }}}

" file encoding {{{
" Auto encoding {{{
if &encoding !=# 'utf-8'
    set encoding=japan fileencoding=japan
endif
if has('iconv')
    let s:enc_euc = 'euc-jp'
    let s:enc_jis = 'iso-2022-jp'
    " eucJP-ms support check
    if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
        let s:enc_euc = 'eucjp-ms'
        let s:enc_jis = 'utf-8'
        " JISX0213 support check
    elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
        let s:enc_euc = 'euc-jisx0213'
        let s:enc_jis = 'utf-8'
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
            let &encoding     = s:enc_euc
            let &fileencoding = s:enc_euc
        else
            let &fileencodings = &fileencodings .','. s:enc_euc
        endif
    endif
    unlet s:enc_euc
    unlet s:enc_jis
endif
" }}}

" use encoding as fileencoding when file NOT contain japanese {{{
if has('autocmd')
    function! g:AU_ReCheck_FENC()
        if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
            let &fileencoding=&encoding
        endif
    endfunction
    autocmd MyAutoCmd BufReadPost * call g:AU_ReCheck_FENC()
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
  \ display=uhex
  \ scrolloff=1
  \ showbreak=+
  \ display=lastline
  \ laststatus=2
  \ wrap
  \ breakindent
  \ wildmenu
  \ wildmode=list:full
  \ wildchar=<TAB>

" Indent {{{
filetype indent on
set autoindent smartindent smarttab expandtab shiftwidth=4 tabstop=4
" }}}

" status line
autocmd MyAutoCmd BufEnter * if winwidth(0) >= 60 |
            \ set statusline=[%n]\ %t\ %m%R%H%W%y\ %([%{&fenc}][%{&ff}]%)\ %([%l(%p%%),%v]%)(%B)\ |
            \ else |
            \ set statusline=[%n]%t |
            \ endif

" draw cursorline onry current window {{{
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
    autocmd CursorHold,CursorHoldI   * call s:auto_cursorline('CursorHold')
    autocmd WinEnter                 * call s:auto_cursorline('WinEnter')
    autocmd WinLeave                 * call s:auto_cursorline('WinLeave')

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

" tabline {{{
" ref:http://d.hatena.ne.jp/thinca/20111204/1322932585
function! g:MakeTabLine()
    let l:titles   = map(range(1, tabpagenr('$')),'s:tabpage_label(v:val)' )
    let l:sep      = '|'
    let l:tabpages = join(l:titles , l:sep) . l:sep . '%#TabLineFill#%T'
    let l:path     = fnamemodify(getcwd(),':~')
    return l:tabpages . '%=' .l:path
endfunction

set tabline=%!g:MakeTabLine()

function! s:tabpage_label(n)
    if v:version >= 703
        let l:title = gettabvar(a:n, 'l:title')
    else
        let l:title = ''
    endif

    if l:title !=# ''
        return l:title
    endif

    let l:bufnrs   = tabpagebuflist(a:n)
    let l:hi       = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

    let l:mod      = len(filter(copy(l:bufnrs), 'getbufvar(v:val, "&l:modified")')) ? '[+]' : ''
    let l:curbufnr = l:bufnrs[tabpagewinnr(a:n) - 1] " tabpagewinnr()は1 origin
    let l:fname    = pathshorten(bufname(l:curbufnr))

    if l:fname ==# ''
        let l:fname = ' '
    endif

    let l:label = a:n .':' .  l:fname . l:mod

    return '%' . a:n . 'T' . l:hi . l:label .  '%T%#TabLineFill#'

endfunction
" }}}
" }}}

" Backup {{{
if !isdirectory(&backupdir)
    call mkdir(&backupdir,'p')
endif
set backup
if exists('*strftime')
    augroup Bex
        autocmd!
        au BufWritePre * let &bex = '-' . strftime('%y%m%d%H%M') . '~'
    augroup END
else
    augroup Bex
        autocmd!
        au BufWritePre * let &bex = '-' . localtime('%y%m%d%H%M') . '~'
    augroup END
endif
" }}}

" search {{{
set ignorecase smartcase incsearch showmatch nowrapscan
" }}}

" other {{{
" auto read templates {{{
let s:FileExts = [
            \ 'c',
            \ 'html',
            \ 'pl',
            \ 'py',
            \ 'rb',
            \ 'tex',
            \]
for s:Extension in s:FileExts
    execute 'autocmd BufNewFile *.' . s:Extension . ' 0r $VIMFILE_DIR/skeleton/skel.' . s:Extension
endfor
" }}}

" undo each unite words {{{
function! s:is_changed() " {{{
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
function! s:changed_text() " {{{
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

" vimrc power {{{
" :Scouter
function! g:Scouter(file, ...)
    let l:pat = '^\s*$\|^\s*"'
    let l:lines = readfile(a:file)
    if !a:0 || !a:1
        let l:lines = split(substitute(join(l:lines, "\n"), '\n\s*\\', '', 'g'), "\n")
    endif
    return len(filter(l:lines,'v:val !~ l:pat'))
endfunction
command! -bar -bang -nargs=? -complete=file Scouter
            \        echo g:Scouter(empty(<q-args>) ? $VIMRC : expand(<q-args>), <bang>0)
command! -bar -bang -nargs=? -complete=file GScouter
            \        echo g:Scouter(empty(<q-args>) ? $GVIMRC : expand(<q-args>), <bang>0)
" }}}

" operator {{{
function! g:ReplaceMotion(type, ...)
    let l:sel_save = &selection
    let &selection = 'inclusive'
    let l:reg_save = @@
    let l:mark_save = getpos("'a")

    if a:0 " visual mode
        silent exe "normal! '>$"
        if getpos("'>") == getpos('.')
            silent exe 'normal! `<"_d`>"_d$"0p`<'
        else
            silent exe 'normal! `>lma`<"_d`a"0P`<'
        endif
    elseif a:type ==# 'char' " char motion
        silent exe "normal! ']$"
        if getpos("']") == getpos('.')
            silent exe 'normal! `["_d`]"_d$"0p`['
        else
            silent exe 'normal! `]lma`["_d`a"0P`['
        endif
    endif

    let &selection = l:sel_save
    let @@ = l:reg_save
    call setpos("'a", l:mark_save)
endfunction
" }}}

" Rename new_filename {{{
command! -nargs=+ -bang -complete=file Rename let pbnr=fnamemodify(bufname('%'), ':p')|exec 'f '.escape(<q-args>, ' ')|w<bang>|call delete(pbnr)
" }}}

" DiffOrig diff with current buffer {{{
" Diff [filanem|buff num]
command! -nargs=? -complete=file Diff if '<args>'=='' | browse vertical diffsplit|else| vertical diffsplit <args>|endif
" }}}

" commands which reopen with encodings {{{
" ref: http://zudolab.net/blog/?p=132
command! ChgEncCp932     edit ++enc=cp932
command! ChgEncEucjp     edit ++enc=euc-jp
command! ChgEncIso2022jp edit ++enc=iso-2022-jp
command! ChgEncUtf8      edit ++enc=utf-8
command! ChgEncSjis      edit ++enc=cp932
command! ChgEncJis       Iso2022jp
" }}}

" commands which change encodings {{{
" ref: http://zudolab.net/blog/?p=132
" change encoding commands
command! ChgFencCp932     set fenc=cp932
command! ChgFencEucjp     set fenc=euc-jp
command! ChgFencIso2022jp set fenc=iso-2202-jp
command! ChgFencUtf8      set fenc=utf-8
command! ChgFencSjis      set fenc=cp932
command! ChgFencJis       ChgencIso2022jp
" }}}

" spell check {{{
command! CheckSpell :set spell!
" }}}

" easy fold expanding {{{
" https://gist.github.com/1240267
nnoremap <expr> h
            \   col('.') == 1 && foldlevel(line('.')) > 0 ? 'zc' : 'h'

nnoremap <expr> l
            \   foldclosed(line('.')) != -1 ? 'zo' : 'l'
" }}}

" date iput Macro {{{
if exists('*strftime')
    inoremap  <Leader>date <C-R>=strftime('%Y/%m/%d (%a)')<CR>
    inoremap  <Leader>time <C-R>=strftime('%H:%M:%S')<CR>
endif
" }}}

" smart split window {{{
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

" Spell Check , Toriger F9 {{{
nnoremap <F9> :call g:SpellToggle()<CR>
function! g:SpellToggle()
    setlocal spell!
    if exists('g:syntax_on')
        syntax off
    else
        syntax on
    endif
endfunction
"}}}

command! Sudowrite :w !sudo tee %
cabbr w!! w !sudo tee > /dev/null %
" }}}

" keymap {{{
" window resize +/- {{{
nnoremap + <C-w>+
nnoremap - <C-w>-
" }}}

" kana's useful tab function {{{
function! s:move_window_into_tab_page(target_tabpagenr)
    " Move the current window into a:target_tabpagenr.
    " If a:target_tabpagenr is 0, move into new tab page.
    if a:target_tabpagenr < 0  " ignore invalid number.
        return
    endif
    let l:original_tabnr = tabpagenr()
    let l:target_bufnr   = bufnr('')
    let l:window_view    = winsaveview()
    if a:target_tabpagenr == 0
        tabnew
        tabmove  " Move new tabpage at the last.
        execute l:target_bufnr 'buffer'
        let l:target_tabpagenr = tabpagenr()
    else
        execute a:target_tabpagenr 'tabnext'
        let l:target_tabpagenr = a:target_tabpagenr
        topleft new  " FIXME: be customizable?
        execute l:target_bufnr 'buffer'
    endif
    call winrestview(l:window_view)
    execute l:original_tabnr 'tabnext'
    if 1 < winnr('$')
        close
    else
        enew
    endif
    execute l:target_tabpagenr 'tabnext'
endfunction
" }}}

" <Leader>to move current buffer into a new tab.
nnoremap <silent> <Leader>to :<C-u>call <SID>move_window_into_tab_page(0)<CR>

" tab {{{
nnoremap <Leader>tn :tabnew<CR>
for g:i in range(1,9)
    execute 'nnoremap ' . g:i . '<Leader>t ' . g:i .'gt'
endfor
" }}}

" easy date input in filename {{{
" ref:vim tech bible 4-1
cnoremap <expr> <Leader>date strftime('%Y-%m-%d')
cnoremap <expr> <Leader>time strftime('%Y-%m-%d_%H:%M')
" }}}

" easy movin window & fix size {{{
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

" gb:Jump last edit point {{{
nnoremap gb `.zz

" <C-g><M-g>:jump edit poit forward direction
nnoremap <C-b> g;

" vb: select last select region
nnoremap vb `[v`]
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
" nnoremap <Leader>bn :<C-u>bn<CR>
" nnoremap <Leader>bp :<C-u>bp<CR>
" nnoremap <Leader>bd :<C-u>bdelete<CR>
" }}}

" replace a word at cursor and yank string {{{
" ref: vim tech bible 4-6
nnoremap <silent> cy  ce<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
vnoremap <silent> cy   c<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
nnoremap <silent> ciy ciw<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
" }}}

" search {{{
" advanceable search {{{
nnoremap n nzz
nnoremap N Nzz
nnoremap * *N
nnoremap # #N
nnoremap g* g*N
nnoremap g# g#N
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

" repeatable indent handling {{{
vnoremap > >gv
vnoremap < <gv
" }}}

nnoremap <expr> s* ':%s/\<' . expand('<cword>') . '\>/'
vnoremap <expr> s* ':s/\<' . expand('<cword>') . '\>/'

nnoremap ggyG :echo "Use :%y"<CR>
vnoremap Gy :<C-u>echo "Use :,$y"<CR>
" }}}

" Plugin {{{
" if filereadable(expand('~/.vimrc.plugin'))
if filereadable(expand($VIMRC_PLUGING))
    source $VIMRC_PLUGING
endif
" }}}

" Language setting {{{
" Prefix {{{
let g:FileTypeSettings = [
            \ 'ruby',
            \ 'python',
            \ 'markdown',
            \ 'go',
            \ 'javascript',
            \ 'coffee',
            \ 'make'
            \]
for g:MyFileType in g:FileTypeSettings
    execute 'autocmd MyAutoCmd FileType ' . g:MyFileType . ' call g:My' . g:MyFileType . 'Settings()'
endfor
" }}}
" Ruby {{{
function! g:MyrubySettings()
    setlocal shiftwidth=2 tabstop=2 dictionary=$HOME/.vim/dict/ruby.dict
    let l:ruby_space_errors = 1
    if filereadable(expand('~/rtags'))
        au MyAutoCmd FileType ruby,eruby setl tags+=~/rtags,~/gtags
    endif
endfunction
" }}}
" Python {{{
function! g:MypythonSettings()
    setlocal expandtab shiftwidth=4 tabstop=4
endfunction
" }}}
" markdown {{{
function! g:MymarkdownSettings()
    if has('folding')
        setlocal foldmethod=expr
    endif
    if has ('folding') &&has('eval')
        setlocal foldexpr=g:Markdown(v:lnum)
    endif

    function! g:Markdown(lnum)
        let l:level = matchend(getline(a:lnum), '^#\+')
        return l:level > 0 ? '>' . l:level : '='
    endfunction
endfunction
" }}}
" make {{{
function! g:MymakeSettings()
    setlocal noexpandtab
endfunction
" }}}
" golang {{{
function! g:MygoSettings()
    setlocal noexpandtab fdm=indent tabstop=4 shiftwidth=4 fdl=1
endfunction
" }}}
" js,coffeescript {{{
function! g:MyjavascriptSettings()
    set shiftwidth=2 softtabstop=2 tabstop=2 expandtab
endfunction
function! g:MycoffeeSettings()
    call g:MyjavascriptSettings()
endfunction
" }}}
" }}}

syntax on
"============================================================
" vim:set tw=0 tabstop=4 shiftwidth=4 fdm=marker fdl=0:
