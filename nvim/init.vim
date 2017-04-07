" {{{ General
" {{{
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
if has('nvim')
    let $VIMFILE_DIR   = $HOME.'/.config/nvim'
    let $VIMRC         = $VIMFILE_DIR.'/init.vim'
    let $GVIMRC        = $VIMFILE_DIR.'/gvim.vim'
    let $VIMRC_PLUGIN  = $VIMFILE_DIR.'/plugin.vim'
else
    let $VIMFILE_DIR   = $HOME.'/.vim'
    let $VIMRC         = $HOME.'/.vimrc'
    let $GVIMRC        = $HOME.'/.gvimrc'
    let $VIMRC_PLUGIN  = $HOME.'/.vimrc.plugin'
end
" }}}

" edit configs {{{
nnoremap <Leader>ev :edit $VIMRC<CR>
nnoremap <Leader>eg :edit $GVIMRC<CR>
nnoremap <Leader>ep :edit $VIMRC_PLUGIN<CR>
" }}}
" }}}

" Auto delete line-end Space {{{
augroup Autoplace
    autocmd!
    autocmd BufWritePre * if &filetype != "markdown" | :%s/\s\+$//ge
augroup END
" }}}

set tags+=tags;
" }}}

" file encoding {{{
set encoding=utf-8
scriptencoding utf-8
" set fenc=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp
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
  \ wildmenu
  \ wildmode=list:full
  \ wildchar=<TAB>

if has('nvim')
    set inccommand=split
endif

" Indent {{{
filetype indent on
set autoindent smartindent smarttab expandtab shiftwidth=4 tabstop=4
" }}}

set cursorline

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
        au BufWritePre * let &bex = '-' . localtime() . '~'
    augroup END
endif
" }}}

" search {{{
set ignorecase smartcase incsearch showmatch nowrapscan
" }}}

" other {{{
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
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
" }}}

" Emacs's window handling {{{
nnoremap <silent> <C-x>1 :<C-u>only<CR>
nnoremap <silent> <C-x>2 :<C-u>sp<CR>
nnoremap <silent> <C-x>3 :<C-u>vsp<CR>
nnoremap <silent> <C-w>1 :<C-u>only<CR>
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

tnoremap <silent> <ESC> <C-\><C-n>
" }}}

" Plugin {{{
" dein.vim{{{
filetype plugin indent off
let s:dein_dir = $VIMFILE_DIR.'/dein'
let s:dein_repo_dir = s:dein_dir.'/repos/github.com/Shougo/dein.vim/'
let s:dein_toml = $VIMFILE_DIR.'/dein.toml'
if has('vim_starting')
    if !isdirectory(expand(s:dein_repo_dir))
        echo 'install dein.vim...'
        call system('git clone git://github.com/Shougo/dein.vim '.s:dein_repo_dir)
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
autocmd MyAutoCmd VimEnter * call dein#call_hook('post_source')

filetype plugin indent on
syntax on

" install plugins
if dein#check_install()
    call dein#install()
endif

call map(dein#check_clean(), "delete(v:val, 'rf')")
" }}}
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
    setlocal shiftwidth=2 tabstop=2 dictionary=$VIMFILE_DIR/dict/ruby.dict
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
" vim:tw=0 tabstop=4 shiftwidth=4 fdm=marker fdl=0
