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
  \ nu
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
  \ wildignore+=*.class
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

" configs and directories {{{
    set backupdir=$HOME/.vimbackup
if has('nvim')
    let $VIMFILE_DIR   = $HOME.'/.config/nvim'
    let $VIMRC         = $VIMFILE_DIR.'/init.vim'
    let $GVIMRC        = $VIMFILE_DIR.'/gvim.vim'
else
    let $VIMFILE_DIR   = $HOME.'/.vim'
    let $VIMRC         = $HOME.'/.vimrc'
    let $GVIMRC        = $HOME.'/.gvimrc'
end
" }}}

" edit configs {{{
nnoremap <Leader>ev :edit $VIMRC<CR>
nnoremap <Leader>eg :edit $GVIMRC<CR>
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
" use encoding as fileencoding when file NOT contain Japanese {{{
if has('autocmd')
    function! g:AU_ReCheck_FENC()
        if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
            let &fileencoding=&encoding
        endif
    endfunction
    autocmd MyAutoCmd BufReadPost * call g:AU_ReCheck_FENC()
endif
" }}}

" auto recognition fileformats {{{
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
  \ showbreak=↪
  \ display=lastline
  \ laststatus=2
  \ statusline=%F%m%h%w\ %<[%{&fenc!=''?&fenc:&enc}]\ [%{&ff}]\ [%Y]\ %=(0x%02B)\ [%l/%L(%02v)]
  \ wrap
  \ wildmenu
  \ wildmode=list:full
  \ wildchar=<TAB>
  \ fillchars=fold:\ ,

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
set spelllang=en,cjk
command! CheckSpell :set spell!
" }}}

" easy fold expanding {{{
" https://gist.github.com/1240267
nnoremap <expr> h
            \   col('.') == 1 && foldlevel(line('.')) > 0 ? 'zc' : 'h'

nnoremap <expr> l
            \   foldclosed(line('.')) != -1 ? 'zo' : 'l'
" }}}

" date input Macro {{{
if exists('*strftime')
    inoremap  <Leader>date <C-R>=strftime('%Y/%m/%d')<CR>
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

" {{{ toggle windows maximize

let g:toggle_window_size = 0
function! ToggleWindowSize()
    if g:toggle_window_size == 1
        exec "normal \<C-w>="
        let g:toggle_window_size = 0
    else
        :resize
        :vertical resize
        let g:toggle_window_size = 1
    endif
endfunction
nnoremap <silent><C-w>m :<C-u>call ToggleWindowSize()<CR>
" }}}

" }}}

command! Sudowrite :w !sudo tee %
cabbr w!! w !sudo tee > /dev/null %
" }}}

" keymap {{{
" page down {{{
nnoremap <C-f> :echoerr "Use <C-d>"<CR>
" }}}
" tab {{{
nnoremap <Leader>tn :tabnew<CR>
" }}}

" easy date input in filename {{{
" ref:vim tech bible 4-1
cnoremap <expr> <Leader>date strftime('%Y-%m-%d')
cnoremap <expr> <Leader>time strftime('%Y-%m-%d_%H:%M')
" }}}

" easy moving window & fix size {{{
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

" Jump last edit point {{{
nnoremap gb `.zz

" <C-g><M-g>:jump edit point forward direction
nnoremap <C-b> g;

" vb: select last select region
nnoremap vb `[v`]
" }}}

" Emacs's cursor moving {{{
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
nnoremap <C-w>2 :only<CR>
nnoremap <C-w>1 :only<CR>
nnoremap <C-w>0 :only<CR>
" }}}

" search {{{
" advance search {{{
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
nnoremap <expr> <Leader>g ':vimgrep /\<' . expand('<cword>') . '\>/j **/*.' . expand('%:e')
" }}}
" }}}

" repeatable indent handling {{{
vnoremap > >gv
vnoremap < <gv
" }}}

nnoremap ggyG :echoerr "Use :%y"<CR>
vnoremap Gy :<C-u>echoerr "Use :,$y"<CR>

if has('terminal')
    tnoremap <silent> <ESC> <C-\><C-n>
endif

if executable('jq')
  function! s:jq(has_bang, ...) abort range
    execute 'silent' a:firstline ',' a:lastline '!jq' string(a:0 == 0 ? '.' : a:1)
    if !v:shell_error || a:has_bang
      return
    endif
    let error_lines = filter(getline('1', '$'), 'v:val =~# "^parse error: "')
    " 範囲指定している場合のために，行番号を置き換える
    let error_lines = map(error_lines, 'substitute(v:val, "line \\zs\\(\\d\\+\\)\\ze,", "\\=(submatch(1) + a:firstline - 1)", "")')
    let winheight = len(error_lines) > 10 ? 10 : len(error_lines)
    " カレントバッファがエラーメッセージになっているので，元に戻す
    undo
    " カレントバッファの下に新たにウィンドウを作り，エラーメッセージを表示するバッファを作成する
    execute 'botright' winheight 'new'
    setlocal nobuflisted bufhidden=unload buftype=nofile
    call setline(1, error_lines)
    " エラーメッセージ用バッファのundo履歴を削除(エラーメッセージをundoで消去しないため)
    let save_undolevels = &l:undolevels
    setlocal undolevels=-1
    execute "normal! a \<BS>\<Esc>"
    setlocal nomodified
    let &l:undolevels = save_undolevels
    " エラーメッセージ用バッファは読み取り専用にしておく
    setlocal readonly
  endfunction
  command! -bar -bang -range=% -nargs=? Jq  <line1>,<line2>call s:jq(<bang>0, <f-args>)
endif
" }}}

" Plugin {{{
" vim-plug.vim{{{

if has('nvim')
    let s:plug_dir = $HOME.'/.local/share/nvim/plugged'
else
    let s:plug_dir = $HOME.'/.cache/vim-plugged'
end
let g:vim_plug_repo_dir = s:plug_dir.'/vim-plug'

if has('vim_starting')
    if !isdirectory(expand(g:vim_plug_repo_dir))
        echo 'install vim-plug.vim...'
        call system('git clone git://github.com/junegunn/vim-plug '.g:vim_plug_repo_dir.'/autoload')
    endif

    exe 'set rtp+='.g:vim_plug_repo_dir
endif

call plug#begin(s:plug_dir)
Plug 'junegunn/vim-plug',
            \ {'dir': g:vim_plug_repo_dir.'/autoload'}

Plug 'Shougo/vimproc.vim'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'

Plug 'tomasr/molokai'

Plug 'osyo-manga/vim-precious'
Plug 'Shougo/context_filetype.vim'

Plug 'airblade/vim-rooter'

Plug 'scrooloose/nerdtree'

Plug 'jistr/vim-nerdtree-tabs'

if version >= 800
    Plug 'mattn/sonictemplate-vim'
endif

Plug 'majutsushi/tagbar'
Plug 'h1mesuke/vim-alignta'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-fugitive'
Plug 'vim-scripts/The-NERD-Commenter'
Plug 'kana/vim-smartinput'

if !has('kaoriya')
    Plug 'vim-scripts/scratch-utility'
endif

if has('nvim') || version >= 800
    Plug 'vim-jp/vimdoc-ja'
endif

if has('nvim') || version >= 800
    Plug 'prabirshrestha/async.vim'
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
    Plug 'prabirshrestha/vim-lsp'

    Plug 'mattn/vim-lsp-settings'
    Plug 'mattn/vim-lsp-icons'

    Plug 'hrsh7th/vim-vsnip'
    Plug 'hrsh7th/vim-vsnip-integ'
endif

Plug 'mattn/emmet-vim', {'for': ['html', 'erb', 'eruby']}
Plug 'udalov/kotlin-vim', {'for': 'kotlin'}
Plug 'fatih/vim-go', {'for': 'go'}
Plug 'jelera/vim-javascript-syntax', {'for': 'javascript'}
Plug 'kchmck/vim-coffee-script', {'for': 'coffee'}
Plug 'slim-template/vim-slim', {'for': 'slim'}
Plug 'fishbullet/deoplete-ruby', {'for': 'ruby'}
Plug 'jmcantrell/vim-virtualenv', {'for': 'python'}
Plug 'Vimjas/vim-python-pep8-indent', {'for': 'python'}
Plug 'aklt/plantuml-syntax', {'for': 'puml'}
Plug 'martinda/Jenkinsfile-vim-syntax', {'for': 'Jenkinsfile'}
Plug 'dag/vim-fish', {'for': 'fish'}
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'mrk21/yaml-vim', {'for': 'yaml'}
Plug 'pearofducks/ansible-vim', {'for': 'yaml'}
call plug#end()

let s:plugin_config = glob($VIMFILE_DIR."/_config/*")
let s:splitted = split(s:plugin_config, "\n")
for s:file in s:splitted
    exec "source ".s:file
endfor

colorscheme molokai

let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {}
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['md'] = "\uf48a"

let g:NERDTreeIgnore = [
            \ '^__pycache__$',
            \ '.pyc$',
            \]

let g:sonictemplate_vim_template_dir = [
            \ '~/.vim/templates'
            \]

let g:tagbar_autoshowtag = 1
nnoremap <silent><F11>  :Tagbar<CR>
let g:tagbar_type_ruby = {
    \ 'kinds' : [
        \ 'm:modules',
        \ 'c:classes',
        \ 'd:describes',
        \ 'C:contexts',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
\ }

let g:table_mode_corner_corner='+'
let g:table_mode_header_fillchar='='

let g:user_emmet_leader_key='<C-e>'

let g:rustfmt_autosave = 1
" }}}
" }}}

syntax on
"============================================================
" vim:tw=0 tabstop=4 shiftwidth=4 fdm=marker
