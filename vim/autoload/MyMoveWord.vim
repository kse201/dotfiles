""""""""""""""""""""""""""""""
"*移動コマンドw,b,W,E,Bのカスタマイズ
" 　w,b,W,B,E のカーソル移動を行末で一旦停止させます。
" W,B,E については指定セパレータでも一旦停止させます。
" 「w,bの行末で停止」は他のWindowsエディタから移行の際に、違和感をなくすための
" もので機能的に特別の意味はありません。vimに慣れてくるとvimにおいてはデフォル
" トの動作の方が合理的なので無効にした方がよいでしょう。
"
" 　また標準のW,B,Eは日本語文書では空白がないため、ほとんど意味がありません。
" これを解消するためにW,B,Eのセパレータとして '。、' などを指定して、長い日本
" 語の同一行内カーソル移動を楽にする事を目的としています。
" 一行の長い日本語では指定した日本語セパレータで止まることがわかると思います。
" デフォルトでは
"
" let g:MyMoveWord_JpSep = '　。、．，／！？「」'
"
" が設定されています。
"
" 　w,bは違和感のある人のみが対象であることから、W,B,Eについては実装がいいかげ
" んなことから、このスクリプトはデフォルトで無効にされています。それぞれ.vimrc
" に下記を設定することにより有効になります。
"
" let MyMoveWord_enable_wb  = 1
" let MyMoveWord_enable_WBE = 1
"
" オプション
"
" MyMoveWord_stop_eol = 0  : 行末のeolで停止しない。
" MyMoveWord_stop_eol = 1  : 行末のeolで必ず停止。
" MyMoveWord_stop_eol = 2  : カーソルがeol直前の文字ならeolで非停止。
"
" 1と2は virtualedit=onemore が指定されていなければ違いはありません。
" デフォルトでは virtualedit+=onemore が設定されます。
"
" MyMoveWord_onemore       : 0ならvirtualeditに'onemore'を設定しない。
" MyMoveWord_enable_wb     : 非0でw,bを有効にする。
" MyMoveWord_enable_WBE    : 非0でW,B,Eを有効にする。
"
" 上記四つはvim起動時のみ影響します。
"
" MyMoveWord_JpSep         : W,B,Eで止まる日本語セパレータを指定。
"
" 注意事項
"
"     * W,B,Eについてはmotionを実装する方法がわからないので、マップを使う非常
"     にテキトーな方法で実装しています。具体的にはdWなどはモーションではなくdW
"
"     というコマンド自体を定義しています。
"       当然d2Wのような形式は使えないので、nmap d2W 2dW  の様なマップを定義し
"       ています。使えるのはd9Wまでで、c,d,yについてそれぞれ定義されています。
"     * visualモードではvWWWWのようには使えますが、v2Wのような形式は扱えません。
"     * W,B,Eは私的な使用では日本語を扱うなら有用で、特に有害な副作用も無いよ
"     うに見えますが、使用には注意してください。
""""""""""""""""""""""""""""""
scriptencoding cp932

if !exists('enable_MyMoveWord')
  let enable_MyMoveWord = 1
endif
if enable_MyMoveWord == 0 || exists('loaded_MyMoveWord')
  if !exists('g:fudist') || g:fudist == 0
    finish
  endif
endif
let loaded_MyMoveWord = 1

if !exists('MyMoveWord_enable_wb')
  let MyMoveWord_enable_wb = 0
endif
if !exists('MyMoveWord_enable_WBE')
  let MyMoveWord_enable_WBE = 0
endif

"WBEを一時停止させる文字
if !exists('MyMoveWord_JpSep')
  let g:MyMoveWord_JpSep = ' 　。、．，／！？「」'
endif

"wb,WBEを行末で止める
if !exists('MyMoveWord_stop_eol')
  let g:MyMoveWord_stop_eol = 0
endif

"virtualedit=onemore を設定する。
if !exists('MyMoveWord_onemore')
  if g:MyMoveWord_stop_eol
    let g:MyMoveWord_onemore = 1
  else
    let g:MyMoveWord_onemore = 0
  endif
endif
if g:MyMoveWord_onemore != 0
  set virtualedit+=onemore
endif

if g:MyMoveWord_stop_eol > 0 && g:MyMoveWord_enable_wb
  nmap <silent> w :Move w<CR>
  nmap <silent> b :Move b<CR>
endif
if g:MyMoveWord_JpSep != '' && g:MyMoveWord_enable_WBE
  nmap <silent> W :Move W<CR>
  nmap <silent> B :Move B<CR>
  nmap <silent> E :Move E<CR>
  vmap <silent> W v:Move vW<CR>
  vmap <silent> B v:Move vB<CR>
  vmap <silent> E v:Move vE<CR>
  omap <silent> W :Move W<CR>
  omap <silent> B :Move B<CR>
  omap <silent> E :Move Eo<CR>

"  W,B,Eはカウントに対応できないので、d2Wを2dWとして実行。
  nmap <silent> cW :Move cW<CR>
  nmap <silent> cB :Move cB<CR>
  nmap <silent> cE :Move cE<CR>
  nmap <silent> dW :Move dE<CR>
  nmap <silent> dB :Move dB<CR>
  nmap <silent> dE :Move dE<CR>
  nmap <silent> yW :Move yW<CR>
  nmap <silent> yB :Move yB<CR>
  nmap <silent> yE :Move yE<CR>
  let s:cc = 1
  while s:cc <= 9
    exec 'nmap c'.s:cc.'W' s:cc.'cW'
    exec 'nmap c'.s:cc.'B' s:cc.'cB'
    exec 'nmap c'.s:cc.'E' s:cc.'cE'
    exec 'nmap d'.s:cc.'W' s:cc.'dE'
    exec 'nmap d'.s:cc.'B' s:cc.'dB'
    exec 'nmap d'.s:cc.'E' s:cc.'dE'
    exec 'nmap y'.s:cc.'W' s:cc.'yW'
    exec 'nmap y'.s:cc.'B' s:cc.'yB'
    exec 'nmap y'.s:cc.'E' s:cc.'yE'
    let s:cc = s:cc+1
  endwhile
endif

command! -count -nargs=1 Move call Mvw(count, '<args>')
function! Mvw(count, cmd)
  let l:cmd=a:cmd
  let l:cmd = substitute(l:cmd, '[cdvy]\([WEB]\)', '\1', '')
  let l:loop = 1
  if a:count > 0
    let l:loop = a:count
  endif
  let saved_lz = &lazyredraw
  set lazyredraw
  if a:cmd =~ '\Cv[WEB]'
    exec "normal! gvom'ov"
  endif
  if a:cmd == 'vW' || a:cmd == 'vE' || a:cmd == 'vB'
    exec "normal! gvom'ov"
  endif
  let l:save_cursor = getpos('.')
  "極端に大きいカウントを入力された場合チェック
  if l:loop > 1000
    if s:CheckOverRun(l:loop, l:cmd, l:save_cursor) == 1
      return ''
    endif
  endif
  let stop_eol = g:MyMoveWord_stop_eol
  if match(&virtualedit, 'onemore') == -1
    if stop_eol == 1
      let stop_eol = 2
    endif
  endif
  let saved_ve=&virtualedit
  setlocal virtualedit+=onemore
  call setpos('.', l:save_cursor)
  let cursormode = mode()
  for l:a in range(l:loop)
    if stop_eol > 1
      if l:cmd=='w' && mode() != 'i'
        if col('$') == col('.')+strlen(matchstr(getline('.'), '.\{1}$'))
          call cursor(line('.'), col('$'))
        endif
      endif
    endif
    let l:isEol = 0
    if col('$') == col('.')
      let l:isEol = 1
    endif
    let l:prevline = line('.')
    let l:prevcol = col('.')
    if l:cmd =~ '\C[WE]' && g:MyMoveWord_JpSep != ''
      let jp = search('['.g:MyMoveWord_JpSep.']', 'W', line('.'))
      if jp == 0
        exec 'normal! '.l:cmd
      else
        if a:cmd =~ '\C^W'
          exec 'normal! l'
        elseif l:cmd =~ '\CE'
        endif
      endif
    elseif l:cmd =~ '\CB' && g:MyMoveWord_JpSep != ''
      let jp = search('['.g:MyMoveWord_JpSep.']', 'bW', line('.'))
      if jp == 0
        exec 'normal! '.l:cmd
      endif
    else
      exec 'normal! '.l:cmd
    endif
    if stop_eol == 0 || line('.') == l:prevline
      continue
    endif
    "eolをまたいだ場合
    if l:cmd == 'w' || l:cmd == 'W'
      if l:isEol == 0
        call cursor(l:prevline, 0)
        call cursor(line('.'), col('$'))
      endif
      if line('.')-prevline > 1
        call cursor(prevline+1, 0)
        call cursor(line('.'), col('$'))
      endif
    elseif l:cmd == 'b' || l:cmd == 'B'
      call cursor(line('.'), col('$'))
      if prevline - line('.') > 1
        call cursor(prevline-1, 0)
        call cursor(line('.'), col('$'))
      endif
    endif
  endfor
  if a:cmd =~ '[dcy][WBE]'
    exec 'normal! hv'
    call setpos('.', l:save_cursor)
    if a:cmd =~ 'y[WBE]'
      exec 'normal! y'
    elseif a:cmd =~ '[cd][WEB]'
      exec 'normal! d'
    endif
    if a:cmd =~ 'c[WBE]'
      startinsert
    endif
  endif
  if a:cmd =~ 'v[WEB]'
    exec "normal! v`'o"
  endif
  exec 'set '.(saved_lz ? "" : "no").'lazyredraw'
  exec 'setlocal virtualedit='.l:saved_ve
  return ''
endfunction

"最初に、オリジナルのコマンドで移動してみて、
"eolの数をカウントから引いて終端に達するかチェックしなければならない。
function! s:CheckOverRun(count, cmd, save_cursor)
  if a:cmd == 'w' || a:cmd == 'W' || a:cmd == 'e' || a:cmd == 'E'
    let nloop = a:count
    silent exec 'silent normal! '.nloop.a:cmd
    let nloop = a:count - (line('.') - a:firstline)
    call setpos('.', a:save_cursor)
    if nloop > 0
      silent exec 'silent normal! '.nloop.a:cmd
    endif
    if line('.') == line('$') && col('.') == col('$')
      return 1
    endif
  elseif a:cmd == 'b' || a:cmd == 'B'
    let nloop = a:count
    silent exec 'silent normal! '.nloop.a:cmd
    let nloop = a:count - (a:firstline - line('.'))
    call setpos('.', a:save_cursor)
    if nloop > 0
      silent exec 'silent normal! '.nloop.a:cmd
    endif
    if line('.') == 1 && col('.') == 1
      return 1
    endif
  endif
  return 0
endfunction

