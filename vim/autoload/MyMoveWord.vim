""""""""""""""""""""""""""""""
"*�ړ��R�}���hw,b,W,E,B�̃J�X�^�}�C�Y
" �@w,b,W,B,E �̃J�[�\���ړ����s���ň�U��~�����܂��B
" W,B,E �ɂ��Ă͎w��Z�p���[�^�ł���U��~�����܂��B
" �uw,b�̍s���Œ�~�v�͑���Windows�G�f�B�^����ڍs�̍ۂɁA��a�����Ȃ������߂�
" ���̂ŋ@�\�I�ɓ��ʂ̈Ӗ��͂���܂���Bvim�Ɋ���Ă����vim�ɂ����Ă̓f�t�H��
" �g�̓���̕��������I�Ȃ̂Ŗ����ɂ��������悢�ł��傤�B
"
" �@�܂��W����W,B,E�͓��{�ꕶ���ł͋󔒂��Ȃ����߁A�قƂ�ǈӖ�������܂���B
" ������������邽�߂�W,B,E�̃Z�p���[�^�Ƃ��� '�B�A' �Ȃǂ��w�肵�āA�������{
" ��̓���s���J�[�\���ړ����y�ɂ��鎖��ړI�Ƃ��Ă��܂��B
" ��s�̒������{��ł͎w�肵�����{��Z�p���[�^�Ŏ~�܂邱�Ƃ��킩��Ǝv���܂��B
" �f�t�H���g�ł�
"
" let g:MyMoveWord_JpSep = '�@�B�A�D�C�^�I�H�u�v'
"
" ���ݒ肳��Ă��܂��B
"
" �@w,b�͈�a���̂���l�݂̂��Ώۂł��邱�Ƃ���AW,B,E�ɂ��Ă͎�������������
" ��Ȃ��Ƃ���A���̃X�N���v�g�̓f�t�H���g�Ŗ����ɂ���Ă��܂��B���ꂼ��.vimrc
" �ɉ��L��ݒ肷�邱�Ƃɂ��L���ɂȂ�܂��B
"
" let MyMoveWord_enable_wb  = 1
" let MyMoveWord_enable_WBE = 1
"
" �I�v�V����
"
" MyMoveWord_stop_eol = 0  : �s����eol�Œ�~���Ȃ��B
" MyMoveWord_stop_eol = 1  : �s����eol�ŕK����~�B
" MyMoveWord_stop_eol = 2  : �J�[�\����eol���O�̕����Ȃ�eol�Ŕ��~�B
"
" 1��2�� virtualedit=onemore ���w�肳��Ă��Ȃ���ΈႢ�͂���܂���B
" �f�t�H���g�ł� virtualedit+=onemore ���ݒ肳��܂��B
"
" MyMoveWord_onemore       : 0�Ȃ�virtualedit��'onemore'��ݒ肵�Ȃ��B
" MyMoveWord_enable_wb     : ��0��w,b��L���ɂ���B
" MyMoveWord_enable_WBE    : ��0��W,B,E��L���ɂ���B
"
" ��L�l��vim�N�����̂݉e�����܂��B
"
" MyMoveWord_JpSep         : W,B,E�Ŏ~�܂���{��Z�p���[�^���w��B
"
" ���ӎ���
"
"     * W,B,E�ɂ��Ă�motion������������@���킩��Ȃ��̂ŁA�}�b�v���g�����
"     �Ƀe�L�g�[�ȕ��@�Ŏ������Ă��܂��B��̓I�ɂ�dW�Ȃǂ̓��[�V�����ł͂Ȃ�dW
"
"     �Ƃ����R�}���h���̂��`���Ă��܂��B
"       ���Rd2W�̂悤�Ȍ`���͎g���Ȃ��̂ŁAnmap d2W 2dW  �̗l�ȃ}�b�v���`��
"       �Ă��܂��B�g����̂�d9W�܂łŁAc,d,y�ɂ��Ă��ꂼ���`����Ă��܂��B
"     * visual���[�h�ł�vWWWW�̂悤�ɂ͎g���܂����Av2W�̂悤�Ȍ`���͈����܂���B
"     * W,B,E�͎��I�Ȏg�p�ł͓��{��������Ȃ�L�p�ŁA���ɗL�Q�ȕ���p��������
"     ���Ɍ����܂����A�g�p�ɂ͒��ӂ��Ă��������B
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

"WBE���ꎞ��~�����镶��
if !exists('MyMoveWord_JpSep')
  let g:MyMoveWord_JpSep = ' �@�B�A�D�C�^�I�H�u�v'
endif

"wb,WBE���s���Ŏ~�߂�
if !exists('MyMoveWord_stop_eol')
  let g:MyMoveWord_stop_eol = 0
endif

"virtualedit=onemore ��ݒ肷��B
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

"  W,B,E�̓J�E���g�ɑΉ��ł��Ȃ��̂ŁAd2W��2dW�Ƃ��Ď��s�B
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
  "�ɒ[�ɑ傫���J�E���g����͂��ꂽ�ꍇ�`�F�b�N
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
    "eol���܂������ꍇ
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

"�ŏ��ɁA�I���W�i���̃R�}���h�ňړ����Ă݂āA
"eol�̐����J�E���g��������ďI�[�ɒB���邩�`�F�b�N���Ȃ���΂Ȃ�Ȃ��B
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

