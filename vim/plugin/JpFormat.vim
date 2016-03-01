"=============================================================================
"    Description: ���{�ꕶ�����`�v���O�C��
"     Maintainer: fuenor@gmail.com
"                 http://sites.google.com/site/fudist/Home/jpformat
"  Last Modified: 2010-09-01 21:32
"        Version: 1.15
"=============================================================================
scriptencoding cp932

if exists("loaded_JpFormat") && !exists('fudist')
  finish
endif
if exists('disable_JpFormat') && disable_JpFormat
  finish
endif
let loaded_JpFormat = 1
if &cp
  finish
endif

"�������w��𔼊p�P�ʂɂ���
"1:���p
"2:�S�p
if !exists('JpFormatCountMode')
  let JpFormatCountMode = 2
endif
"�܂�Ԃ�(���e�p���c)������
if !exists('JpCountChars')
  let JpCountChars = 40
endif
"���p�ꕶ�����I�[�o�[���Ă��܂�Ԃ�����������/���Ȃ�
if !exists('JpFormatHankakuOver')
  let JpFormatHankakuOver = 1
endif
"�܂�Ԃ�������(b:JpCountChars)�̒l��textwidth����ݒ肷��B
"�L���ȏꍇ g:JpCountChars�͖��������
if !exists('JpCountChars_Use_textwidth')
  let JpCountChars_Use_textwidth = 0
endif
"���e�p���s��
if !exists('JpCountLines')
  let JpCountLines = 17
endif
"�֑������̍ő�Ԃ牺���莚��
if !exists('JpCountOverChars')
  let JpCountOverChars = 1
endif
"���e�p�����Z�v�Z���ɍ폜���郋�r���̐��K�\��
if !exists('JpCountDeleteReg')
  let JpCountDeleteReg = '\[[^]]\+\]\|<[^>]\+>\|�s[^�t]\+�t\|�m[^�n]\+�n\|�b'
endif

"�A���}�[�J�[
if !exists('JpFormatMarker')
  let JpFormatMarker = "\t"
endif
"���`�R�}���h���g�p�����玩�����`��ON
if !exists('JpAutoFormat')
  let JpAutoFormat = 1
endif
"�J�[�\���ړ����Ƃɐ��`
if !exists('JpFormatCursorMovedI')
  let JpFormatCursorMovedI = 0
endif
"�J�[�\���ړ����Ƃɐ��`���鎞�̍s������<BS>�̋���
"1�Ȃ當���폜�A0�Ȃ�ړ��̂݁B
"set backspace��<BS>���C���f���g����s���폜�ł���悤��
"�ݒ肵�Ă���ꍇ�̂݊֌W����B
if !exists('JpFormatCursorMovedI_BS')
  let JpFormatCursorMovedI_BS = 1
endif
"�}�����[�h�ֈڍs�����玩���A��
" 1 : �J�[�\���ʒu�ȍ~�������A��
" 2 : �p���O���t�������A��
if !exists('JpAutoJoin')
  let JpAutoJoin = 1
endif

"���`��gq�R�}���h���Ăяo��
if !exists('JpFormatGqMode')
  let JpFormatGqMode = 0
endif

"��{�I�ȏ������@
"1. �܂��w�蕶�����ɍs�𕪊�
"2. ���s�̍s���֑����������ݍs�ֈړ�
"3. ���ݍs�̍s���֑����������s�ֈړ�
"4. �Ԃ牺���蕶�����𒴂��ĂԂ牺�����Ă�����ǂ��o��
"
"�s���֑�
if !exists('JpKinsoku')
  let JpKinsoku = '[-!?}>�|��`�I�H�J�K�T�U�j�n�����߁R�S�[�@�B�D�F�H�b�����������������������������X�]�E:;.���������񁎁A�B�C�D,)\]�p�l�r�t�v�x�z���f�h��\�c�d]'
endif
"�s���֑�
if !exists('JpKinsokuE')
  let JpKinsokuE = '[_0-9a-zA-Z([{<�i�o�k�q�s�u�w�y���e�g��]'
endif
"��_�ƕ�����
if !exists('JpKutenParen')
  let JpKutenParen = '[�A�B�C�D,)\]�p�l�r�t�v�x�z���f�h��]'
endif
"��_�ƕ����ʁ{�����s�����ǂ��o���p
"�����s������ǂ��o����JpNoDivN����������A��������ǂ��o���B
"�ł����H�\�\<����> ���������� �H�͎c���ā\�\�݂̂�ǂ��o�����߂̎w��B
if !exists('JpNoDivN')
  let JpNoDivN = '[�A�B�C�D,)\]�p�l�r�t�v�x�z���f�h��!?�I�H]'
endif
"�����s��
if !exists('JpNoDiv')
  let JpNoDiv = '[�\�c�d]'
endif

"�ǂ��o���p(���݂͖��g�p)
"�Ԃ牺���蕶�����𒴂��Ă��鎞�AJpKinsokuO�ȊO��1�����𑫂��Ēǂ��o���B
if !exists('JpKinsokuO')
  let JpKinsokuO = '[-!?}>�|��`�I�H�J�K�T�U�j�n�����߁R�S�[�@�B�D�F�H�b�����������������������������X�]�E:;.���������񁎁A�B�C�D,)\]�p�l�r�t�v�x�z���f�h��\�c�d]'
endif
"���`�ΏۊO�s�̐��K�\��
if !exists('JpFormatExclude')
  let JpFormatExclude = ''
endif
"���`�ΏۊO�s�̐��K�\��()
if !exists('JpFormatExclude_div')
  let JpFormatExclude_div = '^$'
endif

"�A���}�[�J�[��g�p����EOL�L�����N�^�[
if !exists('JpJoinEOL')
  let JpJoinEOL = '[�B�v�I�H�n]'
endif
"�A���}�[�J�[��g�p����TOL�L�����N�^�[
if !exists('JpJoinTOL')
  let JpJoinTOL = '[\s�@�u�E��]'
endif

"J�R�}���h���
if (!exists('JpAltJ') || JpAltJ) && JpFormatMarker != ''
  noremap <silent> J :JpAltJ<CR>
endif
"C-v���
if (!exists('JpAltCv') || JpAltCv)
  nnoremap <silent> <expr> <C-v> JpAltCv()
endif
"gq���s����JpFormat���I�t�ɂ���
"if (exists('JpAltgq') && JpAltgq) || (!exists('JpAltgq') && has('Kaoriya') && !exists('plugin_format_disable'))
if (exists('JpAltgq') && JpAltgq)
  nnoremap <silent> <expr> gq JpFormat_cmd("gq")
endif
"DEL�R�}���h���
if (exists('JpAltDEL') && JpAltDEL== 0) && JpFormatMarker != '' && JpFormatCursorMovedI
  inoremap <silent> <DEL> <C-r>=JpAltDEL()<CR>
endif

"<C-v>�R�}���h���
command! -range JpAltCv call JpAltCv(<line1>, <line2>)
function! JpAltCv(...)
  let b:jpformat = b:jpformat == 0 ? 0 : -1
  let cmd = "\<C-v>"
  return cmd
endfunction

"�R�}���h���s����JpFormat���I�t�ɂ����փR�}���h
function! JpFormat_cmd(cmd)
  let b:jpformat = 0
  return a:cmd
endfunction

"DEL�R�}���h���
function! JpAltDEL(...)
  if b:jpformat <= 0 || g:JpFormatMarker == '' || g:JpFormatCursorMovedI == 0
    return "\<Del>"
  endif
  let str = getline('.')
  let start = col('.')-1
  if strpart(str, start) != g:JpFormatMarker
    return "\<Del>"
  endif
  let l = line('.')+1
  let str = getline(l)
  let s = len(matchstr(str, '^.\{1}'))
  let str = strpart(str, s)
  call setline(l, str)
  return ''
endfunction

"���e�p�����Z
command! -bang -range=% -nargs=* JpCountPages call JpCountPages(<line1>, <line2>, <bang>0, <f-args>)
"�w��͈͂Ǝw��͈͍ŏI�s���܂ރp���O���t�𐮌`
"�͈͖��w�莞�͌��ݍs���琮�`
command! -bang -range -nargs=* JpFormat call JpFormat(<line1>, <line2>, <bang>0, <f-args>)
"�p���O���t���t�H�[�}�b�g
command! -bang -range JpFormatP call JpFormatP(<line1>, <line2>, <bang>0)
"�S�����`
command! -bang -range=% -nargs=* JpFormatAll call JpFormatAll(<line1>, <line2>, <bang>0, <f-args>)
"�w��͈͂̃p���O���t��A��
command! -bang -range JpJoin call JpJoin(<line1>, <line2>, <bang>0)
"�S���A��
command! -bang -range=% JpJoinAll call JpJoinAll(<line1>, <line2>, <bang>0)
"���ݍs��A�����Ă��烄���N
command! -bang -range JpYank call JpYank(<line1>, <line2>, <bang>0)
"�������`���g�O��
command! -count JpFormatToggle call JpFormatToggle()
"gq���[�h���g�O��
command! JpFormatGqToggle call JpFormatGqToggle()

function! JpFormatToggle()
  if count > 0
    let b:JpCountChars = count
    echo 'JpFormat : Chars = '.b:JpCountChars
    return
  endif
  let b:jpformat = !b:jpformat
  echo 'JpFormat : '. (b:jpformat ? 'ON' : 'OFF')
endfunction
function! JpFormatGqToggle()
  let b:JpFormatGqMode = !b:JpFormatGqMode
  echo 'JpFormat : '. (b:JpFormatGqMode ? '(gq)' : '(normal)')
endfunction


augroup JpFormat_
  au!
  "�}�����[�h���甲����Ǝ������`
  au InsertEnter  * call JpFormatEnter()
  au InsertLeave  * call JpFormatLeave()
  au VimEnter     * call JpFormatInit()
  au BufNew,BufNewFile,BufWinEnter * call JpFormatInit()
  au CursorMovedI * call JpFormatCursorMovedI_()
augroup END

if JpKinsoku == ''
  let JpKinsoku = '[]'
endif
if JpKinsokuE == ''
  let JpKinsokuE = '[]'
endif
if JpFormatExclude == ''
  let JpFormatExclude = '^$'
endif
if JpJoinEOL == ''
  let JpJoinEOL = '[]'
endif
if JpJoinTOL == ''
  let JpJoinTOL = '[]'
endif

let s:debug = 0
if exists('g:fudist')
  let s:debug = g:fudist
endif

if JpAutoJoin == 3
  silent! nnoremap <silent> i i<C-g>u
  silent! nnoremap <silent> I I<C-g>u
  silent! nnoremap <silent> a a<C-g>u
endif

function! JpFormatInit()
  if !exists('b:jpformat')
    let b:jpformat = 0
  endif
  if !exists('b:JpFormatExclude')
    let b:JpFormatExclude = g:JpFormatExclude
  endif
  if b:JpFormatExclude == ''
    let b:JpFormatExclude = '^$'
  endif
  if !exists('b:JpCountChars')
    let b:JpCountChars = g:JpCountChars
    if g:JpCountChars_Use_textwidth
      let b:JpCountChars = &textwidth/g:JpFormatCountMode
    endif
  endif
  if !exists('b:JpCountLines')
    let b:JpCountLines = g:JpCountLines
  endif
  if !exists('b:JpCountOverChars')
    let b:JpCountOverChars = g:JpCountOverChars
  endif
  if !exists('b:sline')
    let b:sline = line('.')
    let b:prevtime = localtime()
    let b:prevstr = []
    let b:pline = line('.')
    let b:pcol  = col('.')
    let b:pmarker = getline(b:pline-1) =~ g:JpFormatMarker."$"
    if g:JpFormatMarker == ''
      let b:pmarker = 0
    endif
"    let b:markerline = 0
  endif
  if !exists('b:JpFormatGqMode')
    let b:JpFormatGqMode = g:JpFormatGqMode
  endif
endfunction

function! JpFormatCursorMovedI_()
  if !exists('b:jpformat')
    let b:jpformat=0
  endif
  if g:JpFormatCursorMovedI == 0 || b:jpformat <= 0 || g:JpFormatMarker == ''
    return
  endif
  if exists('*JpFormatCursorMovedI')
    call JpFormatCursorMovedI()
    return
  endif
  let altch = b:pmarker && line('.') == b:pline - 1 && b:pcol == 1 && col('.') != col('$')
  if (exists('JpAltBS') && JpAltBS==0)
    let altch = 0
  endif
  if altch
    call feedkeys("\<C-h>", 'n')
    if g:JpFormatCursorMovedI_BS
      call feedkeys("\<C-h>", 'n')
    endif
  elseif b:JpFormatGqMode
    call JpFormatGq(line('.'), line('.'), 0)
  else
    call JpFormat(line('.'), line('.'), 0)
  endif
  let b:pline = line('.')
  let b:pcol  = col('.')
  let b:pmarker = getline(b:pline-1) =~ g:JpFormatMarker."$"
  if g:JpFormatMarker == ''
    let b:pmarker = 0
  endif
endfunction

silent! function JpFormatEnter()
  if !exists('b:jpformat')
    let b:jpformat = 0
  endif
  if g:JpCountChars_Use_textwidth
    let b:JpCountChars = &textwidth/g:JpFormatCountMode
  endif
  let b:saved_tw=&textwidth
  let b:saved_bs=&backspace
  if b:jpformat < 1
    return
  endif
  setlocal textwidth=0
  if g:JpFormatCursorMovedI
    setlocal backspace=indent,eol,start
  endif
  let b:sline = line('.')
  let fline = line('.')
  let lline = line('.')
  let b:pline = line('.')
  let b:pcol  = col('.')
  let b:pmarker = getline(b:pline-1) =~ g:JpFormatMarker."$"
  if g:JpFormatMarker == ''
    let b:pmarker = 0
  endif
  let lines = 1
  let l:JpAutoJoin = g:JpAutoJoin
  if g:JpFormatCursorMovedI
    let l:JpAutoJoin = 0
  endif
  if l:JpAutoJoin
    let b:prevtime = localtime()
    let b:prevstr = getline(1, line('$'))
"    let b:markerline = getline(line('.')) =~ g:JpFormatMarker.'$'
  endif
  if b:jpformat > 0
    if l:JpAutoJoin == 1 || l:JpAutoJoin == 3
      if b:JpFormatGqMode
        call JpJoinGq(fline, lline, 0)
      else
        let lines = JpJoinExec(fline, lline)
      endif
    elseif l:JpAutoJoin == 2
      if b:JpFormatGqMode
        call JpJoinGq(fline, lline, 0)
      else
        call JpJoin(fline, lline)
        let b:sline = line('.')
      endif
    endif
  endif
endfunction

silent! function JpFormatLeave()
  if !exists('b:jpformat')
    let b:jpformat = 0
  endif
  silent! exec 'setlocal textwidth='.b:saved_tw
  silent! exec 'setlocal backspace='.b:saved_bs
  if b:jpformat == 0
    return
  endif
  if b:jpformat <= -1
    let b:jpformat = 1
    return
  endif
  if b:jpformat > 0
    call JpFormatInsertLeave()
  endif
  if g:JpFormatCursorMovedI
    return
  endif
  let l:JpAutoJoin = g:JpAutoJoin
  if l:JpAutoJoin
"    if b:markerline && b:prevstr == getline(1, line('$'))
    if b:prevstr == getline(1, line('$'))
      let save_cursor = getpos(".")
      let ptime = localtime()
      let utime = localtime() - b:prevtime + 1
      silent! exec 'earlier '.utime.'s'
      for i in range(20)
        if b:prevstr == getline(1, line('$'))
          break
        endif
        silent! exec 'redo'
      endfor
      call setpos('.', save_cursor)
    endif
  endif
endfunction

"�w��͈͈ȍ~�𐮌`
function! JpFormat(fline, lline, mode, ...)
  if g:JpCountChars_Use_textwidth
    let b:JpCountChars = &textwidth/g:JpFormatCountMode
  endif
  if a:0 >= 1
    let b:JpCountChars = a:1
  endif
  if a:0 >= 2
    let b:JpCountLines = a:2
  endif
  if a:0 == 3
    let b:JpCountOverChars = a:3
  endif
  let exclude = b:JpFormatExclude
  if a:mode
    let b:JpFormatExclude = '^$'
  endif
  if b:JpFormatGqMode
    call JpFormatGq(a:fline, a:lline, 0)
  else
    call JpFormatExec(a:fline, a:lline)
  endif
  let b:JpFormatExclude = exclude
endfunction

"�S�����`
function! JpFormatAll(fline, lline, mode, ...)
  if g:JpCountChars_Use_textwidth
    let b:JpCountChars = &textwidth/g:JpFormatCountMode
  endif
  let start = reltime()
  let fline = a:fline
  let lline = a:lline
  let cline = line('.')
  let crlen = strlen(g:JpFormatMarker)
  let crlen += &ff=='dos' ? 2 : 1
  let elen = line2byte(cline) + col('.') - 1
  let saved_JpCountChars     = b:JpCountChars
  let saved_JpCountLines     = b:JpCountLines
  let saved_JpCountOverChars = b:JpCountOverChars
  if a:0 >= 1
    let b:JpCountChars = a:1
  endif
  if a:0 >= 2
    let b:JpCountLines = a:2
  endif
  if a:0 == 3
    let b:JpCountOverChars = a:3
  endif
  if b:JpFormatGqMode
"    redraw|echo 'JpFormat(gq) : Restore to its original state...'
"    call JpJoin(1, line('$'))
    redraw|echo 'JpFormat(gq) : Formatting...'
    call JpFormatGq(fline, lline, 1)
    if a:mode
"      " �}�[�J�[���폜
"      for n in range(fline, lline)
"        let str = substitute(getline(n), g:JpFormatMarker.'$', '', '')
"        call setline(n, [str])
"      endfor
    endif
    let lines = line('$')
    let pages = lines/b:JpCountLines + (lines % b:JpCountLines > 0)
    redraw| echom printf("[Easy mode] %d pages (%dx%d) : %d(%d) lines", pages, b:JpCountChars, b:JpCountLines, lines, lines % b:JpCountLines)
    echo 'JpFormat(gq) : Done. ('.reltimestr(reltime(start)).' sec )'
    return
  endif
  echo 'JpFormat : Restore to its original state...'
  let [glist, lines, delmarker] = JpJoinStr(fline, lline)
  let elen = elen - delmarker * crlen
  let marker = g:JpFormatMarker
  if a:mode
    let g:JpFormatMarker = ''
    let crlen = &ff=='dos' ? 2 : 1
  endif
  redraw| echo 'JpFormat : Formatting...'
  let clidx = elen - line2byte(fline)
  let [glist, addmarker]= JpFormatStr(glist, clidx)
  let elen = elen + addmarker * crlen
  let g:JpFormatMarker = marker

  let lines = len(glist)
  let pages = lines/b:JpCountLines + (lines % b:JpCountLines > 0)
  redraw| echom printf("[Easy mode] %d pages (%dx%d) : %d(%d) lines", pages, b:JpCountChars, b:JpCountLines, lines, lines % b:JpCountLines)
"  let b:JpCountChars     = saved_JpCountChars
"  let b:JpCountLines     = saved_JpCountLines
"  let b:JpCountOverChars = saved_JpCountOverChars
  if glist == getline(fline, lline)
    echo 'JpFormat : Not modified. ('.reltimestr(reltime(start)).' sec )'
    return 0
  endif

  call s:setline(glist, fline, lline)
  exec elen.'go'
  echo 'JpFormat : Done. ('.reltimestr(reltime(start)).' sec )'
endfunction

"�p���O���t���t�H�[�}�b�g
function! JpFormatP(fline, lline, mode)
  if b:jpformat == 0
    return
  endif
  if g:JpCountChars_Use_textwidth
    let b:JpCountChars = &textwidth/g:JpFormatCountMode
  endif
  let save_cursor = getpos(".")
  let fline = a:fline
  let lline = a:lline
  let pattern = '^$\|[^'.g:JpFormatMarker.']$'
  if g:JpFormatMarker == ''
    let pattern = '^$'
  endif
  let exclude = b:JpFormatExclude
  if a:mode
    let b:JpFormatExclude = '^$'
  endif
  call cursor(fline, 1)
  let fline = search(pattern, 'ncbW') + 1
  call cursor(lline, 1)
  let lline = search(pattern, 'ncW')
  let lline = lline == 0 ? line('$') : lline
  call setpos('.', save_cursor)
  if b:JpFormatGqMode
    call JpFormatGq(fline, lline, 1)
  else
    call JpFormatExec(fline, lline)
  endif
  let b:JpFormatExclude = exclude
endfunction

function! JpJoinAll(fline, lline, ...)
  let mode = ''
  if b:JpFormatGqMode
    let mode = '(gq)'
  endif
  redraw|echo 'JpJoin'.mode.' : Processing...'
  let start = reltime()
  call JpJoin(a:fline, a:lline)
  echo 'JpJoin'.mode.' : Done. ('. reltimestr(reltime(start)) . ' sec )'
endfunction

"�w��͈͂̃p���O���t��A��
function! JpJoin(fline, lline, ...)
  let save_cursor = getpos(".")
  let fline = a:fline
  let lline = a:lline
  let saved_marker = g:JpFormatMarker
  if a:0 && a:1 == 1
  let g:JpFormatMarker = ''
  endif
  let pattern = '^$\|[^'.g:JpFormatMarker.']$'
  if g:JpFormatMarker == ''
    let pattern = '^$'
  endif
  call cursor(fline, 1)
  let fline = search(pattern, 'ncbW') + 1
  let fline = fline == 0 ? 1 : fline
  call cursor(lline, 1)
  let lline = search(pattern, 'ncW')
  let lline = lline == 0 ? line('$') : lline
  call setpos('.', save_cursor)
  if b:JpFormatGqMode
    call JpJoinGq(fline, lline, 1)
  else
    call JpJoinExec(fline, lline)
  endif
  let g:JpFormatMarker = saved_marker
endfunction

"���ݍs�̃p���O���t�������N
function! JpYank(fline, lline, ...)
  let save_cursor = getpos(".")
  let fline = a:fline
  let lline = a:lline
  let pattern = '^$\|[^'.g:JpFormatMarker.']$'
  if g:JpFormatMarker == ''
    let pattern = '^$'
  endif
  call cursor(fline, 1)
  let fline = search(pattern, 'ncbW') + 1
  let fline = fline == 0 ? 1 : fline
  call cursor(lline, 1)
  let lline = search(pattern, 'ncW')
  let lline = lline == 0 ? line('$') : lline
  call setpos('.', save_cursor)
  exec fline.','.lline.'yank'
endfunction

function! s:setline(glist, fline, lline)
  let nlist = []
  let loop = len(a:glist)
  for i in range(loop)
    call add(nlist, " ")
  endfor
  call cursor(a:lline, col('.'))
  silent! exec 'silent! '.'put=nlist'
  call setline(a:lline+1, a:glist)
  let cmd = 'silent! '.a:fline.','. a:lline . 'delete _'
  silent! exec cmd
endfunction

"�w��͈͂𐮌`
function! JpFormatExec(fline, lline)
  let start = reltime()

  let fline = a:fline
  let lline = a:lline
  let cline = line('.')

  let crlen = strlen(g:JpFormatMarker)
  let crlen += &ff=='dos' ? 2 : 1
  let elen = line2byte(cline) + col('.') - 1

  let [glist, lines, delmarker] = JpJoinStr(fline, lline)
  let lline = fline + lines - 1
  let elen = elen - delmarker * crlen
  let clidx = elen - line2byte(fline)

  let [glist, addmarker]= JpFormatStr(glist, clidx)

  let crlen = strlen(g:JpFormatMarker)
  let crlen += &ff=='dos' ? 2 : 1
  let elen = elen + addmarker * crlen

  if glist == getline(fline, lline)
    return 0
  endif

  call s:setline(glist, fline, lline)
  exec elen.'go'
  if s:debug
"    redraw| echo reltimestr(reltime(start)) 'sec'
  endif
  return (lline - fline + 1)
endfunction

"�w��͈͂�A��
function! JpJoinExec(fline, lline)
  let start = reltime()
  let fline = a:fline
  let lline = a:lline

  let crlen = strlen(g:JpFormatMarker)
  let crlen += &ff=='dos' ? 2 : 1
  let elen = line2byte(line('.'))+col('.')-1

  let [glist, lines, delmarker] = JpJoinStr(fline, lline)
  let elen = elen - delmarker * crlen
  let lline = fline + lines - 1

  if glist == getline(fline, lline)
    return 0
  endif

  call s:setline(glist, fline, lline)
  exec elen.'go'
  if s:debug
"    redraw| echo reltimestr(reltime(start)) 'sec'
  endif
  return (lline-fline+1)
endfunction

"���X�g��A��
function! JpJoinStr(fline, lline, ...)
  let fline = a:fline
  let lline = a:lline
  let cline = line('.')
  if a:0 == 3
    let clidx = a:3
  endif
  let clidx = cline - fline
  let delmarker = 0

  let eol = g:JpJoinEOL.'$'
  let tol = '^'.g:JpJoinTOL
  let loop = lline - fline + 1
  let idx = 0
  let lines = 0
  let glist = []
  let catpattern = g:JpFormatMarker.'$'
  while loop - lines > 0
    call add(glist, getline(fline+lines))
    "�Ō�ɘA���L�����L�����狭���A��
    while glist[idx] =~ catpattern
      if g:JpFormatMarker == '' || glist[idx] =~ b:JpFormatExclude
        break
      endif
      let glist[idx] = substitute(glist[idx], catpattern, '' , '')
      let lines = lines + 1
      let glist[idx] = glist[idx] . getline(fline+lines)
      if lines <= clidx
        let delmarker += 1
      endif
      if fline+lines >= line('$')
        break
      endif
    endwhile
    while g:JpFormatMarker == ''
      if fline+lines == line('$') || glist[idx] == ''
        break
      endif
      if glist[idx] =~ b:JpFormatExclude
        break
      endif
      if glist[idx] =~ eol
        break
      endif
      let nstr = getline(fline+lines+1)
      if nstr == '' || nstr =~ tol || nstr =~ b:JpFormatExclude
        break
      endif
      let glist[idx] = glist[idx] . nstr
      let lines = lines + 1
    endwhile
    let lines = lines + 1
    let idx = idx + 1
  endwhile
  let lline = fline + lines - 1
  return [glist, lines, delmarker]
endfunction

"���X�g�𐮌`
function! JpFormatStr(str, clidx)
  let clidx = a:clidx
  let b:jpformat = g:JpAutoFormat
  let fstr = []
  let idx = 0
  let JpKinsoku  = '^'.g:JpKinsoku.'\+'
  let JpKinsokuO = g:JpKinsoku.'\+$'
  let JpKinsokuS = g:JpKinsoku.'\+$'
  let JpKinsokuE = g:JpKinsokuE.'\+$'
  let JpNoDiv = g:JpNoDiv.'\+$'
  let JpNoDivL = '^'.g:JpNoDiv.'\+'
  let JpNoDivN = g:JpNoDivN.g:JpNoDiv.'\{2}$'
  let cmode = g:JpFormatCountMode
  let hankakuover = g:JpFormatHankakuOver*(cmode-1)
  let chars  = (b:JpCountChars)*cmode-hankakuover
  let ochars = (b:JpCountOverChars)*cmode
  let catmarker = g:JpFormatMarker
  let addcr = 0
  let crlen = &ff=='dos' ? 2 : 1
  let bidx = 0
  for l in range(len(a:str))
    let lstr = a:str[l]
    if lstr == '' || lstr =~ b:JpFormatExclude
      let clidx -= strlen(a:str[l]) + crlen
      call add(fstr, lstr)
      continue
    endif
    let bidx = 0
    while 1
      let str = substitute(lstr, '\%>'.chars.'v.*','','')
      let lstr = strpart(lstr, strlen(str))
      if lstr == ''
        call add(fstr, str)
        break
      endif
      "str�̍s���֑�������S�Ď��s�ֈړ�
      if str =~ JpKinsokuE
        let ostr = matchstr(str, JpKinsokuE)
        let str = strpart(str, 0, strlen(str)-strlen(ostr))
        let lstr = ostr.lstr
        if str != ''
          let bidx += strlen(str)
          let addcr += (clidx-bidx) > 0 ? 1 : 0
          let str = str . catmarker
          call add(fstr, str)
          continue
        endif
      endif

      "lstr�̍s���֑�������S�Č��ݍs�ֈړ�
      if lstr =~ JpKinsoku
        let ostr = matchstr(str, '.\{1}$').matchstr(lstr, JpKinsoku)
        "��_�֌W���������炻���܂�
        if ostr =~ g:JpKutenParen
          let ostr = strpart(ostr, 0, matchend(ostr, g:JpKutenParen.'\+'))
        endif
        let ostr = strpart(ostr, strlen(matchstr(str, '.\{1}$')))
        let str = str.ostr
        let lstr = strpart(lstr, strlen(ostr))
      endif

      "---------- �֑������̃��C�����[�v ----------
      "�s���̕����s�����̒ǂ��o��
      if str =~ JpNoDivN
        let ostr = matchstr(str, '.\{2}$')
        let str = strpart(str, 0, strlen(str)-strlen(ostr))
        let lstr = ostr.lstr
      endif
      let slen = strlen(str)

      "�ǂ��o������
      let outstr = 1
      "�����s�����͋��E��2�����P�ʂň���
      "TODO:�����s�����̎�ނ���ʂ��Ă��Ȃ�
      if ochars > 0
        let ostr = substitute(str, '\%>'.chars.'v.*','','')
        if ostr =~ JpNoDiv
          let olen = strlen(ostr)
"          let ochar = matchstr(ostr, '.\{1}$')
          let nstr = strpart(str, olen)
"          let nchar = matchstr(nstr, '.\{1}$')
          if match(nstr, '^'.g:JpNoDiv.'\{2}') > -1
            let lstr = strpart(str, olen).lstr
            let str = strpart(str, 0, olen)
            let outstr = 0
          endif
        endif
      endif

      if outstr && ochars >= 0
        if substitute(str, '\%>'.(chars+ochars).'v.*','','') != str
          let ostr = matchstr(str, JpKinsokuO)
          let str = strpart(str, 0, strlen(str)-strlen(ostr))
          let ostr = matchstr(str, '.\{1}$').ostr
          let str = strpart(str, 0, strlen(str)-strlen(matchstr(str, '.\{1}$')))
          let lstr = ostr.lstr
          "�s���֑�������S�Ď��s�ֈړ�
          if str =~ JpKinsokuE
            let ostr = matchstr(str, JpKinsokuE)
            let str = strpart(str, 0, strlen(str)-strlen(ostr))
            let lstr = ostr.lstr
          endif
        endif
      endif
      "---------- �����܂ł��֑������̃��C�����[�v ----------
      if str == ''
        let str = substitute(lstr, '\%>'.chars.'v.*','','')
        let lstr = strpart(lstr, strlen(str))
      endif
      let bidx += strlen(str)
      if lstr != ''
        let addcr += (clidx-bidx) > 0 ? 1 : 0
        let str = str . catmarker
      endif
      call add(fstr, str)
      if strlen(lstr) == ''
        break
      endif
    endwhile
    let clidx -= strlen(a:str[l]) + crlen
  endfor
  return [fstr, addcr]
endfunction

"�}�����[�h��Ɏ������`
silent! function JpFormatInsertLeave()
  if b:jpformat == 0
    return
  endif
  if exists('*JpFormatInsert')
    call JpFormatInsert()
    return
  endif
  let fline = line('.')
  let lline = line('.')
  if fline > b:sline
    let fline = b:sline
  endif
  if lline < b:sline
    let lline = b:sline
  endif
  if b:JpFormatGqMode
    call JpFormatGq(line('.'), lline, 0)
  else
    let cline = JpFormatExec(fline, lline)
  endif
endfunction

"���e�p�����Z
function! JpCountPages(fline, lline, mode, ...)
  if g:JpCountChars_Use_textwidth
    let b:JpCountChars = &textwidth/g:JpFormatCountMode
  endif
  if a:0 == 1 && a:1 =~ 'easy'
    if JpSetAutoFormat('check')
      let lines = line('$')
      let pages = lines/b:JpCountLines + (lines % b:JpCountLines > 0)
      redraw| echom printf("[Easy mode] %d pages (%dx%d) : %d(%d) lines", pages, b:JpCountChars, b:JpCountLines, lines, lines % b:JpCountLines)
    else
      JpCountPages
    endif
    return
  endif
  let start = reltime()
  let fline = a:fline
  let lline = a:lline
  let saved_jpformat         = b:jpformat
  let saved_JpCountChars     = b:JpCountChars
  let saved_JpCountLines     = b:JpCountLines
  let saved_JpCountOverChars = b:JpCountOverChars
  if a:0 >= 1 && a:1 =~ '^[0-9]\+$'
    let b:JpCountChars = a:1
  endif
  if a:0 >= 2 && a:2 =~ '^[0-9]\+$'
    let b:JpCountLines = a:2
  endif
  if a:0 == 3 && a:3 =~ '^[0-9]\+$'
    let b:JpCountOverChars = a:3
  endif

  echo 'JpCount : Restore to its original state...'
  let [glist, lines, delmarker] = JpJoinStr(fline, lline)
  let wc0 = 0
  for str in glist
    let wc0 += strlen(substitute(str, '.', 'x', 'g'))
  endfor
  if g:JpCountDeleteReg != ''
    for l in range(len(glist))
      let glist[l] = substitute(glist[l], g:JpCountDeleteReg, '', 'g')
    endfor
  endif
  let wc = 0
  for str in glist
    let wc += strlen(substitute(str, '.', 'x', 'g'))
  endfor
  redraw|echo 'JpCount : Formatting...'
  let [glist, addmarker] = JpFormatStr(glist, 0)

  let lines = len(glist)
  let pages = lines/b:JpCountLines + (lines % b:JpCountLines > 0)
  redraw| echom printf("%d pages (%dx%d) : %d(%d) lines : %d/%d chars", pages, b:JpCountChars, b:JpCountLines, lines, lines % b:JpCountLines, wc, wc0)
  let b:JpCountChars     = saved_JpCountChars
  let b:JpCountLines     = saved_JpCountLines
  let b:JpCountOverChars = saved_JpCountOverChars
  let b:jpformat         = saved_jpformat
  if a:mode == 0 || glist == getline(fline, lline)
    if s:debug
      echo 'JpCount : Done. ('. reltimestr(reltime(start)) . ' sec )'
    endif
    return
  endif

  call s:setline(glist, fline, lline)
  call cursor(fline, 1)
  if s:debug
    echo 'JpCount : Done. ('. reltimestr(reltime(start)) . ' sec )'
  endif
endfunction

"J �R�}���h���
command! -count JpAltJ call JpAltJ(<line1>, <line2>)
function! JpAltJ(fline, lline)
  let fline = a:fline
  let lline = a:lline
  let lline = lline > line('$') ? line('$') : lline
  let save_cursor = getpos(".")
  let cnt = lline - fline + 1

  if cnt < 0
    let cnt = 1
  endif
  if b:JpFormatGqMode
    let lline = fline + lline - 1
    let chars = 64
    for n in range(fline, lline)
      let chars += strlen(getline(n))
    endfor
    call cursor(fline, 1)
    let b:saved_tw=&textwidth
    silent! exec 'setlocal textwidth='.chars
    silent exec 'normal! '.cnt.'gqq'
    silent! exec 'setlocal textwidth='.b:saved_tw
  endif
  if g:JpFormatMarker != '' && b:jpformat > 0
    let lline = fline + lline - 1
    let glist = getline(fline, lline)
    let loop = len(glist)
    let loop = loop > 1 ? loop -1 : loop
    for i in range(loop)
      if glist[i] != b:JpFormatExclude
        let glist[i] = substitute(glist[i], g:JpFormatMarker.'$', '', '')
      endif
    endfor
    call setline(fline, glist)
  endif
  call setpos('.', save_cursor)
  silent exec 'normal! '.cnt.'J'
endfunction

"�}�[�J�[�����݂���Ȃ玩�����`��ON
"au BufRead *.txt call JpSetAutoFormat()
function! JpSetAutoFormat(...)
  call JpFormatInit()

  let save_cursor = getpos(".")
  call cursor(1, 1)
  let sl = search(g:JpFormatMarker."$", 'cW')
  while 1
    if sl == 0
      break
    endif
    if getline('.') !~ b:JpFormatExclude
      break
    endif
    let sl = search(g:JpFormatMarker."$", 'W')
  endwhile
  call setpos('.', save_cursor)
  if a:0 == 0
    let b:jpformat = sl != 0 ? 1 : 0
  endif
  return sl
endfunction

"=============================================================================
"  Description: gq���g�p����JpFormat�`���̐��`
"=============================================================================
command! -bang -range JpFormatGq call JpFormatGq(<line1>, <line2>, <bang>0)
function!  JpFormatGq(fline, lline, mode)
  let b:jpformat = 1
  let b:saved_tw=&textwidth
  let b:saved_fex=&formatexpr
  if g:JpCountChars_Use_textwidth
    let b:JpCountChars = &textwidth/g:JpFormatCountMode
  endif
  let cmode = g:JpFormatCountMode
  let chars = (b:JpCountChars)*cmode
  silent! exec 'setlocal textwidth='.chars

  if exists('g:JpFormat_formatexpr')
    silent! exec 'setlocal formatexpr='.g:JpFormat_formatexpr
  endif

  let fline = line('.')
  let lines = 1
  let fline = a:fline
  let lines = a:lline-a:fline+1
  let mode = a:mode

  let lnum = fline
  let col = col('.')
  let s:InsertPoint = -1
  while lnum && lines
    call cursor(lnum, col)
    let [lnum, col] = s:JpFormatGqExec(mode)
    let lines -= 1
  endwhile
  if s:InsertPoint > -1
    exec s:InsertPoint.'go'
  endif
  silent! exec 'setlocal formatexpr='.b:saved_fex
  silent! exec 'setlocal textwidth='.b:saved_tw
endfunction

function! s:JpFormatGqExec(mode, ...)
  let mode = a:mode
  let cline = line('.')
  let col = col('.')
  let elen = line2byte(cline) + col - 1

  let fline = cline
  let lline = cline
  if g:JpFormatMarker != ''
    if getline(lline) =~ g:JpFormatMarker.'$'
      let lline = search('[^'.g:JpFormatMarker.']$\|^$', 'ncW')
      if lline == 0
        return [0, 0]
      endif
    endif
    "paragraph
    if mode
      let fline = search('[^'.g:JpFormatMarker.']$\|^$', 'ncbW')
      let fline += 1
    endif
  endif

  " �}�[�J�[���폜
  if g:JpFormatMarker != ""
    for n in range(fline, lline)
      let str = substitute(getline(n), g:JpFormatMarker.'$', '', '')
      call setline(n, [str])
    endfor
  endif

  ":FIXME ���`��̃J�[�\���ʒu�𓾂�
  let ostr = strpart(getline(fline), 0, col-1)

  let l = lline - fline + 1
  call cursor(fline, 1)
  exec 'normal! '.l.'gqq'
  let eline = line('.')
  let nline = eline+1
  let nline = nline > line('$') ? 0 : nline

  ":FIXME ���`��̃J�[�\���ʒu�𓾂�
  let crlen = strlen(g:JpFormatMarker)
  if s:InsertPoint == -1
    let l = fline
    let olen = strlen(ostr)
    let lstr = getline(l)

    let s:InsertPoint = elen
    let elen = -1
    if ostr =~ '^\s*$'
      let elen = matchend(lstr, '^\s*')
      let elen = elen < 0 ? 0 : elen
      let s:InsertPoint = line2byte(l) + (l-fline) * crlen + elen
    elseif olen > strlen(lstr)
      let ostr = strpart(ostr, matchend(ostr, lstr))
      let l+= 1
      while 1
        if ostr =~ '^\s*$'
          let elen = matchend(lstr, '^\s*')
          let elen = elen < 0 ? 0 : elen
          let s:InsertPoint = line2byte(l) + (l-fline) * crlen + elen
          break
        endif
        let olen = strlen(ostr)
        let lstr = getline(l)
        if olen <= strlen(lstr)
          let elen = matchend(lstr, ostr)
          let elen = elen < 0 ? 0 : elen
          let s:InsertPoint = line2byte(l) + (l-fline) * crlen + elen
          break
        endif
        let ostr = strpart(ostr, matchend(ostr, lstr))
        let l+= 1
      endwhile
    endif
  endif

  " �}�[�J�[��t��
  if eline > fline && g:JpFormatMarker != ""
    for n in range(fline, eline-1)
      let str = getline(n)
      if str !~ '^$'
        let str = str . g:JpFormatMarker
      endif
      call setline(n, [str])
    endfor
  endif

  return [nline, 1]
endfunction

function! JpJoinGq(fline, lline, mode)
  let b:jpformat = 1
  let cline = line('.')
  let fline = cline
  let lines = 1
  let fline = a:fline
  let lines = a:lline-a:fline+1
  let mode = a:mode

  ":TODO ���ݍs�̑O�܂Ō����ΏۂɂȂ�s���J�E���g����crlen���}�C�i�X
  "exec elen. 'go' �ŃJ�[�\���ʒu��
  let elen = line2byte(line('.')) + col('.') - 1
  let crlen = strlen(g:JpFormatMarker)
  let crlen += &ff=='dos' ? 2 : 1

  let lnum = fline
  let col = col('.')
  let s:InsertPoint = -1
  while lnum && lines
    call cursor(lnum, col)
    let [lnum, col] = s:JpJoinGqExec(mode, cline)
    let lines -= 1
  endwhile
  let crlen = strlen(g:JpFormatMarker)
  let crlen += &ff=='dos' ? 2 : 1
  let elen -= s:InsertPoint * crlen
  exec elen.'go'
endfunction

function! s:JpJoinGqExec(mode, ...)
  let b:saved_tw=&textwidth

  let mode = a:mode
  let cline = line('.')
  let col = col('.')
  let elen = line2byte(cline) + col - 1

  let lline = search('[^'.g:JpFormatMarker.']$', 'ncW')
  if lline == 0
    return [0, 0]
  endif

  let fline = cline
  "paragraph
  if mode
    let fline = search('[^'.g:JpFormatMarker.']$\|^$', 'ncbW')
    let fline += 1
  endif
  if s:InsertPoint == -1
    let s:InsertPoint = a:1-fline
  endif

  " �}�[�J�[���폜
  let chars = 64  "�ԕ�
  for n in range(fline, lline)
    let str = getline(n)
    let chars += strlen(str)
    call setline(n, [substitute(str, g:JpFormatMarker.'$', '', '')])
  endfor
  silent! exec 'setlocal textwidth='.chars

  let l = lline - fline + 1
  call cursor(fline, 1)
  exec 'normal! '.l.'gqq'
  let eline = line('.')
  let nline = eline+1
  let nline = nline > line('$') ? 0 : nline

  silent! exec 'setlocal textwidth='.b:saved_tw
  return [nline, 1]
endfunction

"=============================================================================
"    Description: �O���r���[�A�Ăяo��
"     Maintainer: fuenor@gmail.com
"                 http://sites.google.com/site/fudist/Home/extviewer
"  Last Modified: 2009-11-08 18:00
"        Version: 1.00
"=============================================================================
if exists("loaded_ExtViewer") && !exists('fudist')
  finish
endif
if exists('disable_ExtViewer') && disable_ExtViewer
  finish
endif
let loaded_ExtViewer = 1
if &cp
  finish
endif

if !exists('g:ExtViewer_cmd')
  let ExtViewer_cmd = '!start "'.$windir.'/notepad.exe" "%f"'
"  let ExtViewer_cmd = '!start "C:/Program Files/Mozilla firefox/firefox.exe" file://%f'
"  let ExtViewer_cmd = '!start "C:/Program Files/Internet Explorer/iexplore.exe" file://%f'
  if has('unix')
    let ExtViewer_cmd = "call system('evince %f &')"
  endif
endif

"JpFormat���g�p�����A�����s��
if !exists('EV_JoinStr')
  let EV_JoinStr = 1
endif
"�r���[�A��y�[�W������̍s��
if !exists('EV_CountLines')
  let EV_CountLines = 16
endif

"JpFormat.vim���g�p���Ă��鎞�Ƀ}�[�J�[���폜����
if !exists('EV_RemoveMarker')
  let EV_RemoveMarker = 0
endif
"Windows?
let s:MSWindows = has('win95') + has('win16') + has('win32') + has('win64')
let s:tmpname = tempname()

"�O���r���[�A�N��
command! -nargs=* -range=% -bang ExtViewer call ExtViewer(<bang>0, <line1>, <line2>, <f-args>)
command! -nargs=* -range=% -bang JpExtViewer call ExtViewer(<bang>0, <line1>, <line2>, 'txt')
function! ExtViewer(mode, fline, lline, ...)
  let fline = a:fline
  let lline = a:lline
  let file = ''
  let line = 0
  let suffix = ''

  if a:0
    let suffix = a:1
  endif
  if suffix == ''
    let suffix = expand("%:e")
  endif
  if suffix == 'howm' || suffix == 'nvl'
    let suffix = expand("txt")
  endif
  if a:mode == 0
    if exists('g:EV_Tempname_'.suffix)
      exec 'let s:tmpname = '. 'g:'.'EV_Tempname_'.suffix
    endif
    let s:tmpname = fnamemodify(s:tmpname, ":r") .'.'.suffix
    let file = s:tmpname
  else
    let file = expand('%:p')
  endif
  if line == 0
    let line = line('.')
    if fline != 1 || lline != line('$')
      let line = 1
    endif
  endif
  let file = substitute(file, '\', '/', 'g')
  let file = expand(file)

  if a:mode == 0
    if exists('*EVwrite_'.suffix)
      exec 'let line = EVwrite_'.suffix.'("'.file.'", '.fline.', '. lline.')'
    else
      let line = EVwrite_template(file, fline, lline)
    endif
  endif
  call EVAddTempFileList(file)
  if suffix =~ 'html\|htm'
    let file = substitute(file, ' ', '%20', 'g')
  elseif has('unix')
    let file = escape(file, ' ')
  endif
  let cmd = g:ExtViewer_cmd
  if exists('g:ExtViewer_'.suffix)
    exec 'let cmd = g:ExtViewer_'.suffix
  endif
  let cmd = substitute(cmd, '%f', file, '')
  let cmd = substitute(cmd, '%l', line, '')
  if s:MSWindows
    let cmd = iconv(cmd, &enc, 'cp932')
  endif
  let cmd = escape(cmd, '%#')
  silent! exec cmd
endfunction

"�O���r���[�A�ɓn���t�@�C�����o��(template)
function! EVwrite_template(file, fline, lline)
  let line = line('.')
  let glist = getline(a:fline, a:lline)
  let cnvCR = &fileformat == 'dos'
  let toFenc = &fileencoding
  "fenc�Ɖ��s�����t�@�C���Ɠ���ɂ���B
  let loop = len(glist)
  for i in range(loop)
    let glist[i] = iconv(glist[i], &enc, toFenc)
    if cnvCR
      let glist[i] = substitute(glist[i], '$', '\r', '')
    endif
  endfor
  call writefile(glist, a:file, 'b')
  return line
endfunction

"�O���r���[�A�ɓn���t�@�C�����o��
function! EVwrite_txt(file, fline, lline)
  let suffix = 'txt'
  let removeMarker = g:EV_RemoveMarker
  if !exists('g:JpFormatMarker') || g:JpFormatMarker == ''
    let removeMarker = 0
    let catmarker=''
  else
    let catmarker = g:JpFormatMarker.'$'
  endif
  let cnvCR = &ff == 'dos'
  let toFenc = &fenc

  if exists('g:EV_toFenc_'.suffix)
    exec 'let toFenc = g:EV_toFenc_'.suffix
  endif
  let line = line('.')

  if g:EV_JoinStr && exists('b:jpformat')
    let [glist, lines, delmarker] = JpJoinStr(a:fline, a:lline)
    let line -= delmarker
    let removeMarker = 0
    let catmarker=''
  else
    let glist = getline(a:fline, a:lline)
  endif
  let loop = len(glist)
  for i in range(loop)
    let glist[i] = iconv(glist[i], &enc, toFenc)
    if removeMarker && glist[i] != b:JpFormatExclude
      let glist[i] = substitute(glist[i], catmarker, '', '')
    endif
    if cnvCR
      let glist[i] = substitute(glist[i], '$', '\r', '')
    endif
  endfor
  call writefile(glist, a:file, 'b')
  return line
endfunction

command! -count EVJumpPage silent! exec 'normal! '.((count-1)*g:EV_CountLines).'gg'

augroup ExtViewer
  au!
  au VimLeave * call s:EVLeave()
augroup END

let s:flist = []
function! EVAddTempFileList(file)
  if count(s:flist, a:file) > 0
    return
  endif
  call add(s:flist, a:file)
endfunction

function! s:EVLeave()
  for fname in s:flist
    call delete(fname)
  endfor
endfunction

