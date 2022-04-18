setlocal shiftwidth=2
setlocal tabstop=2
setlocal dictionary=$VIMFILE_DIR/dict/ruby.dict

if filereadable(expand('~/rtags'))
    au MyAutoCmd FileType ruby,eruby setl tags+=~/rtags,~/gtags
endif
