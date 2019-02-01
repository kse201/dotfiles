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
