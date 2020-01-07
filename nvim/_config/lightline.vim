function! LSPStatus() abort
    let l:errors = 0
    let l:warnings = 0
    for item in getqflist()
        if item["type"] == "E"
            let l:errors += 1
        else
            let l:warnings += 1
        endif
    endfor
    " Error =>  , Warning => 
    return l:errors + l:warnings == 0 ? "" : "\uf188 :" . l:errors . " " . "\uf071 :" . l:warnings
endfunction

function! LightlineModified()
    return &ft =~ 'help\|vimfiler' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help' && &readonly ?  "\ue0a2" : ''
endfunction

function! LightlineFilename()
    return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
                \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
                \  &ft == 'unite' ? unite#get_status_string() :
                \  &ft == 'vimshell' ? vimshell#get_status_string() :
                \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
                \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
    if &ft !~? 'vimfiler' && exists('*fugitive#head')
        let branch = fugitive#head()
        " 
        return branch !=# '' ? "\ue0a0 ".branch : ''
    endif
    return ''
endfunction

function! MyFiletype()
    " return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' : 'no ft') : ''
endfunction

function! MyFileformat()
    " return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
    return winwidth(0) > 70 ? (&fileformat . ' ' ) : ''
endfunction

let g:lightline = {
            \ 'active': {
            \ 'left': [ [ 'mode', 'paste' ],
            \           [ 'fugitive', 'filename' ],
            \           [ 'lsp' ] ],
            \ },
            \ 'component_function': {
		    \   'fugitive': 'LightlineFugitive',
		    \   'filename': 'LightlineFilename',
            \   'filetype': 'MyFiletype',
            \   'fileformat': 'MyFileformat',
            \   'lsp': 'LSPStatus',
            \ }
            \}
