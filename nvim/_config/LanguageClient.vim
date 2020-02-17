set hidden
let g:LanguageClient_autoStart = 1
let g:LanguageClient_selectionUI = "quickfix"
let g:LanguageClient_serverCommands = {
            \ 'c': ['clangd'],
            \ 'python': ['pyls', '-vv', '--log-file', '/tmp/pyls.log'],
            \ 'ruby': ['solargraph', 'stdio'],
            \ 'javascript': ['/home/vagrant/.yarn/bin/javascript-typescript-stdio'],
            \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
            \ 'dockerfile': ['docker-langserver', '--stdio'],
            \ 'rust': ['rustup', 'run', 'stable', 'rls'],
            \ 'sql': ['sql-language-server', 'up', '--method', 'stdio', '--debug'],
            \ }

function! LC_maps()
    if has_key(g:LanguageClient_serverCommands, &filetype)
        nnoremap <buffer> <silent> K :call LanguageClient#textDocument_hover()<cr>
        nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
        nnoremap <buffer> <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
        nnoremap <silent> <F3> :call LanguageClient_textDocument_references()<CR>
        nnoremap <silent> <C-l> :call LanguageClient_contextMenu()<CR>
    endif
endfunction

autocmd FileType * call LC_maps()
let g:LanguageClient_loggingFile = expand('/tmp/LanguageClient.log')
let g:LanguageClient_loggingLevel = 'DEBUG'
