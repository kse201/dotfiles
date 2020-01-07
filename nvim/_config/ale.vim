" let g:ale_ruby_rubocop_options='--config .rubocop_todo.yml'
let g:ale_ansible_ansible_lint_executable  = "ansible-lint"
let g:ale_echo_msg_error_str = "\uf188 "
let g:ale_echo_msg_warning_str = "\uf071 "
let g:ale_echo_msg_format = '[%linter%] [%severity%] %s'
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

function! ALEEnableXorLSP()
    if has_key(g:LanguageClient_serverCommands, &filetype)
        ALEDisable
    else
        ALEEnableBuffer
    endif
endfunction

autocmd FileType * call ALEEnableXorLSP()
