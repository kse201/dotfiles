let g:fern#renderer = "devicons"

" Keymap
function! s:init_fern() abort
  nmap <buffer> <Plug>(fern-action-open) <Plug>(fern-action-open:select)
  nmap <buffer> R <Plug>(fern-action-reload:all)
  nmap <buffer> u <Plug>(fern-action-leave)
  nmap <buffer> p <Plug>(fern-action-project-top)
  nmap <buffer> I <Plug>(fern-action-hidden)
endfunction

augroup fern-custom
  autocmd! *
  autocmd FileType fern call s:init_fern()
augroup END

nmap <C-n> :Fern . -drawer -toggle<CR>
