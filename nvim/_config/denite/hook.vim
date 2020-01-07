nmap <Leader>a <SID>[denite]
nnoremap <SID>[denite] <Nop>

command! DeniteBuffer Denite buffer
command! DeniteFile Denite file/rec
command! DeniteGrep Denite grep -buffer-name=search-buffer-denite
command! DeniteGrepResume Denite -resume -buffer-name=search-buffer-denite
command! DeniteRegister Denite register
command! DeniteCommand Denite command
command! DeniteYank Denite yank
command! DeniteMru Denite file_mru
command! DeniteLine Denite line

nnoremap <silent> <SID>[denite]b :<C-u>DeniteBuffer<CR>
nnoremap <silent> <SID>[denite]f :<C-u>DeniteFile<CR>
nnoremap <silent> <SID>[denite]g :<C-u>DeniteGrep<CR>
nnoremap <silent> <SID>[denite]r :<C-u>DeniteGrepResume<CR>
" nnoremap <silent> <SID>[denite]r :<C-u>DeniteRegister<CR>
" nnoremap <silent> <SID>[denite]l :<C-u>DeniteLine<CR>
" nnoremap <silent> <SID>[denite]c :<C-u>DeniteCommand<CR>
" nnoremap <silent> <SID>[denite]y :<C-u>DeniteYank<CR>
nnoremap <silent> <SID>[denite]m :<C-u>DeniteMru<CR>
autocmd FileType denite call s:denite_my_settings()
function! s:denite_my_settings() abort
    nnoremap <silent><buffer><expr> <CR>
                \ denite#do_map('do_action')
    nnoremap <silent><buffer><expr> d
                \ denite#do_map('do_action', 'delete')
    nnoremap <silent><buffer><expr> p
                \ denite#do_map('do_action', 'preview')
    nnoremap <silent><buffer><expr> q
                \ denite#do_map('quit')
    nnoremap <silent><buffer><expr> i
                \ denite#do_map('open_filter_buffer')
    nnoremap <silent><buffer><expr> <Space>
                \ denite#do_map('toggle_select').'j'
    nnoremap <silent><buffer><expr> <Tab>
                \ denite#do_map('choose_action')
endfunction
