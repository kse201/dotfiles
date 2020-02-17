nmap <C-n> :NERDTreeToggle<CR>
let NERDTreeShowBookmarks=1
let g:NERDTreeDirArrows = 1
let g:NERDTreeDirArrowExpandable  = '|'
let g:NERDTreeDirArrowCollapsible = '>'

function! NERDTreeHighlightFile(extension, fg, bg)
    exec 'autocmd filetype nerdtree syn match NERDTreeTxtFile #^\s\+.*'. a:extension .'$#'
    exec 'autocmd filetype nerdtree highlight NERDTreeTxtFile ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:bg .' guifg='. a:fg
endfunction

call NERDTreeHighlightFile('md',     'blue',    'black')
