let s:ignore_globs = [
            \ '.git',
            \ '.svn',
            \ '.tox',
            \ 'node_modules',
            \ '*.pyc'
            \ ]
" call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>')
" call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>')
call denite#custom#source('file_mru', 'matchers', ['matcher/fuzzy', 'matcher/project_files'])
call denite#custom#var('file/rec', 'command', [
            \ 'ag',
            \ '--follow',
            \ ] + map(deepcopy(s:ignore_globs), { k, v -> '--ignore=' . v }) + [
            \ '--nocolor',
            \ '--nogroup',
            \ '-g',
            \ ''
            \ ])
call denite#custom#filter('matcher/ignore_globs', 'ignore_globs', s:ignore_globs)
call denite#custom#var('grep', 'command', ['ag'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'default_opts', ['--follow', '--no-group', '--no-color'])
