scriptencoding utf-8

" 見出し（行頭が'*'の行）
syntax match txtTitle1   '^\*\{1}[^*].*' contains=txtBullet,txtCategory
syntax match txtTitle2   '^\*\{2}[^*].*' contains=txtBullet,txtCategory
syntax match txtTitle3   '^\*\{3}[^*].*' contains=txtBullet,txtCategory
syntax match txtBullet   contained '^\*\+'
syntax match txtCategory contained '\[.\{-}]'

" highlight link txtTitle1   Title
" highlight link txtTitle2   Type
" highlight link txtTitle3   Identifier
" highlight link txtBullet   Special
" highlight link txtCategory Label

" URLとファイル
syntax match txtUrl display '\(http\|https\|ftp\|file\):[-0-9a-zA-Z;/?:@&=+$,_.!~*'()%#]\+'
highlight link txtUrl Underlined
syntax match txtFile '\([A-Za-z]:[/\\]\|\~\/\)[-0-9a-zA-Z;/?:@&=+$,_.!~*'()%{}[\]\\]\+'
highlight link txtFile Underlined

" 引用文 (行頭の'> ')
syntax match txtQuote '^\s*>\(\s.*\|$\)'
highlight link txtQuote Comment

" リスト (行頭の '-' '+')
syntax region txtList start='^[-+]\+\s*' end=':\s' end='$' contains=txtListBullet,txtListColon,txtUrl,txtFile keepend
syntax match txtListBullet contained '^\s*[-+*]\+\s*'
syntax match txtListColon  contained ':\s'

highlight link txtList       Constant
highlight link txtListBullet Statement
highlight link txtListColon  Label

" |*テーブル | 項目 |  (セル内で'*'を使うとタイトル)
syntax match txtTable +^|\(.\{-}|\)\++ contains=txtTableHeader,txtTableSeparator,txtUrl,txtFile
syntax match txtTableHeader    contained +\*[^|]\++
syntax match txtTableSeparator contained +|+

highlight link txtTableHeader    Title
highlight link txtTableSeparator Statement

" 定義リスト (行頭の':'と' :')
syn match txtDefinition '^:.\{-}\s:' contains=txtDefColon
syn match txtDefColon contained '^:\|\s:'

hi link txtDefinition Identifier
hi link txtDefColon   Label

" TODO: FIXME: (行頭の'TODO:' 'FIXME:')
syntax match txtWarning '^\s*\(TODO\|FIXME\):'
highlight link txtWarning TODO

" 区切り線
syntax match txtHLine '-\{20,}'
syntax match txtHLine '=\{20,}'
highlight link txtHLine Label

" キーワード
" syn region txtKeyword start=+"+  skip=+\\"+  end=+"+ end=+$+
" syn region txtKeyword start=+'+  skip=+\\'+  end=+'+ end=+$+
" hi link txtKeyword Define

" hatena (superpreと引用)
syn region hatenaSuperPre   matchgroup=hatenaBlockDelimiter start=+^>|[^|]*|$+ end=+^||<$+
syn region hatenaBlockQuote matchgroup=hatenaBlockDelimiter start=+^>>$+  end=+^<<$+ contains=ALL

hi link hatenaSuperPre       Comment
hi link hatenaBlockDelimiter Delimiter

