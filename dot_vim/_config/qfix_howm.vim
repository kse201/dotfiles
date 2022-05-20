if executable('rg')
    " ripgrepを使う（PATHは通してある前提）
    let mygrepprg = 'rg'
    let myjpgrepprg = 'rg'
    " マルチエンコーディングgrepを使用する
    let MyGrep_MultiEncoding = 1
    " マルチエンコーディングgrepで使用するエンコーディングリスト
    let MyGrep_MultiEncodingList = ['utf-8', 'cp932']
    " 外部grep(shell)のエンコーディング(Windows)
    let MyGrep_ShellEncoding = 'utf-8'
    " 以下、上記URL参考
    " 実行時のオプションをripgrep用に変更（GNU Grepと同じ出力になるように）
    let MyGrepcmd_useropt='-nH --no-heading --color never'
    let MyGrepcmd_regexp=''
    let MyGrepcmd_regexp_ignore='-i'
    let MyGrepcmd_fix='-F'
    let MyGrepcmd_fix_ignore='-F -i'
    let MyGrepcmd_recursive=''
    " gipgrepにファイルパターンとして「*」「*.*」を渡したらエラーになったのでその対策
    let MyGrep_GrepFilePattern='.'
endif
