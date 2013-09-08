# 重複したパスを登録しない
typeset -U path
# (N-/) をつけることで存在しなけらえば無視してくれる
path=($path /usr/*/bin(N-/) /usr/local/*/bin(N-/) /var/*/bin(N-/))
export EDITOR=vim

limit coredumpsize 0

export RSYNC_RSH=ssh
export CVS_RSH=ssh


