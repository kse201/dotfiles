set -x PATH $HOME/bin /usr/local/bin /usr/local/sbin /usr/local/rbenv/bin $PATH
set -x LANG ja_JP.UTF-8
set -x LESSCHARSET utf-8
set -x EDITOR 'vim'
set -x HISTSIZE 7500
set -x SAVEHIST 7500
set -x HISTIGNORE "ls *:cd:history:fg*:history-all"

set -x RBENV_ROOT /usr/local/rbenv
rbenv init - | source
