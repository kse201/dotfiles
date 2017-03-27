set -x PATH $HOME/bin /usr/local/bin /usr/local/sbin $PATH
set -x LANG ja_JP.UTF-8
set -x LESSCHARSET utf-8
set -x EDITOR 'nvim'
set -x HISTSIZE 7500
set -x SAVEHIST 7500
set -x HISTIGNORE "ls *:cd:history:fg*:history-all"

abbr vi nvim
abbr vim nvim

abbr vst vagrant status
abbr vup vagrant up

abbr gst git status
abbr glg git lg

if test -e $HOME/.rbenv
  set -x RBENV_ROOT $HOME/.rbenv
else
  set -x RBENV_ROOT /usr/local/rbenv/bin
  set -x PATH /usr/local/rbenv/bin $PATH
  set -x PATH /usr/local/rbenv/shims $PATH
end

if test -e $HOME/.ruby-version
  set -x PATH $HOME/.gem/ruby/(cat $HOME/.ruby-version)/bin $PATH
end

set -x PATH $HOME/.pyenv/bin $HOME/.pyenv/shims $PATH
