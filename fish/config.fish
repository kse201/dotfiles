set -x PATH ( find $HOME/bin/ -type d ) /usr/local/bin /usr/local/sbin $PATH
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

# abbr ghl peco_select_ghq_repository
function fish_user_key_bindings
  bind \cg peco_select_ghq_repository
end
set -x PATH $HOME/.gem/ruby/2.4.0/bin $PATH
