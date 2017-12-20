set -x PATH ( find $HOME/bin/ -type d ) /usr/local/bin /usr/local/sbin $PATH
set -x LANG ja_JP.UTF-8
set -x LESSCHARSET utf-8
set -x EDITOR 'nvim'
set -x HISTSIZE 7500
set -x SAVEHIST 7500
set -x HISTIGNORE "ls *:cd:history:fg*:history-all"

abbr pacman yaourt

abbr vi nvim
abbr vim nvim

abbr vst vagrant status
abbr vup vagrant up

abbr gst git st
abbr glg git lg
abbr gmt git cmt
abbr gco git co
abbr gct git ct
abbr gdf git df

abbr re bundle exec

abbr ls exa
abbr ll exa -lhg --git --time-style iso
abbr la exa -lhga --git --time-style iso

# abbr ghl peco_select_ghq_repository
function fish_user_key_bindings
  bind \cg peco_select_ghq_repository
end
set -x PATH $HOME/.gem/ruby/2.4.0/bin $PATH

set -x PATH $HOME/go/bin $PATH
set -x PATH $HOME/.local/bin/ $PATH

set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"

function __direnv_export_eval --on-event fish_prompt;
    eval (direnv export fish);
end
