set -x PATH ( find $HOME/bin/ -type d ) (ls -d /usr/local/*bin) $PATH
set -x PATH $HOME/.gem/ruby/2.5.0/bin $PATH
set -x LANG ja_JP.UTF-8
set -x LC_ALL ja_JP.UTF-8
set -x LESSCHARSET utf-8
set -x EDITOR 'nvim'
set -x HISTSIZE 750
set -x SAVEHIST 7500
set -x HISTIGNORE "ls *:cd:history:fg*:history-all"

abbr pacman yay

abbr vi nvim
abbr vim nvim
abbr spvim nvim -u /dev/null

function fzf_edit
    $EDITOR (fzf)
end

abbr vst vagrant status
abbr vup vagrant up

abbr gst git st
abbr glg git lg
abbr gmt git cmt
abbr gco git co
abbr gct git ct
abbr gdf git df

abbr re bundle exec

if test -f (which exa)
    abbr ls exa
    abbr ll exa -lhg --git --time-style iso
    abbr la exa -lhga --git --time-style iso
end

abbr be bundle exec
abbr ber bundle exec rake

abbr brname git symbolic-ref --short HEAD

abbr diff colordiff -wu

function peco_z
  set -l query (commandline)

  if test -n $query
    set peco_flags --query "$query"
  end

  z -l | peco --prompt "DIRECTORY>" $peco_flags | awk '{ print $2 }' | read recent
  if [ $recent ]
      cd $recent
      commandline -r ''
      commandline -f repaint
  end
end

# abbr ghl peco_select_ghq_repository
function fish_user_key_bindings
  bind \cg peco_select_ghq_repository
  bind \cs peco_ssh
  bind \cr peco_select_history
  bind \cv fzf_edit
  bind \x1d peco_z # => Ctrl=]
end

rbenv init - | source

set -x PATH $HOME/.rbenv/shims $PATH
if test -f $HOME/.yarn/bin
    set -x PATH $HOME/.yarn/bin $PATH
end

set -x PATH $HOME/go/bin $PATH
set -x PATH $HOME/.local/bin/ $PATH

set -x GOPATH $HOME/go

set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"

function __direnv_export_eval --on-event fish_prompt;
    eval (direnv export fish);
end

if test -f /usr/share/doc/pkgfile/command-not-found.fish
    source /usr/share/doc/pkgfile/command-not-found.fish
end

if test -f $HOME/.cargo/env
    source $HOME/.cargo/env
end

if test -f /usr/local/opt/python/libexec/bin
    source  /usr/local/opt/python/libexec/bin
end
set -x PYTHONSTARTUP $HOME/.pythonrc.py

function command_not_found_handler --on-event fish_command_not_found
    echo "ハァ...?「$argv[1]」とか何言ってんの ?"
end

if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end
