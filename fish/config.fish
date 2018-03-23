set -x PATH ( find $HOME/bin/ -type d ) /usr/local/bin /usr/local/sbin $PATH
set -x PATH /home/vagrant/.gem/ruby/2.5.0/bin $PATH
set -x LANG ja_JP.UTF-8
set -x LESSCHARSET utf-8
set -x EDITOR 'nvim'
set -x HISTSIZE 750
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

abbr be bundle exec

function peco_z
  set -l query (commandline)

  if test -n $query
    set peco_flags --query "$query"
  end

  z -l | peco $peco_flags | awk '{ print $2 }' | read recent
  if [ $recent ]
      cd $recent
      commandline -r ''
      commandline -f repaint
  end
end

function peco_ssh
  awk '
    tolower($1)=="host" {
      for(i=2;i<=NF; i++) {
        if ($i !~ "[*?]") {
          print $i
        }
      }
    }
  ' ~/.ssh/config | sort | peco | read -l hostname

  if test -n "$hostname"
    ssh $hostname
  end
end

# abbr ghl peco_select_ghq_repository
function fish_user_key_bindings
  bind \cg peco_select_ghq_repository
  bind \cs peco_ssh
  bind \cr peco_select_history
  bind \x1d peco_z # => Ctrl=]
end

rbenv init - | source

set -x PATH $HOME/.rbenv/shims $PATH
set -x PATH $HOME/.gem/ruby/2.4.0/bin $PATH
set -x PATH $HOME/.yarn/bin $PATH

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

function command_not_found_handler --on-event fish_command_not_found
    echo "ハァ...?「$argv[1]」とか何言ってんの ?"
end
