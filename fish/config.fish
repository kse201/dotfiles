set -x PATH ( find $HOME/bin/ -type d ) (ls -d /usr/local/*bin) $PATH
set -x PATH (gem environment gempath | sed 's/:/\\n/g' | xargs -I@ echo @/bin) $PATH
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

set -x FZF_DEFAULT_COMMAND 'fd --type f --hidden --follow --exclude .git --color=always'
set -x FZF_DEFAULT_OPTS '--ansi'

function fzf_edit
    fzf | read select_line
    if [ $select_line ]
        eval $EDITOR $select_line
    end
end

function fzf_cd
    find ./ -type d | fzf | read select_line
    if [ $select_line ]
        eval cd $select_line
    end
end

abbr vst vagrant status
abbr vup vagrant up

if test -f (which hub)
    alias git hub
end

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

if test -f (which bat)
    abbr cat bat
end

abbr be bundle exec
abbr ber bundle exec rake

abbr brname git symbolic-ref --short HEAD

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
  bind \cr peco_history
  bind \cv fzf_edit
  bind \cc fzf_cd
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


# set -x PYENV_ROOT "$HOME/.pyenv"
# set -x PATH "$PYENV_ROOT/bin:$PATH"
# if which pyenv > /dev/null and test -f (which pyenv)
    # eval (pyenv init - | source)
# end

if test -d $HOME/.yarn/bin
    set -x PATH "$HOME/.yarn/bin/:$PATH"
end

function command_not_found_handler --on-event fish_command_not_found
    echo "ハァ...?「$argv[1]」とか何言ってんの ?"
end

if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

if test -f (which starship)
    eval (starship init fish)
end

if test -f (which docker-compose)
    abbr dc  docker-compose
end

set -x GOENV_DISABLE_GOPATH 1
if which anyenv >/dev/null 2>&1
    set -x PATH $HOME/.anyenv/bin $PATH
    eval (anyenv init - | source)
end

function __call_navi
    navi --print
end

function navi-widget -d "Show cheat sheets"
  begin
    set ttysettings (stty -g)
    stty sane
    __call_navi | perl -pe 'chomp if eof' | read -lz result
    and commandline -- $result

    stty $ttysettings
  end
  commandline -f repaint
end

bind \co navi-widget
if bind -M insert > /dev/null 2>&1
  bind -M insert \cg navi-widget
end

if which sccache >/dev/null 2>&1
    set -x RUSTC_WRAPPER (which sccache)
end
