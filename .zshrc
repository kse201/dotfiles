#============================================================
#                      *** .zshrc ***                       |
#============================================================

########################################
# General
########################################

is_exist()  { which "$1" >/dev/null 2>&1; return $?; }

# Source global definitions
if [ -f /etc/zshrc ]; then
    . /etc/zshrc
fi

if [ ! -f $HOME/.zshrc.zwc -o $HOME/.zshrc -nt $HOME/.zshrc.zwc ]; then
    zcompile $HOME/.zshrc
fi
# env
export PATH=/usr/local/bin:/usr/local/sbin:$PATH:$HOME/bin
typeset -U path PATH
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case --quit-if-one-screen --RAW-CONTROL-CHARS -X'
export EDITOR='vi'
export RSYNC_RSH=ssh
export CVS_RSH=ssh

# history
setopt \
    extended_history \
    hist_ignore_dups \
    hist_ignore_space \
    hist_expire_dups_first \
    hist_expand \
    hist_reduce_blanks \
    inc_append_history \
    share_history
unsetopt hist_verify

HISTFILE=$HOME/.zsh_history
HISTSIZE=7500
SAVEHIST=7500
export HISTIGNORE="ls *:cd:history:fg*:history-all"
function history-all { history -E 1 }

autoload -U compinit
compinit -u
zstyle ':completion:*' format '%B%d%b'
zstyle ':completion:*' group-name ''

zstyle ':completion:*:default' menu select=2
## color completion
zstyle ':completion:*:default' list-colors ""
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'

zstyle ':completion:*' completer _oldlist _complete _match _history _ignored _approximate _prefix _list
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' verbose yes
zstyle ':completion:*' ignore-parents parent pwd ..

REPORTTIME=3

watch="all"
log

limit coredumpsize 102400

bindkey -e

LISTMAX=0
export LISTMAX
setopt auto_cd auto_remove_slash auto_name_dirs

setopt extended_glob list_types 
setopt cdable_vars sh_word_split
setopt auto_resume
setopt correct
setopt \
    no_menu_complete \
    complete_aliases \
    complete_in_word \
    glob_complete
setopt list_rows_first
setopt auto_pushd \
    pushd_minus \
    pushd_ignore_dups
setopt numeric_glob_sort

setopt magic_equal_subst
setopt mark_dirs
setopt long_list_jobs

cdpath=(~)
chpwd_functions=($chpwd_functions dirs)

setopt auto_param_keys
setopt list_packed
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^P' history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

export ls_colors='no=01;37:fi=00:di=01;36:pi=40;33:so=01;35:bd=40;33:cd=40;33;01:or=40;32;01:ex=01;33:*core=01;31:'
export GREP_COLOR='1;3741'

WORDCHARS='*?_-.[]~=&;!#S%^(){}<>'
WORDCHARS=${WORDCHARS:s,/,,}

cdpath=($HOME)

########################################
# Prompt
########################################

setopt \
    transient_rprompt \
    prompt_subst \
    prompt_percent \
    no_beep \
    always_last_prompt
unsetopt promptcr
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '%F{green}%u%c(%b)%f'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}+"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}!"
precmd () { vcs_info }
prompt_bar_left="%F{cyan}%n%{%b%}@%F{cyan}%m%{%b%}:%~"
prompt_bar_right=""
prompt_left="%(1j,(%j),)%# "

count_prompt_characters()
{
    print -n -P -- "$1" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'
}

update_prompt()
{
    local bar_left="$prompt_bar_left"
    LANG=C vcs_info >&/dev/null
    local vcs_info="${vcs_info_msg_0_}"
    PROMPT="${bar_left}${vcs_info}${prompt_left}"
}

precmd_functions=($precmd_functions update_prompt)

PROMPT2="%_%%"
SPROMPT="%{$fg[red]%}%{$suggest%}(*'_'%)? < You mean %B%r%b %{$fg[red]%}? [y,n,a,e]:${reset_color} "

########################################
# Alias
########################################

alias -s py="python"
alias -s rb="ruby"
alias -s txt="cat"

### iab
alias -g L="| less"
alias -g G='| grep'
alias -g H='| head'
alias -g T='| tail'
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc'

alias remem='du -sx / &> /dev/null & sleep 25 && kill $!'

if is_exist 'pbcopy' ; then
    # Mac
    alias -g C='| pbcopy'
elif is_exist 'xsel' ; then
    # Linux
    alias -g C='| xsel --input --clipboard'
elif is_exist 'putclip' ; then
    # Cygwin
    alias -g C='| putclip'
fi

if is_exist 'colordiff' ; then
    alias diff='colordiff'
fi

if ! is_exist 'tree' ; then
    alias tree="pwd;find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'"
fi

os=$(uname)
if [ ${os} = "Darwin" ] ; then
    alias ls="ls -G"
else
    alias ls="ls --color=auto"
fi
alias cp='cp -i'
alias mv='mv -i'
alias grep='grep -E --color=auto'
alias ll='ls -l'
alias la='ls -la'

# vim
if is_exist 'vim' ; then
    alias vi="vim"
    # spartan Vim
    alias spvim='vim -u NONE'
fi

[[ $EMACS = t ]] && unsetopt zle

# emacs
if [ `uname` != "Darwin" ] ; then
    alias emacsclient=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n
    alias e='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n'
fi

alias dl='docker ps -l -q'
function container_ip () {
docker inspect $1 | grep IPAddres | awk -F'"' '{print $4}'
}

# man
if is_exist 'tldr' ; then
    alias man='tldr'
fi

########################################
# Packages
########################################

PLUGIN_MNGR="${HOME}/.zplug/zplug"
if [ ! -e ${PLUGIN_MNGR} ] ; then
    curl -fLo ${PLUGIN_MNGR} --create-dirs https://git.io/zplug
fi

source ${PLUGIN_MNGR}

zplug "mollifier/cd-gitroot"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zaw"
zplug "mollifier/cd-bookmark"
zplug "mollifier/anyframe"
zplug "b4b4r07/enhancd", of:enhancd.sh
zplug "zsh-users/zsh-history-substring-search", do:"__zsh_version 4.3"
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "junegunn/fzf-bin", as:command, from:gh-r, file:fzf
zplug "peco/peco", as:command, from:gh-r, of:"*amd64*"

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose

########################################
# Function
########################################

conf() {
    case $1 in
        bash)   vim $HOME/.bashrc ;;
        git)    vim $HOME/.gitconfig ;;
        tmux)   vim $HOME/.tmux.conf ;;
        screen) vim $HOME/.screenrc ;;
        vim)    vim $HOME/.vimrc ;;
        zsh)    vim $HOME/.zshrc && source $HOME/.zshrc ;;
        *)      echo "Unknown application: $1" ;;
    esac
}

reload (){
    exec $SHELL
}

timestamp() {
    date +%Y%m%d%H%M%S
}

########################################
# Screen
########################################

export SCREENDIR=$HOME/.screens
if [ ! -e "$HOME/.log" ] ; then
    mkdir "$HOME/.log"
fi

# local setting
if [ -f "$HOME/.zshrc.local" ] ; then
    source "$HOME/.zshrc.local"
fi

########################################
# Misc
########################################

# Haskell
# Add GHC 7.8.4 to the PATH, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/GHC.app"
if [ -d "$GHC_DOT_APP" ]; then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

# golang
if [ -e '/usr/local/go' ] ; then
    export PATH=$PATH:/usr/local/go/bin:$HOME/.go/bin
fi
if is_exist 'go' ; then
    export GOPATH="${HOME}/.go"
    alias gopkg="find $GOPATH -name '*.go' | grep -E \"\/[^\.].+\.go\" | sed -e 's/^.*src\/\(.*\)\/.*go$/\"\1\"/' | sort | uniq | grep -v $USER"
fi

if is_exist "cabal" ; then
    export PATH=$HOME/.cabal/bin:$PATH
fi

# fzf
if [ -f "${HOME}"/.fzf.zsh ] ; then
    source "${HOME}"/.fzf.zsh
else
    git clone --depth 1 https://github.com/junegunn/fzf.git "${HOME}"/.fzf
    "${HOME}"/.fzf/install && source "${HOME}"/.fzf.zsh
fi

if [ -f ~/.fzf.zsh ] ; then
    source ~/.fzf.zsh
    bindkey '^Y' fzf-file-widget
fi
