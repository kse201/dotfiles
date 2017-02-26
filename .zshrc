#============================================================
#                      *** .zshrc ***                       |
#============================================================

########################################
# General
########################################

setopt auto_menu correct

# autoload
autoload -U  run-help
autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz colors; colors
autoload -Uz compinit; compinit -u
autoload -Uz is-at-least
autoload -Uz history-search-end
autoload -Uz modify-current-argument
autoload -Uz smart-insert-last-word
autoload -Uz terminfo
autoload -Uz vcs_info
autoload -Uz zcalc
autoload -Uz zmv
autoload     run-help-git
autoload     run-help-svk
autoload     run-help-svn

is_exist()  { which "$1" >/dev/null 2>&1; return $?; }

if [ ! -f $HOME/.zshrc.zwc -o $HOME/.zshrc -nt $HOME/.zshrc.zwc ]; then
    zcompile $HOME/.zshrc
fi
# env
export PATH=$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH
typeset -U path PATH
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export EDITOR='vim'
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

export HISTFILE=$HOME/.zsh_history
export HISTSIZE=7500
export SAVEHIST=7500
export HISTIGNORE="ls *:cd:history:fg*:history-all"
function history-all { history -E 1 }

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

exec_login(){
    w
}
exec_login


limit coredumpsize 102400

bindkey -e

export LISTMAX=0
setopt auto_cd auto_remove_slash auto_name_dirs

setopt extended_glob list_types no_case_glob
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
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^P' history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

export ls_colors='no=01;37:fi=00:di=01;36:pi=40;33:so=01;35:bd=40;33:cd=40;33;01:or=40;32;01:ex=01;33:*core=01;31:'
export GREP_COLOR='1;3741'

export WORDCHARS='*?_-.[]~=&;!#S%^(){}<>'
export WORDCHARS=${WORDCHARS:s,/,,}

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

if is_exist 'colordiff' ; then
    alias diff='colordiff'
fi

os=$(uname)
if [ ${os} = "Darwin" ] ; then
    alias ls="ls -G"
else
    alias ls="ls --color=auto"
fi
alias cp='cp -i'
alias mv='mv -i'
alias grep='grep --color=auto'
alias ll='ls -l'
alias la='ls -la'

tmux() {
    if [ $# -eq 0 ] ; then
        $(which tmuxx)
    else
        $(which tmux) $@
    fi
}

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
# Misc
########################################

# golang
[[ -s "${HOME}/.gvm/scripts/gvm" ]] && source "${HOME}/.gvm/scripts/gvm"

if is_exist 'go' ; then
    export GOPATH=$HOME/.go
    export PATH=$PATH:/usr/local/go/bin:$HOME/.go/bin
    alias gopkg="find $GOPATH -name '*.go' | grep -E \"\/[^\.].+\.go\" | sed -e 's/^.*src\/\(.*\)\/.*go$/\"\1\"/' | sort | uniq | grep -v $USER"
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

#
if is_exist "pry" ; then
    alias irb="pry"
fi

# gcloud
if [ -d "/usr/local/google-cloud-sdk" ] ; then
    export PATH=$PATH:/usr/local/google-cloud-sdk/bin
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f /usr/local/google-cloud-sdk/path.zsh.inc ]; then
  source '/usr/local/google-cloud-sdk/path.zsh.inc'
fi

# The next line enables shell command completion for gcloud.
if [ -f /usr/local/google-cloud-sdk/completion.zsh.inc ]; then
  source '/usr/local/google-cloud-sdk/completion.zsh.inc'
fi

if is_exist 'rbenv' ; then
    eval "$(rbenv init -)"
fi
