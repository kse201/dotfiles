is_exist()  { which "$1" >/dev/null 2>&1 ; return $?; }

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

export LANG='ja_JP.UTF-8'
export LC_ALL='ja_JP.UTF-8'
export LC_MESSAGES='ja_JP.UTF-8'

export LESSCHARSET=utf-8
export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case --quit-if-one-screen --RAW-CONTROL-CHARS -X'
export EDITOR='vi'

# 補完時に大文字小文字の違いを無視する
set completion-ignore-case on

# bash を8bitクリーンにする
set convert-meta off
set output-meta on

########################################
# history
########################################
export HISTCONTROL=ignoreboth:erasedups:
export HISTIGNORE=history:hst:ls:'which *':cd:'. ~/.bashrc'

export HISTSIZE=7500


########################################
# prompt
########################################
export PATH=$PATH:/opt/local/:$HOME/bin
# PS1="\u@\h \w\n\$ "
# PS1="\033[031m\]\u\[\033[0m\]"
BRACK="\[\e[0;30m\]"
BLUE="\[\e[0;34m\]"
GREEN="\[\e[0;32m\]"
CYAN="\[\e[0;36m\]"
RED="\[\e[0;31m\]"
PURPLE="\[\e[0;35m\]"
LIGHTGRAY="\[\e[0;37m\]"
DARKGRAY="\[\e[1;30m\]"
LIGHTBLUE="\[\e[1;34m\]"
LIGHTGREEN="\[\e[1;32m\]"
LIGHTCYAN="\[\e[1;36m\]"
LIGHTRED="\[\e[1;31m\]"
LIGHTPURPLE="\[\e[1;35m\]"
YELLOW="\[\e[0;33m\]"
WHITE="\[\e[1;37m\]"
END="\[\e[0m\]"

function length()
{
    echo -n ${#1}
}

function init-prompt-git-branch()
{
    git symbolic-ref HEAD 2>/dev/null >/dev/null &&
        echo "($(git symbolic-ref HEAD 2>/dev/null | sed 's/^refs\/heads\///'))"
}

if is_exist 'git' ; then
    export PS1_GIT_BRANCH="\[\e[$[COLUMNS]D\]${LIGHTRED}\[\e[$[COLUMNS-$(length $(init-prompt-git-branch))]C\]$(init-prompt-git-branch)\[\e[$[COLUMNS]D\]${END}"
else
    export PS1_GIT_BRANCH=
fi
HABA="\[\e[$[COLUMNS]D\]\[\e[$[COLUMNS-$(length "hogehoge")]C\]"
# PS1=" ${BRACK}BRACK\n ${LIGHTGRAY}LIGHTGRAY\n ${DARKGRAY}DARKGRAY\n ${GREEN}GREEN\n ${LIGHTGREEN}LIGHTGREEN\n ${BLUE}BLUE\n ${LIGHTBLUE}LIGHTBLUE\n ${CYAN}CYAN\n ${LIGHTCYAN}LIGHTCYAN\n ${RED}RED\n ${LIGHTRED}LIGHTRED\n ${PURPLE}PURPLE\n ${LIGHTPURPLE}LIGHTPURPLE\n ${YELLOW}YELLOW\n ${WHITE}WHITE\n "
PS1="-${LIGHTGREEN}\u${END}@${GREEN}\h${END} ${YELLOW}\w${END}${DARKGRAY} [\T]${END}\n ${LIGHTPURPLE}(\!)${END}\$ "

########################################
# alias
########################################

# grep
# 検索ワード色付け
export GREP_COLOR='1;3741'
alias grep='grep -E --color=auto'
alias G='grep'

# -i 確認 -v 詳細な情報の表示
alias cp='cp -iv'
# alias rm='rm -iv'
alias mv='mv -iv'

alias less='less -r'

# ls
export ls_colors='no=01;37:fi=00:di=01;36:pi=40;33:so=01;35:bd=40;33:cd=40;33;01:or=40;32;01:ex=01;33:*core=01;31:'
case `uname` in
    "Darwin") # BSD ls
        alias ls='/bin/ls -FG'
        ;;
    *) # GNU ls
        alias ls='/bin/ls -F --color=tty --show-control-chars'
        ;;
esac
alias ll='ls -l'
alias la='ls -la'

alias hst='history'

alias L="less"

# 検索ワード色付け
export GREP_COLOR='1;3741'
alias grep='grep -E --color=auto'

if is_exist 'vim' ; then
    alias vi="vim"
    # spartan Vim
    alias spvim='vim -u NONE'
fi


if [ "$TERM" == xtrem ] ; then
    export TERN=xterm-color
fi

if is_exist 'colordiff' ; then
      alias diff='colordiff'
fi

if is_exist 'tree' ; then
    alias tree="pwd;find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'"
fi

########################################
# cd
########################################
# ディレクトリ名に変数を指定できる
cdable_vars=on

man() {
        env \
                LESS_TERMCAP_mb=$(printf "\e[1;31m") \
                LESS_TERMCAP_md=$(printf "\e[1;31m") \
                LESS_TERMCAP_me=$(printf "\e[0m") \
                LESS_TERMCAP_se=$(printf "\e[0m") \
                LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
                LESS_TERMCAP_ue=$(printf "\e[0m") \
                LESS_TERMCAP_us=$(printf "\e[1;32m") \
                man "$@"
}


# screen
export SCREENDIR="$HOME/.screens"
if [ ! -e "$HOME/.log" ] ; then
    mkdir "$HOME/.log"
fi

# local setting
if [ -f "$HOME/.bashrc.local" ] ; then
    source "$HOME/.bashrc.local"
fi

function timestamp () {
    date +%Y%m%d%H%M%S
}

# golang

if [ -e '/usr/local/go' ] ; then
    export PATH=$PATH:/usr/local/go/bin:$HOME/.go/bin
fi
if is_exist 'go' ; then
    export GOPATH="${HOME}/.go"
fi

if [ -e "$HOME/proxyrc" ] ; then
    source "$HOME/proxyrc"
fi

# fzf
if [ -f "${HOME}"/.fzf.bash ] ; then
    source "${HOME}"/.fzf.bash
else
    git clone --depth 1 https://github.com/junegunn/fzf.git "${HOME}"/.fzf
    "${HOME}"/.fzf/install && source "${HOME}"/.fzf.bash
fi
