#use zsh
#export SHELL=/bin/zsh
# if [ -f /bin/zsh ];then
# exec /bin/zsh
# fi
export LANG='ja_JP.UTF-8'
export LC_ALL='ja_JP.UTF-8'
export LC_MESSAGES='ja_JP.UTF-8'

########################################
# prompt
########################################
# export PATH=$PATH:~/myshellscript:/opt/local/:~/local/bin/
# PS1="\u@\h \w\n\$ "
# PS1="\033[031m\]\u\[\033[0m\]"
BRACK="\e[0;30m\]"
BLUE="\e[0;34m\]"
GREEN="\e[0;32m\]"
CYAN="\e[0;36m\]"
RED="\e[0;31m\]"
PURPLE="\e[0;35m\]"
LIGHTGRAY="\e[0;37m\]"
DARKGRAY="\e[1;30m\]"
LIGHTBLUE="\e[1;34m\]"
LIGHTGREEN="\e[1;32m\]"
LIGHTCYAN="\e[1;36m\]"
LIGHTRED="\e[1;31m\]"
LIGHTPURPLE="\e[1;;35m\]"
YELLOW="\e[0;33m\]"
WHITE="\e[1;37m\]"
END="\e[0m\]"

function length()
{
    echo -n ${#1}
}

function init-prompt-git-branch()
{
    git symbolic-ref HEAD 2>/dev/null >/dev/null &&
        echo "($(git symbolic-ref HEAD 2>/dev/null | sed 's/^refs\/heads\///'))"
}

if which git 2>/dev/null >/dev/null
then
    export PS1_GIT_BRANCH="\[\e[$[COLUMNS]D\]${LIGHTRED}\[\e[$[COLUMNS-$(length $(init-prompt-git-branch))]C\]$(init-prompt-git-branch)\[\e[$[COLUMNS]D\]${END}"
else
    export PS1_GIT_BRANCH=
fi
HABA="\[\e[$[COLUMNS]D\]\[\e[$[COLUMNS-$(length "hogehoge")]C\]"
# PS1=" ${BRACK}BRACK\n ${LIGHTGRAY}LIGHTGRAY\n ${DARKGRAY}DARKGRAY\n ${GREEN}GREEN\n ${LIGHTGREEN}LIGHTGREEN\n ${BLUE}BLUE\n ${LIGHTBLUE}LIGHTBLUE\n ${CYAN}CYAN\n ${LIGHTCYAN}LIGHTCYAN\n ${RED}RED\n ${LIGHTRED}LIGHTRED\n ${PURPLE}PURPLE\n ${LIGHTPURPLE}LIGHTPURPLE\n ${YELLOW}YELLOW\n ${WHITE}WHITE\n "
PS1="-${LIGHTGREEN}\u${END}@${GREEN}\h\[${END} ${YELLOW}\w\[${END} ${DARKGRAY}[\T]${END}${PS1_GIT_BRANCH}\n-${LIGHTPURPLE}(\!)${END}\$ "

export IGNOREEOF

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

alias ll='ls -l'
alias la='ls -la'

alias less='less -r'
alias ls='/bin/ls -F -G --color=tty --show-control-chars'

alias hst='history'

# 検索ワード色付け
export GREP_COLOR='1;3741'
alias grep='grep -E --color=auto'

# スパルタンVim
alias spvim='vim -u NONE'

alias gst="git status "

# lsのカラー化
export ls_colors='no=01;37:fi=00:di=01;36:pi=40;33:so=01;35:bd=40;33:cd=40;33;01:or=40;32;01:ex=01;33:*core=01;31:'
alias ls='ls -G'

if [ "$TERM" == xtrem ] ; then
    export TERN=xterm-color
fi

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

HISTFILESIZE=100000000000000
HISTSIZE=100000000000000

########################################
# cd
########################################
# ディレクトリ名に変数を指定できる
cdable_vars=on

# ctrl+sで出力がロックされてしまうのを防ぐ
stty stop undef