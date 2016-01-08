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

# ヒストリ
HISTFILE=$HOME/.zsh_history
HISTSIZE=7500
SAVEHIST=7500
setopt hist_reduce_blanks # remove space
setopt extended_history #zshの開始,終了時刻を記録
unsetopt hist_verify # ヒストリを呼び出してから実行する間いｎ一旦編集可能を止める
setopt hist_expand # complete from history
export HISTIGNORE="ls *:cd:history:fg*:history-all" # ignored commands
setopt hist_ignore_space # ignore space-start command
setopt hist_expire_dups_first
setopt inc_append_history # すぐにヒストリに追記する
setopt share_history # zshプロセス間でヒストリを共有する
function history-all { history -E 1 } # output all histoy

# 補完機能の強化
autoload -U compinit
compinit -u
## competion method grouping
zstyle ':completion:*' format '%B%d%b'
zstyle ':completion:*' group-name ''

## 補完候補をメニューから選択
zstyle ':completion:*:default' menu select=2
## color completion
## 空文字列はデフォルト値を使うという意味
zstyle ':completion:*:default' list-colors ""
## 補完候補がなければより曖昧に候補を探す
## m:{a-z}={A-Z} : 小文字大文字区別なく補完
## r:|[._-]=* [.][_][-]の前にワイルドカードがあるものとして補完
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'
## 多めに保管方法をとる
# completer
# _oldlist 前回の補完結果を再利用
# _complete 補完する
# _match globを展開しないで候補の一覧から補完
# _history ヒストリのコマンドも補完候補とする
# _ignored 補完候補にださないと指定したものも補完候補とする
# _approximate 似ている補完候補も補完候補とする
# _prefix カーソル移行を無視してカーソル位置までで補完する

zstyle ':completion:*' completer _oldlist _complete _match _history _ignored _approximate _prefix _list
## 補完候補をキャッシュ
zstyle ':completion:*' use-cache yes
## 詳細な情報を使う
zstyle ':completion:*' verbose yes
## カーソル位置で補完
setopt complete_in_word
## globを展開しないで候補の一覧から補完する
setopt glob_complete
## 数字順に並べる
setopt numeric_glob_sort

# 展開
## =の後でも ~ [=コマンド]などのファイル名展開を行う
setopt magic_equal_subst
## パスがディレクトリだったら最後に/をつけるo
setopt mark_dirs

## jobsでプロセスIDも出力
setopt long_list_jobs

## プロセス消費時間が3秒かかったら 自動的に消費時間の統計情報を表示
REPORTTIME=3

## 全てのユーザのログイン・ログアウトを監視
watch="all"
## ログイン語すぐに表示
log

#コアダンプサイズを制限
limit coredumpsize 102400
##出力の文字列末尾に改行コードがない場合でも表示
unsetopt promptcr

# emacs-like keybind
bindkey -e

########################################
# prompt
########################################
# PS1="[@${HOST%%.*} %2~]%(!.#.$) "
setopt transient_rprompt
setopt prompt_subst
setopt prompt_percent
setopt transient_rprompt

color256()
{
    local red=$1; shift
    local green=$2; shift
    local blue=$3; shift

    echo -n $[$red * 36 + $green * 6 + $blue + 16]
}

fg256()
{
    echo -n $'\e[38;5;'$(color256 "$@")"m"
}

bg256()
{
    echo -n $'\e[48;5;'$(color256 "$@")"m"
}

autoload -Uz vcs_info
zstyle ':vcs_info:*' formats \
    '(%{%F{blue}%}%b%{%f%})'
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
# SPROMPT="correct> %R -> %r [n,y,a,e]?"
SPROMPT="%{$fg[red]%}%{$suggest%}(*'_'%)? < You mean %B%r%b %{$fg[red]%}? [y,n,a,e]:${reset_color} "

########################################

# 色を使う
setopt prompt_subst

# no beep
setopt nobeep

# 補完候補一覧でファイル種別を表示
setopt list_types

# ファイル名で#,~,^を正規表現として扱う
setopt extended_glob

# 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups

# 補完リストが多い時に尋ねる数
LISTMAX=0
export LISTMAX

# cd dir_name only
setopt auto_cd auto_remove_slash auto_name_dirs

setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split autopushd pushd_ignore_dups

# cd history
setopt auto_pushd
setopt pushd_ignore_dups #同ディレクトリを履歴に追加しない
setopt pushd_minus
cdpath=(~) # カレントディレクトリ内に指定ディレクトリが見当たらない場合移動先を検索するリスト
chpwd_functions=($chpwd_functions dirs)

# spell check
setopt auto_param_keys

# リストを詰めて表示
setopt list_packed

# history
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^P' history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

# eval `dircolors $HOME/.dir_colors`
# color ls
export ls_colors='no=01;37:fi=00:di=01;36:pi=40;33:so=01;35:bd=40;33:cd=40;33;01:or=40;32;01:ex=01;33:*core=01;31:'
os=$(uname)
if [ ${os} = "Darwin" ] ; then
        alias ls="ls -G"
else
        alias ls="ls --color=auto"
fi
# -i 確認 -v 詳細な情報の表示
alias cp='cp -iv'
# alias rm='rm -iv'
alias mv='mv -iv'
alias grep='grep -E --color=auto'
alias ll='ls -l'
alias la='ls -la'
# color grep word
export GREP_COLOR='1;3741'

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

# 単語区切り記号
WORDCHARS='*?_-.[]~=&;!#S%^(){}<>'
WORDCHARS=${WORDCHARS:s,/,,}

# カレントディレクトリ内にサブディレクトリがない場合にcdが検索するディレクトリのリスト
cdpath=($HOME)

# サスペンド中のプロセスと同じコマンド名を実行した場合はリジュームする
setopt auto_resume
# コマンドのスペルチェックをする
setopt correct
# 補完候補が複数ある場合、一覧表示せず、すぐ最初の候補を補完する
# vimshell 上で邪魔なので無効化
setopt no_menu_complete
# 補完候補の表示を水平方向に
setopt list_rows_first
#  コピペ時rpromptを非表示にする
setopt transient_rprompt
# 括弧の対応を自動補完
setopt auto_param_keys
# 補完される前にオリジナルのコマンドまで展開してチェックされる
setopt complete_aliases

## エイリアス
### suffix
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

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

## 完全に削除
alias rr="command rm -rf"

alias remem='du -sx / &> /dev/null & sleep 25 && kill $!'

# 今いるディレクトリを補完候補から外す
#http://qiita.com/items/7916037b1384d253b457
zstyle ':completion:*' ignore-parents parent pwd ..

# クリップボードにコピー
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

if is_exist 'tree' ; then
    alias tree="pwd;find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'"
fi

########################################
# man
if is_exist 'tldr' ; then
    alias man='tldr'
fi
########################################

########################################
# packages
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

########################################
conf() {
    case $1 in
        bash)       vim $HOME/.bashrc ;;
        git)        vim $HOME/.gitconfig ;;
        tmux)       vim $HOME/.tmux.conf ;;
        screen)     vim $HOME/.screenrc ;;
        vim)        vim $HOME/.vimrc ;;
        zsh)        vim $HOME/.zshrc && source $HOME/.zshrc ;;
        *)          echo "Unknown application: $1" ;;
    esac
}

reload (){
    exec $SHELL
}

# screen
export SCREENDIR=$HOME/.screens
if [ ! -e "$HOME/.log" ] ; then
    mkdir "$HOME/.log"
fi

# local setting
if [ -f "$HOME/.zshrc.local" ] ; then
    source "$HOME/.zshrc.local"
fi

function timestamp() {
date +%Y%m%d%H%M%S
}

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

if [ -e "$HOME/proxyrc" ] ; then
    source "$HOME/proxyrc"
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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
