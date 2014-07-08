## 大いに参考させて頂きました(というかパクリ)
# http://www.clear-code.com/blog/2011/9/5.html

if [ ! -f ~/.zshrc.zwc -o ~/.zshrc -nt ~/.zshrc.zwc ]; then
    zcompile ~/.zshrc
fi
# env
export PATH=/usr/local/bin:/usr/local/sbin:$PATH:~/bin 
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
setopt EXTENDED_HISTORY #zshの開始,終了時刻を記録
unsetopt hist_verify # ヒストリを呼び出してから実行する間いｎ一旦編集可能を止める
setopt hist_expand # complete from history
export HISTIGNORE="ls *:cd:history:fg*:history-all" # ignored commands
setopt hist_ignore_space # ignore space-start command
setopt HIST_EXPIRE_DUPS_FIRST
setopt inc_append_history # すぐにヒストリに追記する
setopt share_history # zshプロセス間でヒストリを共有する
function history-all { history -E 1 } # output all histoy
HISTTIMEFORMAT='%Y%m%d %T';
export HISTTIMEFORMAT

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
PS1="[@${HOST%%.*} %2~]%(!.#.$) "
setopt transient_rprompt
## PROMPT内で変数展開・コマンド置換・算術演算を実行する。
setopt prompt_subst
## PROMPT内で「%」文字から始まる置換機能を有効にする。
setopt prompt_percent
## コピペしやすいようにコマンド実行後は右プロンプトを消す。
setopt transient_rprompt

## 256色生成用便利関数
### red: 0-5
### green: 0-5
### blue: 0-5
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

## プロンプトの作成
### ↓のようにする。
###   -(user@debian)-(0)-<2011/09/01 00:54>--------------------[/home/user]-
###   -[84](0)%                                                         [~]

## バージョン管理システムの情報も表示する
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats \
    '(%{%F{white}%K{green}%}%s%{%f%k%})-[%{%F{white}%K{blue}%}%b%{%f%k%}]'
zstyle ':vcs_info:*' actionformats \
    '(%{%F{white}%K{green}%}%s%{%f%k%})-[%{%F{white}%K{blue}%}%b%{%f%k%}|%{%F{white}%K{red}%}%a%{%f%k%}]'

### プロンプトバーの左側
###   %{%B%}...%{%b%}: 「...」を太字にする。
###   %{%F{cyan}%}...%{%f%}: 「...」をシアン色の文字にする。
###   %n: ユーザ名
###   %m: ホスト名（完全なホスト名ではなくて短いホスト名）
###   %{%B%F{white}%(?.%K{green}.%K{red})%}%?%{%f%k%b%}:
###                           最後に実行したコマンドが正常終了していれば
###                           太字で白文字で緑背景にして異常終了していれば
###                           太字で白文字で赤背景にする。
###   %{%F{white}%}: 白文字にする。
###     %(x.true-text.false-text): xが真のときはtrue-textになり
###                                偽のときはfalse-textになる。
###       ?: 最後に実行したコマンドの終了ステータスが0のときに真になる。
###       %K{green}: 緑景色にする。
###       %K{red}: 赤景色を赤にする。
###   %?: 最後に実行したコマンドの終了ステータス
###   %{%k%}: 背景色を元に戻す。
###   %{%f%}: 文字の色を元に戻す。
###   %{%b%}: 太字を元に戻す。
###   %D{%Y/%m/%d %H:%M}: 日付。「年/月/日 時:分」というフォーマット。
prompt_bar_left_self="%{%B%}%F{cyan}%n%{%b%}@%{%B%}%F{cyan}%m%{%b%}"
prompt_bar_left_status="(%{%B%F{white}%(?.%K{green}.%K{red})%}%?%{%k%f%b%})"
prompt_bar_left_date="[%{%B%}%F{white}%D{%y/%m/%d %H:%M}%{%f%b%}]"
prompt_bar_left="${prompt_bar_left_status} ${prompt_bar_left_self}${prompt_bar_left_date}"
### プロンプトバーの右側
###   %{%B%K{magenta}%F{white}%}...%{%f%k%b%}:
###       「...」を太字のマジェンタ背景の白文字にする。
###   %d: カレントディレクトリのフルパス（省略しない）
prompt_bar_right="[%{%B%K{magenta}%F{white}%}%~%{%f%k%b%}] "

### 2行目左にでるプロンプト。
###   %h: ヒストリ数。
###   %(1j,(%j),): 実行中のジョブ数が1つ以上ある場合だけ「(%j)」を表示。
###     %j: 実行中のジョブ数。
###   %{%B%}...%{%b%}: 「...」を太字にする。
###   %#: 一般ユーザなら「%」、rootユーザなら「#」になる。
prompt_left="$([ -n "$TMUX" ] && tmux display -p "#I-#P ")[%h]%(1j,(%j),)%{%B%}%# %{%b%}"

## プロンプトフォーマットを展開した後の文字数を返す。
## 日本語未対応。
count_prompt_characters()
{
    # print:
    #   -P: プロンプトフォーマットを展開する。
    #   -n: 改行をつけない。
    # sed:
    #   -e $'s/\e\[[0-9;]*m//g': ANSIエスケープシーケンスを削除。
    # sed:
    #   -e 's/ //g': *BSDやMac OS Xのwcは数字の前に空白を出力するので削除する。
    print -n -P -- "$1" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'
}

## プロンプトを更新する。
update_prompt()
{
    # プロンプトバーの左側の文字数を数える。
    # 左側では最後に実行したコマンドの終了ステータスを使って
    # いるのでこれは一番最初に実行しなければいけない。そうし
    # ないと、最後に実行したコマンドの終了ステータスが消えて
    # しまう。
    local bar_left_length=$(count_prompt_characters "$prompt_bar_left")
    # プロンプトバーに使える残り文字を計算する。
    # $COLUMNSにはターミナルの横幅が入っている。
    local bar_rest_length=$[COLUMNS - bar_left_length]

    local bar_left="$prompt_bar_left"
    # パスに展開される「%d」を削除。
    local bar_right_without_path="${prompt_bar_right:s/%d//}"
    # 「%d」を抜いた文字数を計算する。
    local bar_right_without_path_length=$(count_prompt_characters "$bar_right_without_path")
    # パスの最大長を計算する。
    #   $[...]: 「...」を算術演算した結果で展開する。
    local max_path_length=$[bar_rest_length - bar_right_without_path_length]
    # パスに展開される「%d」に最大文字数制限をつける。
    #   %d -> %(C,%${max_path_length}<...<%d%<<,)
    #     %(x,true-text,false-text):
    #         xが真のときはtrue-textになり偽のときはfalse-textになる。
    #         ここでは、「%N<...<%d%<<」の効果をこの範囲だけに限定させる
    #         ために用いているだけなので、xは必ず真になる条件を指定している。
    #       C: 現在の絶対パスが/以下にあると真。なので必ず真になる。
    #       %${max_path_length}<...<%d%<<:
    #          「%d」が「${max_path_length}」カラムより長かったら、
    #          長い分を削除して「...」にする。最終的に「...」も含めて
    #          「${max_path_length}」カラムより長くなることはない。
    bar_right=${prompt_bar_right:s/%d/%(C,%${max_path_length}<...<%d%<<,)/}
    # 「${bar_rest_length}」文字分の「-」を作っている。
    # どうせ後で切り詰めるので十分に長い文字列を作っているだけ。
    # 文字数はざっくり。
    local separator="${(l:${bar_rest_length}:: :)}"
    # プロンプトバー全体を「${bar_rest_length}」カラム分にする。
    #   %${bar_rest_length}<<...%<<:
    #     「...」を最大で「${bar_rest_length}」カラムにする。
    bar_right="%${bar_rest_length}<<${separator}${bar_right}%<<"

    # プロンプトバーと左プロンプトを設定
    #   "${bar_left}${bar_right}": プロンプトバー
    #   $'\n': 改行
    #   "${prompt_left}": 2行目左のプロンプト
    PROMPT="${bar_left}${bar_right}"$'\n'"${prompt_left}"
    # 右プロンプト
    #   %{%B%F{white}%K{green}}...%{%k%f%b%}:
    #       「...」を太字で緑背景の白文字にする。
    #   %~: カレントディレクトリのフルパス（可能なら「~」で省略する）
    # RPROMPT="[%{%B%F{white}%K{magenta}%}%~%{%k%f%b%}]"

    # バージョン管理システムの情報を取得する。
    LANG=C vcs_info >&/dev/null
    # バージョン管理システムの情報があったら右プロンプトに表示する。
    if [ -n "$vcs_info_msg_0_" ]; then
        RPROMPT="${vcs_info_msg_0_}"
    else 
        RPROMPT=""
    fi
}

## コマンド実行前に呼び出されるフック。
precmd_functions=($precmd_functions update_prompt)

# 複数行入力時のプロンプト
PROMPT2="%_%%"
# 入力ミス確認時のプロンプト
SPROMPT="correct> %R -> %r [n,y,a,e]?"
# suggest prompt
SPROMPT="%{$fg[red]%}%{$suggest%}(*'_'%)? < もしかして %B%r%b %{$fg[red]%}かな? [そう!(y), 違う!(n),a,e]:${reset_color} "

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
case `uname` in
    "SunOS")
        ;;
    "Darwin") # BSD ls
        alias ls="ls -G"
        ;;
    *) # GNU ls
        alias ls="ls --color=auto"
        ;;
esac
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
which vim > /dev/null 2>/dev/null
if [ $? = 0 ] ; then
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

function git_commit() {
        BUFFER="git commit -m '"
        CURSOR=$#BUFFER
        BUFFER=$BUFFER\'
}
zle -N git_commit
bindkey '^o' git_commit

## 完全に削除
alias rr="command rm -rf"

alias remem='du -sx / &> /dev/null & sleep 25 && kill $!'

# 今いるディレクトリを補完候補から外す
#http://qiita.com/items/7916037b1384d253b457
zstyle ':completion:*' ignore-parents parent pwd ..

## create emacs env file
if [ -f ~/.emacs.d ] ; then
    perl -wle \
    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
    PATH > ~/.emacs.d/shellenv.el
fi

# クリップボードにコピー
if which pbcopy >/dev/null 2>&1 ; then 
    # Mac  
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then 
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then 
    # Cygwin 
    alias -g C='| putclip'
fi

########################################
# man
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
# man in Vim
function man() { /usr/bin/man $* -P "col -b | vim -Rc 'setl ft=man ts=8 nomod nolist nonu' -c 'nmap q :q<cr>' -" }   
########################################

########################################
# packages
PLUGIN_CONFIG=~/.zsh.d/config/antigen.conf
if [ -e ${PLUGIN_CONFIG} ] ; then
    source ${PLUGIN_CONFIG}
fi
########################################

########################################
# tmux
function ssh() {
    local window_name=$(tmux display -p '#W')
    command ssh $@
    tmux rename-window ${window_name}
}
########################################
conf() {
    case $1 in
        bash)       vim ~/.bashrc ;;
        git)       vim ~/.gitconfig ;;
        tmux)       vim ~/.tmux.conf ;;
        screen)     vim ~/.screenrc ;;
        vim)        vim ~/.vimrc ;;
        zsh)        vim ~/.zshrc && source ~/.zshrc ;;
        *)          echo "Unknown application: $1" ;;
    esac
}

reload (){
    exec $SHELL
}

# screen
export SCREENDIR=~/.screens

# local setting
if [ -f ~/.zshrc.local ] ; then
    source ~/.zshrc.local
fi

function timestamp() {
date +%Y%m%d%H%M%S
}
