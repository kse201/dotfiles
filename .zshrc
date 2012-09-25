export EDITOR=vim
export PATH=$PATH:~/myshellscript:/opt/local/:~/local/bin/:~/git-tasukete/
#LANG
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

#履歴
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt hist_reduce_blanks #スペース排除
setopt EXTENDED_HISTORY #zshの開始終了を記録
unsetopt hist_verify # ヒストリを呼び出してから実行する間いｎ一旦編集可能を止める
setopt hist_expand # 補完時にヒストリを自動的に展開 
export HISTIGNORE="ls:cd:history:fg*" # よく使うコマンドを保存しない
HISTTIMEFORMAT='%Y%m%d %T';
export HISTTIMEFORMAT

#補完機能の強化
autoload -U compinit
compinit -u

#コアダンプサイズを制限
limit coredumpsize 102400
##出力の文字列末尾に改行コードがない場合でも表示
unsetopt promptcr

# emacs キーバインド
bindkey -e

# プロンプト
PS1="[@${HOST%%.*} %2~]%(!.#.$) "
#時間表示 & 入力に応じて消す
RPROMPT="%T"
setopt transient_rprompt

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

# ディレクトリ名だけでcd
setopt auto_cd

# cdの履歴を表示
setopt auto_pushd
setopt pushd_ignore_dups #同ディレクトリを履歴に追加しない
setopt pushd_minus

# スペルチェック
setopt auto_param_keys

# リストを詰めて表示
setopt list_packed

# history周り
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# eval `dircolors $HOME/.dir_colors`
# lsのカラー化
export ls_colors='no=01;37:fi=00:di=01;36:pi=40;33:so=01;35:bd=40;33:cd=40;33;01:or=40;32;01:ex=01;33:*core=01;31:'
alias ls="ls -G"
# -i 確認 -v 詳細な情報の表示
alias cp='cp -iv'
# alias rm='rm -iv'
alias mv='mv -iv'
alias ll='ls -l'
alias la='ls -la'
# 検索ワード色付け
export GREP_COLOR='1;3741'
alias grep='grep -E --color=auto'

# スパルタンVim
alias spvim='vim -u NONE'

[[ $EMACS = t ]] && unsetopt zle

zstyle ':completion:*:default' menu select=1

alias gst="git status "

# 単語区切り記号
WORDCHARS='*?_-.[]~=&;!#S%^(){}<>'

# less オプションを環境変数で指定する
# export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-if-one-screen --RAW-CONTROL-CHARS'

# カレントディレクトリ内にサブディレクトリがない場合にcdが検索するディレクトリのリスト
cdpath=($HOME)

# 複数行入力時のプロンプト
PROMPT2="%_%%"
#入力ミス確認時のプロンプト
SPROMPT="correct> %R -> %r [n,y,a,e]?"

# サスペンド中のプロセスと同じコマンド名を実行した場合はリジュームする
setopt auto_resume
# C-d でログアウトしない
setopt ignore_eof
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

# z 
_Z_CMD=j
source ~/z/z.sh
function premod() {
    z --add "$(pwd -P)"
}

source ~/auto-fu.zsh/auto-fu.zsh
alias ctags='/Applications/MacVim.app/Contents/MacOS/ctags "$@"'
# alias cancel = !git commit -a -m'Temporary commit for cancel' && git reset--hard HEAD~

# auto-fu.zsh
if [ -f ~/.zsh/auto-fu.zsh/auto-fu.zsh ] ; then
    source ~/.zsh/auto-fu.zsh/auto-fu.zsh
    function zle-line-init () {
        auto-fu-init
    }
    zle -N zle-line-init
fi

function show_buffer_stack() {
POSTDISPLAY="
stack: $LBUFFER"
zle push-line-or-edit
}
zle -N show_buffer_stack
