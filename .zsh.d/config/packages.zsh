source ~/.zsh.d/package.zsh

# z 
package-install github rupa/z
_Z_CMD=j
source $(package-directory rupa/z)/z.sh
function precmd() {
_z --add "$(pwd -P)"
}

# auto-fu.zsh
# package-install github hchbaw/auto-fu.zsh
# source $(package-directory hchbaw/auto-fu.zsh)/auto-fu.zsh

    # function zle-line-init () {
    # auto-fu-init
# }
# zle -N zle-line-init
# # C-gやESCでアクティブな補完候補をキャンセル
# # http://d.hatena.ne.jp/tarao/20100531/1275322620
# function afu+cancel (){
# afu-clearing-maybe
# ((afu_in_p == 1)) && { afu_in_p=0; BUFFER="$buffer_cur" }
# }
# function bindkey-advice-before () {
    # local key="$1"
    # local advice="$2"
    # local widget="$3"
    # [[ -z "$widget" ]] && {
        # local -a bind
        # bind=(`bindkey -M main "$key"`)
        # widget=$bind[2]
    # }
    # local fun="$advice"
    # if [[ "$widget" != "undefined-key" ]]; then
        # local code=${"$(<=(cat <<"EOT"
            # function $advice-$widget () {
                # zle $advice
                # zle $widget
            # }
            # fun="$advice-$widget"
# EOT
        # ))"}
        # eval "${${${code//\$widget/$widget}//\$key/$key}//\$advice/$advice}"
    # fi
    # zle -N "$fun"
    # bindkey -M afu "$key" "$fun"
# }
# bindkey-advice-before "^G" afu+cancel
# bindkey-advice-before "^[" afu+cancel
# bindkey-advice-before "^J" afu+cancel afu+accept-line

function show_buffer_stack() {
POSTDISPLAY="
stack: $LBUFFER"
zle push-line-or-edit
}
zle -N show_buffer_stack

package-install github robbyrussell/oh-my-zsh
