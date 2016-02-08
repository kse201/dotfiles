#!/bin/sh -eu
USAGE="$(basename $0)
install my dotfiles
"

is_successful(){ [ "$1" == ${SUCCESS} ]; }
is_failed()    { [ "$1" != ${SUCCESS} ]; }
is_exist()  { [ -x "$(which "$1")" ]; }

SUCCESS=0
E_FAILED=1

DOTFILE_DIR="${HOME}/.dotfiles"
IGNORES=(".gitignore" ".gitmodules")
REPOSITORY_URL="https://github.com/kse201/dotfiles"

DEP_CMDS=("git" "make" "gcc")
DEP_VIMPLUGINS=("https://github.com/Shougo/neobundle.vim" "https://github.com/Shougo/unite.vim")

LOG_ERROR=0
LOG_INFO=1
LOG_DEBUG=2

if [ ! -v "LOG_LEVEL" ] ; then
    LOG_LEVEL=${LOG_ERROR}
fi

############################################################
# Logger function
#   args: message
############################################################
_log() {
    log_level=$1; shift

    if [ ${log_level} -le ${LOG_LEVEL} ] ; then
        cat >&2 <<EOL
$@
EOL
    fi
}

log_error() {
    _log ${LOG_ERROR} "ERROR: $@"
}

log_info() {
    _log ${LOG_INFO} "INFO : $@"
}

log_debug() {
    _log ${LOG_DEBUG} "DEBUG: $@"
}

check_depends() {
    for dep in ${DEPS_CMDS[@]} ; do
        if ! is_exist "${dep}" ; then
            log_error "'${dep}' not found in ${PATH}"
            return ${E_FAILED}
        fi
    done

    return ${SUCCESS}
}

download_dotfiles() {
    git clone "${REPOSITORY_URL}" "${DOTFILE_DIR}"
    return ${SUCCESS}
}

download_vim_depends() {
    mkdir -p "${HOME}/.vim/bundle"

    for depends in ${DEP_VIMPLUGINS[@]} ; do
        depends_name=$(basename ${depends})
        git clone ${depends} "${HOME}/.vim/bundle/${depends_name}" >/dev/null 2>&1
        if is_failed $? ; then
            log_error "failed git-clone ${base}"
            unlink "${HOME}/.vimrc.plugin"
            log_info "download_vim_depends failed"
            return ${E_FAILED}
        fi
        log_debug "${depends_name} installed."
    done

    log_info "download_vim_depends success"
    return ${SUCCESS}
}

install_dotfiles() {
    dotfiles=($(find ${DOTFILE_DIR} -maxdepth 1 -name "\.*" -type f | sed "s;${DOTFILE_DIR}/;;g"))

    set +u
    for i in $(seq ${#dotfiles[@]}) ; do 
        file=${dotfiles[${i}]}
        for ignore in ${IGNORES[@]} ; do
            if [ "${file}" = "${ignore}" ] ; then 
                unset dotfiles[${i}]
            fi
        done
    done
    set -u

    for file in ${dotfiles[@]} ; do
        ln -f -s "${DOTFILE_DIR}/${file}" "${HOME}/${file}" >/dev/null 2>&1
        if is_failed $? ; then 
            log_debug "failed ln -s ${file}"
        fi
    done
    log_debug "dotfiles installed."

    cd "${DOTFILE_DIR}"
    git submodule update --init
    log_debug "submodule installed"

    log_info "install_dotfiles success"
    return ${SUCCESS}
}

main(){
    if [ $# -gt 0 ] ; then
        cat >&2 <<EOU
Usage: ${USAGE}
EOU
        return ${E_INTERNAL}
    fi

    check_depends || return ${E_FAILED}
    download_dotfiles || return ${E_FAILED}
    install_dotfiles || return ${E_FAILED}
    download_vim_depends || return ${E_FAILED}

    exit "${SUCCESS}"
}

main $@

