#!/bin/sh
set -eu

is_exist()  { [ -x "$(which "$1")" ]; }

DIR="${HOME}/.dotfiles"
IGNORES=(".gitignore" ".gitmodules")
RETVAL=0
REPOSITORY_URL="https://github.com/kse201/dotfiles"
dependencies="git make gcc"

dotfiles_download() {
    git clone "${REPOSITORY_URL}" "${DIR}"
}

vim_dependencies() {
    mkdir -p "$HOME/.vim/bundle"

    curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > /tmp/installer.sh
    sh /tmp/installer.sh "${HOME}/.vim/dein"
    if [ $? != 0 ] ; then
        echo "Error: failed installing dein.vim"
        RETVAL=1
        unlink "${HOME}/.vimrc.plugin"
        return
    fi
    echo "dein.vim installed."

    git clone https://github.com/Shougo/unite.vim "${HOME}/.vim/bundle/unite.vim" >/dev/null 2>&1
    if [ $? != 0 ] ; then
        echo "Error: failed git-clone unite.vim"
        RETVAL=1
        unlink "${HOME}/.vimrc.plugin"
        return
    fi
    echo "unite.vim installed."
}

dotfiles_install() {
    dotfiles_download

    dotfiles=($(find ${DIR} -maxdepth 1 -name "\.*" -type f | sed "s;${DIR}/;;g"))

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
        ln -f -s "${DIR}/${file}" "${HOME}/${file}" >/dev/null 2>&1
    done
    echo "dotfiles installed."

    cd "${DIR}"
    git submodule update --init
    echo "submodule installed"

    vim_dependencies
}

for dep in ${dependencies} ; do
    if ! is_exist "${dep}" ; then
        echo "Error: '${dep}' not found in ${PATH}"
        return 1
    fi
done

dotfiles_install

exit "${RETVAL}"
