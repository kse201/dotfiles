#!/bin/sh
set -e
set -u

is_exist()  { [ -x "$(which "$1")" ]; }

DIR="${HOME}/.dotfiles"
TARGET_FILES=".gitconfig .vimrc .vimrc.plugin .bashrc .zshrc .screenrc .tmuxrc .vim"
RETVAL=0
REPOSITORY_URL="https://github.com/kse201/dotfiles"

dotfiles_download() {
    git clone "${REPOSITORY_URL}" "${DIR}"
}

vim_dependencies() {
    mkdir -p "$HOME/.vim/bundle"
    git clone https://github.com/Shougo/neobundle.vim.git "${HOME}/.vim/bundle/neobundle.vim" >/dev/null 2>&1
    if [ $? != 0 ] ; then
        echo "Error: failed git-clone neobundle.vim"
        RETVAL=1
        unlink "${HOME}/.vimrc.plugin"
        return
    fi
    echo "neobundle.vim installed."

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

    for file in ${TARGET_FILES} ; do
        ln -f -s "${DIR}/${file}" "${HOME}/${file}" >/dev/null 2>&1
    done
    echo "dotfiles installed."

    cd "${DIR}"
    git submodule update --init
    echo "submodule installed"

    vim_dependencies
}


cmds="git make"
for cmd in ${cmds} ; do
    if ! is_exist ${cmd} ; then
        echo "Error: '${cmd}' not found in ${PATH}"
        return 1
    fi
done

dotfiles_install

exit "${RETVAL}"
