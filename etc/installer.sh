#!/bin/sh
set -e
set -u

#is_exist()  { which "$1" >/dev/null 2>&1; return $?; }
is_exist()  { [ -x "$(which "$1")" ]; }

DIR="${HOME}/.dotfiles"
TARGET_FILES=".gitconfig .vimrc .vimrc.plugin .bashrc .zshrc .zsh.d .screenrc .tmuxrc .vim"
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

    if is_exist "vim" ; then
        vim -c "NeoBundleInstall"
    fi
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

if ! is_exist 'git' ; then
    echo "Error: 'git' not found in ${PATH}"
    return 1
fi
dotfiles_install

exit "${RETVAL}"
