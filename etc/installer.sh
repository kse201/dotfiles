#!/bin/sh
set -e
set -u

#is_exist()  { which "$1" >/dev/null 2>&1; return $?; }
is_exist()  { [ -x "$(which "$1")" ]; }

DIR="${HOME}/.dotfiles"
TARGET_FILES=".gitconfig .vimrc .vimrc.plugin .gvimrc .bashrc .zshrc .screenrc .tmuxrc"
RETVAL=0
ZIP_URL="https://github.com/kse201/.dotfiles/archive/master.zip"

dotfiles_download() {
    if is_exist 'git' ; then
        git clone https://github.com/kse201/.dotfiles "${DIR}"
    else
        curl -L -o "${HOME}/dotfiles.zip" "${ZIP_URL}"
        unzip "${HOME}/dotfiles.zip"
    fi
}

dotfiles_backup() {
    BACKUP="$HOME/backup_$(date +%Y%m%d_%H%M%S).tar"
    for file in ${TARGET_FILES}
    do
        tar --remove-files -rf "${BACKUP}" "${HOME}/${file}" >/dev/null 2>&1
    done
}

vim_dependencies() {
    mkdir -p "$HOME/.vim/bundle"
    git clone https://github.com/Shougo/neobundle.vim.git "${HOME}/.vim/bundle/neobundle.vim" >/dev/null 2>&1
    if [ $? != 0 ] ; then
        echo "Error: failed git-clone neobundle.vim"
        RETVAL=1
        unlink "$HOME/.vimrc.plugin"
        return
    fi
    echo "neobundle.vim installed."

    git clone https://github.com/Shougo/unite.vim "$HOME/.vim/bundle/unite.vim" >/dev/null 2>&1
    if [ $? != 0 ] ; then
        echo "Error: failed git-clone unite.vim"
        RETVAL=1
        unlink "$HOME/.vimrc.plugin"
        return
    fi
    echo "unite.vim installed."

    if is_exist "vim" ; then
        vim -c "NeoBundleInstall"
    fi
}

dotfiles_install() {
    dotfiles_download
    dotfiles_backup

    for file in ${TARGET_FILES}
    do
        ln -s "${DIR}/${file}" "${HOME}/${file}" >/dev/null 2>&1
    done
    echo "dotfiles installed."

    git submodule init
    git submodule update
    echo "submodule installed"

    vim_dependencies
}

dotfiles_install

exit "${RETVAL}"
