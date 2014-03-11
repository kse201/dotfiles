#!/bin/sh 

INSTALLER=$0
DIR=`pwd`
TARGET_FILES=`ls -1A | grep -v ${INSTALLER}`
RETVAL=0

dotfiles_backup() {
    BACKUP=$HOME/backup_`date +%Y%m%d_%H%M%S`.tar
    for file in ${TARGET_FILES} 
    do
        tar --remove-files -rf ${BACKUP} $HOME/${file} >/dev/null 2>&1
    done
}

dotfiles_install() {
    dotfiles_backup

    for file in ${TARGET_FILES} 
    do
        ln -s ${DIR}/${file} $HOME/${file} >/dev/null 2>&1
    done
    echo "(1/3): dotfiles installed."

    mkdir -p $HOME/.vim/bundle
    git clone https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim >/dev/null 2>&1
    if [ $? != 0 ] ; then
        echo "Error: failed git-clone neobundle.vim"
        RETVAL=1
        unlink $HOME/.vimrc.plugin
	return
    fi
    echo "(2/3): neobundle.vim installed."

    git clone https://github.com/Shougo/unite.vim $HOME/.vim/bundle/unite.vim >/dev/null 2>&1
    if [ $? != 0 ] ; then
        echo "Error: failed git-clone unite.vim"
        RETVAL=1
        unlink $HOME/.vimrc.plugin
	return
    fi
    echo "(3/3): unite.vim installed."

    vim -c "Unite neobundle/install"
}

dotfiles_uninstall(){
    for file in ${TARGET_FILES} 
    do
        unlink $HOME/${file} >/dev/null 2>&1
    done
    RETVAL=0
    echo "Succecc: uninstall dotfiles."
}

case "$1" in
    install)
        dotfiles_install
        ;;
    uninstall)
        dotfiles_uninstall
        ;;
    *)
        echo "Usage: $0 {install|uninstall}"
        RETVAL=1
esac
exit ${RETVAL}
