#!/bin/sh

INSTALLER=$0
TARGET_FILES=`ls -1A | grep -v ${INSTALLER}`
RETVAL=0


dotfiles_backup() {
    BACKUP=$HOME/backup_`date_+%Y%m%d-%H%M%S`.tar
    for file in ${TARGET_FILES} 
    do
        tar --remove-files -rf ${BACKUP} $HOME/${file}
    done
}

dotfiles_install() {
    dotfiles_backup

    for file in ${TARGET_FILES} 
    do
        ln -s $HOME/.dotfiles/${file} $HOME/${file}
    done

    mkdir -p $HOME/.vim/bundle
    git clone https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim
    if [ $? != 0 ] ; then
        return -1
    fi

    git clone https://github.com/Shougo/unite.vim $HOME/.vim/bundle/unite.vim
    if [ $? != 0 ] ; then
        return -1
    fi

    ln -s $HOME/.dotfiles/.vimrc.plugin $HOME/.vimrc.plugin
    if [ $? != 0 ] ;then
        return -1
    fi
    vim -c "Unite neobundle/install"
}

dotfiles_uninstall(){
    for file in ${TARGET_FILES} 
    do
        unlink $HOME/${file}
    done
}

case "$1" in
    install)
        RETVAL = dotfiles_install
        ;;
    uninstall)
        RETVAL = dotfiles_uninstall
        ;;
    *)
        echo "Usage: $0 {install|uninstall}"
        RETVAL = 1
esac
exit ${RETVAL}
