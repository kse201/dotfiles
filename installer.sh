#!/bin/sh

RETVAL=0

dotfiles_install() {
    ln -s $HOME/.dotfiles/.bashrc $HOME/.bashrc
    ln -s $HOME/.dotfiles/.zshrc  $HOME/.zshrc
    ln -s $HOME/.dotfiles/.vimrc  $HOME/.vimrc
    ln -s $HOME/.dotfiles/.gvimrc $HOME/.gvimrc
    ln -s $HOME/.dotfiles/.vim    $HOME/.vim
    ln -s $HOME/.dotfiles/.zsh.d  $HOME/.zsh.d
    ln -s $HOME/.dotfiles/.zshenv $HOME/.zshenv
    ln -s $HOME/.dotfiles/.gitconfig $HOME/.gitconfig

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

dotfiles_uninstall() {
    BACKUP=$HOME/backup-`date +%Y%m%d-%H%M%S`
    mkdir ${BACKUP}
    mv -fr  $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim $HOME/.zsh.d $HOME/.zshenv ${BACKUP} 2>/dev/null
}

case "$1" in
    install)
        ${RETVAL} = dotfiles_install
        ;;
    uninstall)
        ${RETVAL} = dotfiles_uninstall
        ;;
    *)
    echo "Usage: $0 {install|uninstall}"
    ${RETVAL} = 1
    return ${RETVAL}
esac
