#!/bin/sh

ln -s $HOME/.dotfiles/.bashrc $HOME/.bashrc
ln -s $HOME/.dotfiles/.zshrc  $HOME/.zshrc
ln -s $HOME/.dotfiles/.vimrc  $HOME/.vimrc
ln -s $HOME/.dotfiles/.gvimrc $HOME/.gvimrc
ln -s $HOME/.dotfiles/.vim    $HOME/.vim
ln -s $HOME/.dotfiles/.zsh.d  $HOME/.zsh.d
ln -s $HOME/.dotfiles/.zshenv $HOME/.zshenv
ln -s $HOME/.dotfiles/.gitconfig $HOME/.gitconfig

if ! [ -e $HOME/.vimbackup ] ; then
    mkdir $HOME/.vimbackup
fi

mkdir -p $HOME/.vim/bundle
git clone https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim
if [ $? != 0 ] ; then
    exit -1
fi

git clone https://github.com/Shougo/unite.vim $HOME/.vim/bundle/unite.vim
if [ $? != 0 ] ; then
    exit -1
fi

ln -s $HOME/.dotfiles/.vimrc.plugin $HOME/.vimrc.plugin
if [ $? != 0 ] ;then
    exit -1
fi
vim -c "Unite neobundle/install"
