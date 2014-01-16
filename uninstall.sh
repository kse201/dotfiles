#!/bin/sh
BACKUP=$HOME/backup-`date +%Y%m%d-%H%M%S`
mkdir ${BACKUP}
mv -fr  $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim $HOME/.zsh.d $HOME/.zshenv ${BACKUP} 2>/dev/null
