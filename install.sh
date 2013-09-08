# tar cf dotfiles.tar $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim
# rm -r  $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim

BACKUP=$HOME/backup-`date %Y%m%d-%H%M%S`

mkdir ${BACKUP}
mv -r  $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim $HOME/.zsh.d $HOME/.zshenv ${BACKUP} 2>/dev/null

ln -s $HOME/dotfiles/.bashrc $HOME/.bashrc
ln -s $HOME/dotfiles/.zshrc  $HOME/.zshrc
ln -s $HOME/dotfiles/.vimrc  $HOME/.vimrc
ln -s $HOME/dotfiles/.gvimrc $HOME/.gvimrc
ln -s $HOME/dotfiles/.vim    $HOME/.vim
ln -s $HOME/dotfiles/.zsh.d  $HOME/.zsh.d
ln -s $HOME/dotfiles/.zshenv $HOME/.zshenv

if ! [ -e $HOME/.vimbackup ] ; then
    mkdir $HOME/.vimbackup
fi

mkdir -p $HOME/.vim/bundle
git clone https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim
if [ $? != 0 ] ; then
    exit -1
fi

ln -s $HOME/dotfiles/.vimrc.plugin $HOME/.vimrc.plugin
if [ $? != 0 ] ;then
    exit -1
fi
vim -c NeoBundleInstall
