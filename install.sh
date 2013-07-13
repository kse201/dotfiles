tar cf dotfiles.tar $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim
rm -r  $HOME/.bashrc $HOME/.zshrc $HOME/.vimrc $HOME/.gvimrc $HOME/.vimrc.plugin $HOME/.vim

ln -s $HOME/dotfiles/.bashrc $HOME/.bashrc
ln -s $HOME/dotfiles/.zshrc  $HOME/.zshrc
ln -s $HOME/dotfiles/.vimrc  $HOME/.vimrc
ln -s $HOME/dotfiles/.gvimrc $HOME/.gvimrc
ln -s $HOME/dotfiles/.vim    $HOME/.vim

if ! [ -f $HOME/.vimbackup ] ; then
    mkdir $HOME/.vimbackup
fi

mkdir -p $HOME/dotfiles/bundle
git clone https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim

if [ $? != 0 ] ; then
    exit -1
fi

ln -s $HOME/dotfiles/.vimrc.plugin $HOME/.vimrc.plugin

if [ $? != 0 ] ;then
    exit -1
fi
vim -c NeoBundleInstall
