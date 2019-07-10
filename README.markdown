Installation

    git clone git://github.com/hyunggyujang/dotfiles.git

Create required directories:

    mkdir -p ~/.config/nvim
    mkdir -p ~/tmp

Create symlinks:

    ln -s ~/dotfiles/bashrc ~/.bashrc
    ln -s ~/dotfiles/vimrc ~/.vimrc
    ln -s ~/dotfiles/vim ~/.vim
    ln -s ~/dotfiles/ctags ~/.ctags
    ln -s ~/dotfiles/gitconfig ~/.gitconfig
    ln -s ~/dotfiles/global-gitignore ~/.gitignore
    ln -s ~/dotfiles/init.vim ~/.config/nvim/init.vim

# VIM #

My preferences for Vim are stored in `dotfiles/init.vim`. 
All plugins and scripts are stored in the `dotfiles/vim`
directory.
