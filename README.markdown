Installation

    git clone git://github.com/hyunggyujang/dotfiles.git

Create required directories:

    mkdir -p ~/.config/nvim

Create symlinks:

    ln -s ~/dotfiles/init.vim ~/.config/nvim/init.vim
    ln -s ~/dotfiles/init.lua ~/.hammerspoon/init.lua
    ln -s ~/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json
    ln -s ~/dotfiles/vim ~/.local/share/nvim/

# VIM #

My preferences for Vim are stored in `dotfiles/init.vim`. 
All plugins and scripts are stored in the `dotfiles/vim`
directory.

# Hammerspoon & Karabiner

I'm using JIS keyboard layout with mac. And I'm korean so that have to use
korean keylayout too. I've got best keymapping configuration for my
environment by colaboration with Karabiner.
