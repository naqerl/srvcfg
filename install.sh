mkdir -p $HOME/.emacs.d $HOME/.config $HOME/.tmux/scripts

ln -s $PWD/emacs/* $HOME/.emacs.d/
ln -s $PWD/tmux/tmux.conf $HOME/
ln -s $PWD/tmux/bell-attention.conf $HOME/.tmux/
ln -s $PWD/tmux/scripts/* $HOME/.tmux/scripts/
ln -s $PWD/.bashrc $HOME/
ln -s $PWD/.bashrc $HOME/
ln -s $PWD/opencode $HOME/.config/
ln -s $PWD/agents $HOME/.config/
ln -s $PWD/bin/* $HOME/.local/bin/

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
