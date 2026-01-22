mkdir -p $HOME/.emacs.d $HOME/.config

ln -s $PWD/emacs/* $HOME/.emacs.d/
ln -s $PWD/tmux/tmux.conf $HOME/
ln -s $PWD/.bashrc $HOME/
ln -s $PWD/opencode $HOME/.config/
ln -s $PWD/bin/* $HOME/.local/bin/ 
