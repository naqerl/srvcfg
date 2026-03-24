set -ex

mkdir -p \
      $HOME/.emacs.d \
      $HOME/.config

ln -s $PWD/emacs/* $HOME/.emacs.d/ || true
ln -s $PWD/.bashrc $HOME/ || true
ln -s $PWD/agents $HOME/.config/ || true
ln -s $PWD/bin/* $HOME/.local/bin/ || true
