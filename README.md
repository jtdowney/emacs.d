# emacs.d

I've spent the last decade using vim and tmux on an almost daily basis but have been trying to switch to emacs for the extensibility it brings. Due to my vim past, this configuration uses evil-mode as to not cause a big disruption to my fingers.

This emacs configuration uses org mode with babel for a literate programming style and to organize the code. You can find the bulk of the configuration in [config.org](config.org).

## Installation

### Emacs on macOS

I use the railwaycat fork of emacs for macOS. You can install it with homebrew.

``` shell
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-natural-title-bar
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications
```

### Configuration

``` shell
git clone https://github.com/jtdowney/emacs.d.git ~/.emacs.d
```

## Credit

I took inspiration for my configuration from multiple places. Thanks and credit goes to them for what they've done.

- [Aaron Bedra's dotfiles](http://aaronbedra.com/emacs.d)
- [Castlemacs](https://github.com/freetonik/castlemacs)
- [Harry R. Schwartz's dotfiles](https://github.com/hrs/dotfiles)
- [Spacemacs](http://spacemacs.org/)
