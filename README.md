# .doom.d
😈 Doom Emacs configuration

### Prerequisite

``` shell
brew tap jimeh/emacs-builds
brew install --cask emacs-app
```

### Install

```shell
gh repo clone goofansu/.doom.d ~/.doom.d
cd ~/.doom.d && brew bundle

gh repo clone doomemacs/doomemacs ~/.emacs.d -- --depth 1
~/.emacs.d/bin/doom install
```
