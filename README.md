# Adequate emacs.d 🐘

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](LICENSE)

> This is my attempt at configuring Emacs the way I like it. Eternally work in
> progress.

Practical Emacs setup with Vim-ish influences and affection for
[Hydras](https://github.com/abo-abo/hydra).

# Install

Unlike most Emacs configurations packages, Adequate emacs.d should not be
installed as the `~/.emacs.d` directory. Instead, if you wish to use Adequate,
you should make `~/.emacs` to be a symbolic link to the projects `init.el`. The
initialization code will figure out where Adequate is installed and add the
appropriate directories to Emacs load path.

```
git clone https://bitbucket.org/Soft/emacs.d.git
ln -s ./emacs.d/init.el ~/.emacs
```

Not having Adequate's project directory be user's `~/.emacs.d` frees us from
having to craft a detailed `.gitignore` for all the files Emacs might create
inside its init directory. Additionally, having Adequate be in a separate
location helps us keep the project files clean and reduces the possibility of
mistakenly committing unnecessary files.

# Major Bindings

# Structure

# Modifications
