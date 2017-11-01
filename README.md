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

# New Modules

In addition to configuring existing Emacs modules, Adequate contains a bunch of
new modules. Ideally, these would eventually make their way to
[Melpa](http://melpa.milkbox.net/), but for now they can be found here.

#### portage-mode

`portage-mode`, `portage-mode-accept-keywords-mode` and `portage-mode-use-mode`
add syntax highlighting and other convenience features for
[Portage's](https://wiki.gentoo.org/wiki/Portage) configuration files. These are
mainly useful in Gentoo-based environments.

#### mpris-control

`mpris-control-info-mode` display the currently playing song in Emacs mode line.
The mode should support all media players compatible with the [MPRIS D-Bus Interface
Specification](https://specifications.freedesktop.org/mpris-spec/latest/).

#### dbus-control

`dbus-control-mode` is a global minor mode that exposes Emacs as a D-Bus
service. This makes it possible to open files in existing Emacs instances using
`emacs-opener.el` script`. You can achieve similar functionality using Emacs
built-in daemon mode.

#### all-monitors

`all-monitors-fill-all-monitors` function can be used to quickly fill all the
monitors with Emacs frames. By default, the new frames will be full screen
state, but if universal argument is supplied, the frames will be maximized.

#### ebuild-mode

`ebuild-mode` can be used to edit
[Portage's](https://wiki.gentoo.org/wiki/Portage) ebuild files.

#### google-translate-repl

`google-translate-repl` builds upon `google-translate` module by offering a
REPL-like interface to the translator. `:source` and `:target` commands can be
used to control source and target languages. The `:swap` command can be used to
swap the source and target languages.

#### guess-language-lite

`gll-guess-language-lite-mode` builds upon `guess-language` module by providing
a simpler mode for buffer language detection that does not try to support
multiple languages in a single buffer.

#### helm-org-files

Use Helm for finding and visiting Org files from `org-directory`.

#### helm-emoji

Use Helm for inserting emoji characters.

#### helm-weechat

Use Helm for selecting Weechat chat buffers.
