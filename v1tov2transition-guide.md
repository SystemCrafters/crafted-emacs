# Overview

When transitioning from the original release of Crafted Emacs (called Crafted
Emacs V1 or just V1 from now on), the differences in the fundamental approach
need to be considered.

V1 was intended to setup an out-of-the-box experience that provides an essential
base configuration without the need of the user knowing anything about it.  In
several ways, this "hijacked" the normal Emacs configuration.

## Hijacking #1: The Basic Configuration

With V1, we told users to clone the Crafted Emacs repository to `$HOME/.emacs.d`
(or `$HOME/.config/emacs`).  Within that directory, we had the directory
`crafted-emacs`, which contained the user's own configuration.  Essentially, we
took over the place where the user configuration is supposed to live and loaded
the user's actual config from there.

With V2, we turn that around.  Your config lives where it's supposed to go
(`$HOME/.emacs.d` or `$HOME/.config/emacs`) and you can clone the Crafted Emacs
repository where you see fit.

The difference might look like this:

|               | V1                       | V2             |
|---------------|--------------------------|----------------|
| Crafted Emacs | `.emacs.d`               | somewhere else |
| User config   | `.emacs.d/crafted-emacs` | `.emacs.d`     |

- In V1, Crafted Emacs *is* the config and integrates the user's settings.
- In V2, the user's settings integrates pre-built modules of Crafted Emacs.

This will be the first thing to consider when transitioning.

## Hijacking #2: Auto-generated files

Additionally, we moved auto-generated folders and files to that user
configuration folder as well.  The intention was to allow the user to use git or
some other version control for their own configuration repository and ensure it
wouldn't interfere with Crafted Emacs or - even worse - accidentally show up in
the Crafted Emacs repository on GitHub.

This will be the second thing to transition and will likely be the most
difficult part of the process.

## Terms

A few terms we will be using in the rest of this guide:

- `user-emacs-directory` :: The place where you cloned Crafted Emacs V1, likely
  `$HOME/.emacs.d` or `$HOME/.config/emacs`.  If you used
  [Chemacs2](https://github.com/plexus/chemacs2) with V1, check your
  `~/.emacs-profiles.el` to find the correct location.
- `crafted-config-path` :: The folder of your user config in V1, likely called
  `crafted-emacs` within `user-emacs-directory`.

As a final note before starting, this guide will make a best effort at
describing the process, however, that is the best that can be done.  Each user
will have updated their configuration in their own way, which makes it
impossible to describe handling every situation.

# Transitioning the `user-emacs-directory`

The `user-emacs-directory` is the location where you cloned the Crafted Emacs
repository. Since there may be some files there you don't want to lose, the
first step will be to rename this directory to (for example) `crafted-emacs-v1`.

Alternatively, if you have reviewed the contents of the `user-emacs-directory`
and there is nothing there to keep, you can simply delete it.   
**WARNING: Only take this approach if you are 100% certain you DO NOT want to
keep anything as it will be lost forever**

Once complete, you can now rename the folder located at `crafted-config-path` to
something Emacs will recognize as the `user-emacs-directory`. This will be one
of the following depending on your system (this is not an exhaustive list, just
some examples):

* `$HOME/.emacs.d`
* `$HOME/.config/emacs`
* `%APPDATA\.emacs.d`
* `%APPDATA\_emacs_d`

# Transitioning your configuration

Now for the hard part. This would be a great time to go read the [Getting Started
Guide](https://github.com/SystemCrafters/crafted-emacs/docs/getting-started-guide.org),
which explains the basic init and module structure of V2.

## V1 config files to V2 config files

Follow the advice in section [Bootstrapping Crafted
Emacs](https://github.com/SystemCrafters/crafted-emacs/docs/getting-started-guide.org#bootstrapping-crafted-emacs)
to make sure you have the appropriate modules loaded for initializing
the default package management system and the `load-path`.

You can choose to take one of the following approaches:

- Either copy the example files for `early-init.el` and `init.el`, then transfer
  the settings of your `early-config.el` and `config.el` into those files.
- Or rename `early-config.el` to `early-init.el` and `config.el` to `init.el`,
  then load the appropriate V2 modules `crafted-early-init-config` and
  `crafted-init-config` in each. This will set up the `load-path` correctly for
  you.
    
## Structure your `init.el`

Make sure to reorganize your `init.el` file according to the
description in section [Crafted Emacs Modules](https://github.com/SystemCrafters/crafted-emacs/docs/getting-started-guide.org#crafted-emacs-modules). Essentially:

1.  Load the `custom-file`
2.  Create a packages section in your `init.el`, in which you load the `-package`
    modules, and add any additional packages to the `package-selected-packages`
    list
3.  Install the packages (e.g. by calling `package-install-selected-packages`)
4.  Create a configuration section in your `init.el`, in which you load the
    `-config` modules and add your custom configuration as needed

## V1 Modules to V2 Modules

Several of the modules from V1 don't exist in V2. Some of their functionality
has been moved to other V2 modules. Also, many modules now consist of two parts:
package installation (e.g. `crafted-completion-packages`) and configuration
`crafted-completion-config`).  The following table gives an overview. If the
respective V2 module consists of two parts, this is given with an asterisk, e.g.
`crafted-completion-*`.

| V1                        | V2                        |
|---------------------------|---------------------------|
| `crafted-compile`         | deleted                   |
| `crafted-completion`      | `crafted-completion-*`    |
| `crafted-defaults`        | `crafted-defaults-config` |
| `crafted-editing`         | `crafted-writing-*`       |
| `crafted-erlang`          | deleted                   |
| `crafted-evil`            | `crafted-evil-*`          |
| `crafted-ide`             | `crafted-ide-*`           |
| `crafted-latex`           | `crafted-writing-*`       |
| `crafted-lisp`            | `crafted-lisp-*`          |
| `crafted-mastering-emacs` | `crafted-defaults-config` |
| `crafted-org`             | `crafted-org-*`           |
| `crafted-osx`             | `crafted-osx-config`      |
| `crafted-pdf-reader`      | `crafted-writing-*`       |
| `crafted-project`         | `crafted-init-config`     |
| `crafted-python`          | deleted                   |
| `crafted-screencast`      | `crafted-screencast-*`    |
| `crafted-speedbar`        | `crafted-speedbar-*`      |
| `crafted-startup`         | `crafted-startup-config`  |
| `crafted-ui`              | `crafted-ui-*`            |
| `crafted-updates`         | `crafted-updates-config`  |
| `crafted-windows`         | `crafted-defaults-config` |
| `crafted-workspaces`      | `crafted-workspaces-*`    |

To transition, search for `(require 'crafted-` in your config and replace the
respective module. If the module consists of a `-packages` and a `-config` part,
now, the `-packages` part must come earlier in the configuration than the
`-config` part.

For example, replace:
```elisp
;; V1 config.el
(require 'crafted-completion)
```

with:
```elisp
;; V2 init.el
(require 'crafted-completion-packages) ; in the packages section
(require 'crafted-completion-config)   ; in the configuration section
```

## Replace uses of `crafted-install-package`

In V1 we provided a macro `crafted-package-install-package`. If you made use of
that in your V1-`config.el` you need to replace it in your V2-`init.el`, e.g.
like this:

```elisp
;; V1 config.el
(crafted-package-install-package 'foo)
```

```elisp
;; V2 init.el
(add-to-list 'package-selected-packages 'foo) ; in the packages section
```
