# Overview

There are a number of considerations when transitioning between the
original release of Crafted Emacs (called Crafted Emacs V1 or just V1
from now on).  V1 was setup to provide an out-of-the-box experience
and in several ways hijacked normal Emacs configuration.  This will
make the transition more challenging as we have changed the approach
from providing a turn-key solution to acknowledging the user has their
own ideas on what a configuration should be, where it should live, and
what to install.  In V2, we provide hooks (for lack of a better term)
to enable utilizing pre-built modules, but depend on the user to
manage their configuration. 

With V1, we told you to clone the Crafted Emacs repository to either
`$HOME/.emacs.d` or `$HOME/.config/emacs`.  This was the first
"hijacking" we did.  The purpose was to provide the essential base
configuration files without the user knowing anything about it.  This
will be the first thing to transition when moving to V2.
Additionally, we created a Crafted Emacs folder where the user
configuration would live, and made an attempt to move auto-generated
folders and files to that location as well.  While we made a best
effort at moving everything out of the Crafted Emacs repository
location, this was far from perfect or complete.  The intention was to
allow the user to version some file, like the `diary` file in their
own configuration repository and ensure it wouldn't show up in the
Crafted Emacs repository on GitHub.  This will be the second thing to
transition and will likely be the most difficult part of the process.

As a final note before starting, this guide will make a best effort at
describing the process, however, that is the best that can be done.
Each user will have updated their configuration in their own way,
which makes it impossible to describe handling every situation.

# Transitioning the `user-emacs-directory`

The `user-emacs-directory` is the location where you cloned the
Crafted Emacs repository.  Since there may be some files there you
don't want to lose, the first step will be to rename this directory to
(for example) `crafted-emacs-v1`.

Alternatively, if you have reviewed the contents of the
`user-emacs-directory` and there is nothing there to keep, you can
simply delete it. **WARNING: Only take this approach if you are 110%
certain you DO NOT want to keep anything as it will be lost forever**

Once complete, you can now rename the folder located at
`crafted-config-path` to something Emacs will recognize as the
`user-emacs-directory`.  This will be one of the following depending
on your system (this is not an exhaustive list, just some examples):

* `$HOME/.emacs.d`
* `$HOME/.config/emacs`
* `%APPDATA\.emacs.d`
* `%APPDATA\_emacs_d`

# Transitioning your configuration

Now for the hard part.  Several of the modules from V1 don't exist in
V2.  That is something to be aware of.  For those which do exist,
their name(s) will have `-packages` (in some cases) and `-config` in
all cases.  If we suggest installing packages for a module, that
module must come earlier in the configuration than the `-config`
modules.  This would be a great time to go read the [Getting Started
Guide](https://github.com/SystemCrafters/crafted-emacs/docs/getting-started-guide.org)

Assuming you have read that guide, especially section 4.1.2 to clone
the V2 repo, you can choose to take one of a couple of approaches:

1.  Copy the example files for `early-init.el` and `init.el`, then
    merge your `early-config.el` and `config.el` into those files.
2.  Rename `early-config.el` to `early-init.el` and `config.el` to
    `init.el`, then make the changes to load the appropriate V2
    modules in each so the `load-path` is setup correctly for you.
    
Follow the advice in sections 4.2.1 and 4.2.2 to make sure you have
the appropriate modules loaded for initializing the default package
management system and the `load-path`.

Make sure to reorganize your `init.el` file according to the
description in sections 4.3, essentially:

1.  Load the `custom-file`
2.  Load the `-package` modules, and add any additional packages to
    the `package-selected-packages` list
3.  Install the packages
4.  Load the `-config` modules
5.  Add custom configuration as needed

