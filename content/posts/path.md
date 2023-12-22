+++
title = "So, you want to set your PATH?"
slug = "path"
summary = "A rare case when I would actually prefer Windows over Linux."
tags = ["complaints"]

draft = true
+++


## Setting `%PATH%` on Windows
1. Open explorer
2. Right click on an empty space
3. "Properties"
4. The "Advanced System Settings" on the left
5. The "Advanced" tab (i.e. you need advanced advanced system settings)
6. The "Environment Variables" button.

And there you have it!
A nice GUI that lets you set environment variables both for your user only and system-wide.

Despite all the classical Windows' bullshit of performing somewhat arbitrary navigation
through different GUIs until you get where you need to
(as opposed to having all the settings in one control panel),
this time the path taken isn't that counter-intuitive,
and the GUI that you get in the end is totally worth it.

And hey! Modern Windows has search, so unless it decides to open Bing in Internet Explorer
instead of showing the useful results, or unless you have a localized version of Windows,
in which case the search most likely finds nothing 95% of the time,
you can get where you need pretty fast and

## Setting `$PATH` on Linux

If you have PAM, you can use `/etc/environment`, but only supports setting variables to plain text, so no preppeding or appending.
Alternatively, also with PAM you can use `/etc/security/pam_env.conf ` for system-wide configuration, or `~/.pan_environment`. It supports a more advanced syntax with "DEFAULT" and "OVERRIDE".
The order is as follows: `/etc/security/pam_env.conf`, `/etc/environment`, `~/.pan_environment`.

You have systemd?

Then, you get into your login shell. I really hope that you login-shell is bash, or at very least is POSIX-compatible, so not fish, please.
Your login shell will then execute `/etc/profile`, which probably contains code to also execute everything in `/etc/profile.d/`. Those are system wide. Then your per-user `~/.profile` get executed.
Unless you have bash. It will prefer `~/.bash_profile` over `~/.profile`, if present. If `~/.bash_login` is present, it will be used instead of both  `~/.bash_profile` & `~/.profile`. There's also `~/.bashrc`, but it is executed for non-login shells. But chances are you have eveything in `~/.bashrc` and your `~/.profile` / `~/.bash_profile` / ~/.bash_login` just sources it. There's also such thing as `~/.bash_logout`, but that's unrelated.

![Linux дома](https://s00.yaplakal.com/pics/pics_original/0/5/5/120550.jpg)