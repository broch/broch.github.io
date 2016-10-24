---
title: Linux Desktop Installation with XMonad
author: Luke
date: 2016-11-31
tags: linux,xmonad
---

Earlier this year, after many years of using OSX, I decided to switch to using Linux for my desktop. Most of the development tools I use work better on Linux and I wanted to use a tiling window manager with a minimal user interface. The changes Apple have been making to their machines and OS over the past few years seem to have been irrelevant to my needs at best, and in some cases are downright annoying. The recent release of laptops with no escape key and yet more different connectors is just confirmation that I made the right choice. I bought a Thinkpad P50 as my work machine.

I've maintained small Linux server installations for many years, and am happy enough setting up postfix or nginx, but I've never really used Linux as a desktop environment. I wanted to use a lightweight XMonad setup rather than a stock distro desktop installation, which meant having to find out about a lot of things that would otherwise be taken care of automatically, but the result is that I have a simpler system and also a better understanding of how it actually works.

This article describes the installation process, what I learned along the way, and how I got to where I wanted to be (or near enough).

Install Ubuntu 16.04 Server
===========================

I used an Ubuntu server installation as a starting point. I wanted something reliable to build on which would be maintained long-term, but I didn't want all the unnecessary noise of a full Gnome/Unity setup when I would be running XMonad as the window manager. I might experiment with other Linux distros in future when I'm more familiar with my current setup and have been running it for a while and I need to do another installation.

Networking
==========

The installation was done with an ethernet connection and if this is missing on a subsequent boot, the system will hang waiting for the connection to come up (there is timeout configured in `/etc/TODO/failsafe`). By default, the static network configuration is read from the file `/etc/network/interfaces`, but for a laptop, this isn't ideal, as the available networks may change and you probably want to be able to connect to different WIFI networks on demand. The `network-manager` package is a common solution. First comment out everything but the `lo` interface in the `/etc/network/interfaces` file:

```
# The loopback network interface
auto lo
iface lo inet loopback

# Commented out because we are using network-manager

# The primary network interface
# auto enp0s31f6
# iface enp0s31f6 inet dhcp
```

then install the package without the UI parts:

    sudo apt install -no-install-recommends network-manager

In a normal desktop setup, you would just select an available WIFI network from a menu of available connections. The `nmcli` command-line tool provides equivalent commands for everything you need to manage connections.

To create a DHCP configured ethernet connection

    nmcli connection add conn-name WiredHome type ethernet autoconnect yes

To list available WIFI connections ('list' is optional)

    nmcli device wifi list

To create a new WIFI connection (creates a new connection each time)

    nmcli device wifi connect "My Favourite Cafe SSID" password thepassword name "Cafe"

To switch WIFI off

    nmcli radio wifi off

Connections are created in `/etc/NetworkManager/system-connections`. To list all know connections

    nmcli connection show

To delete connections

    nmcli connection delete name_or_id


Managing External Disks
=======================

The `udisks2` package seems to be the back-end which most filesystem management tools (e.g. Nautilus) use to deal with hotplugging of USB disks, so I installed this. It comes with a command line program `udiskctl`.

To list devices

```
$ udiskctl status
MODEL             REVISION    SERIAL          DEVICE
-------------------------------------------------
Databar           5.00        07ABC           sda

```

To find out more about a device

    udiskctl info -b /dev/sda

To mount a filesystem

    udiskctl mount -b /dev/sda1

To unmount it

    udiskctl unmount -b /dev/sda1

To power off the disks

    udiskctl power-off -b /dev/sda

Disks are mounted under `/media/username` - TODO Check.

KeyRing
=======

Install `gnome-keyring` to store encrypted passwords for email and other secrets.

    sudo apt install --no-install-recommends gnome-keyring libpam-gnome-keyring

 Also install `libsecret-tools` which allows access to the keyring using the `secret-tool` command.

    sudo apt install libsecret-tools

PAM login setup adds an entry to `/etc/pam.d/common-password`. We also need entries in `common-auth`:

    auth optional pam_gnome_keyring.so

and `common-session`:

    session optional pam_gnome_keyring.so auto_start

TODO: Check this and add some footnotes on what the entries are supposed to do. Try to work out why the keyring isn't unlocked on login

Email
=====

My email setup is heavily influenced by [Pat Brisbin's](https://github.com/pbrisbin/dotfiles) [^pbmutt]. It uses

* mbsync for syncing gmail over IMAP
* msmtp for sending emails
* mutt for reading mails

[^pbmutt]: Note that the mutt/email articles in his blog are out of date compared to the current setup.

Passwords for accounts can be added to the keyring using secret tool:

    secret-tool store --label="Gmail password for myaccount" gmail myaccount

And looked up in `.mbsyncrc`:

    PassCmd "secret-tool lookup gmail myaccount"

and in `.msmtprc` [^msmptp-pass]:

    passwordeval secret-tool lookup gmail myaccount | awk 1

[^msmtp-pass]: `msmtp` is supposed to integrate directly with the keyring but I couldn't get it to work. In any case it makes more sense to share the same keyring entry between the two. The `passwordeval` option enables this. It fails without the `awk 1`, probably because it doesn't write out the required newline.

XMonad
======

One of the reasons for switching systems was to be able to use XMonad. A tiling window manager just fits much better with a workflow which mostly involves using an editor and terminals.

The main installation:

    sudo apt install xmonad dmenu xmobar xinit rxvt-unicode-256 x11-xserver-utils

which should start up XMonad as the window manager when the `startx` command is run [^xinit].

[^xinit]: The default setup ends up running the script `/usr/bin/x-session-manager` which starts xmonad. For more information, see the `startx` man page.

TODO: Try without default xinitrc (i.e. write our own and call xmonad in it)
TODO: Work out how /usr/bin/x-session-manager gets created (it's not part of xmonad package).

I also installed the font packages `fonts-inconsolata` for use in the console and `fonts-wqy-zenhei` for Chinese support.
