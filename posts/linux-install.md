---
title: Linux Setup with XMonad
author: Luke
date: 2016-11-19
tags: linux,xmonad
updated: 2018-02-11
---


Earlier this year, after many years of using OSX, I decided to switch to using Linux for my desktop. Most of the development tools I use work better on Linux -- I mostly work directly with plain-text format files and I use the terminal a lot. I also wanted to use a tiling window manager with a minimal user interface. The changes Apple have been making to their machines and OS over the past few years have been irrelevant to my needs at best, and in some cases are downright annoying. The recent release of laptops with no escape key and yet more connectors (dongle hell) is just confirmation that I made the right choice. I bought a Thinkpad P50 as my work machine [^p50].

[^p50]: There's nothing specific to the P50 in this article. It's quite a chunky, powerful machine. It has an Nvidia graphics card as well as the on-board Intel card and this is one area where Linux is a bit of a headache. Support for dual-graphics card laptops on Linux isn't great. The laptop screen runs fine using the Intel card, but there are some display issues if I use an external monitor with it. If I switch to the NVidia card exclusively it works fine but this requires a reboot and a BIOS switch. I haven't tried using NVidia's drivers or optimising the setup for switching between the cards yet. There are tools for this but it seems like a bit of a minefield and I'm happy enough with it for now. This will improve in future versions, but if you don't need high-performance graphics then you might be better picking a machine with a single card.

I'd maintained small Linux server installations for many years, and am happy enough setting up postfix or nginx, but I'd never really used Linux as a desktop environment. I wanted to use a lightweight XMonad setup rather than a stock distro desktop installation, which meant having to find out about a lot of things that would otherwise be taken care of automatically, but the result is that I have a simpler system and also a better understanding of how it actually works.

![XMonad up and running](xmonad-desktop.jpg)

This article describes the installation process, things I discovered along the way, and how I got to where I wanted to be (or near enough).

## Install Ubuntu 16.04 Server

_Update_: Not long after writing this article I switched to using Arch Linux as the base distribution, which I'd thoroughly recommend. This is my [current list of installed packages](arch-packages.txt) [^arch]. Also, I now build `xmonad` and `xmobar` as part of my local Haskell development environment rather than installing the distro versions, but aside from that, my setup is still very similar to the one described here.

[^arch]: My initial motivation for the switch to Arch Linux was to have a system with a newer kernel and `xorg` packages (1.19+) which had better support for Nvidia hardware and dual graphics cards. Any concerns I might have had about stability didn't last long. I've had very few issues and the documentation is great.

I used an Ubuntu server installation as a starting point. I wanted something reliable to build on which would be maintained long-term, but I didn't want all the unnecessary noise of a full Gnome/Unity setup when I would be running XMonad as the window manager. I might experiment with other Linux distros in future when I'm more familiar with my current setup and have been running it for a while.

## Networking

The installation was done with an ethernet connection and if this is missing on a subsequent boot, the system will hang waiting for the connection to come up [^net-timeout]. By default, the static network configuration is read from the file `/etc/network/interfaces`, but for a laptop, this isn't ideal, as the available networks may change and you probably want to be able to connect to different WIFI networks depending on where you are. The `network-manager` package is a common solution. First comment out everything but the `lo` interface in the `/etc/network/interfaces` file:

[^net-timeout]: There is a timeout configured somewhere which I changed the first time I ran into the problem, but I can't find the file anymore. In any case it doesn't matter once the ethernet settings have been removed from `/etc/network/interfaces`.

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

    sudo apt install --no-install-recommends network-manager

In a normal desktop setup, you would just select an available WIFI network from a menu of available connections. The `nmcli` command-line tool provides equivalent commands for everything you need to manage connections. NetworkManager will connect automatically to networks in its database when you boot up, so you should only need to interact with it when you are connecting from a new location.

To create a DHCP configured ethernet connection

    nmcli connection add conn-name WiredHome type ethernet autoconnect yes

To list available WIFI connections ('list' is optional)

    nmcli device wifi list

To create a new WIFI connection (creates a new connection each time)

    nmcli device wifi connect "My Favourite Cafe SSID" password thepassword name "Cafe"

To switch WIFI off

    nmcli radio wifi off

To list all known connections

    nmcli connection show

To delete connections

    nmcli connection delete name_or_id

You can also edit or delete existing connections. Connections are stored as files in the `/etc/NetworkManager/system-connections` directory. Note that the passwords are stored in plain text in these files. I don't mind this since none of the WIFI connections I use are very secret [^nm-gkr].

[^nm-gkr]: NetworkManager can be integrated with gnome keyring, using a [`SecretAgent`](https://developer.gnome.org/NetworkManager/stable/gdbus-org.freedesktop.NetworkManager.SecretAgent.html) service. Removing the `psk` and setting `psk-flags=1` in the connection file means a user secret agent will be asked for the password (see `man nm-settings`). The `nm-applet` application from the package `network-manager-gnome` implements the required service interface. It will ask the user for the password on demand and store it for late use in the keyring. If you don't install this, a workaround is to remove the `psk` entry from the connection file and get `nmcli` to ask you to enter the password: `nmcli --ask connection up Cafe`. I also had a go at [writing my own SecretAgent](https://github.com/tekul/nm-agent) to try out the Haskell `dbus` and `gnome-keyring` libraries.


## Managing External Disks

The `udisks2` package seems to be the back-end which most filesystem management tools (e.g. Nautilus) use to deal with hotplugging of USB disks, so I installed this. It comes with a command line program `udisksctl`.

To list devices

```
$ udisksctl status
MODEL             REVISION    SERIAL          DEVICE
-------------------------------------------------
Databar           5.00        07ABC           sda

```

To find out more about a device

    udisksctl info -b /dev/sda

To mount a filesystem

    udisksctl mount -b /dev/sda1

To unmount it

    udisksctl unmount -b /dev/sda1

To power off the disks

    udisksctl power-off -b /dev/sda

Disks are mounted under `/media/<username>/`.

## Key Ring - Storing Passwords Securely

We don't want lots of email passwords and other secrets lying about in configuration files. A keyring program stores passwords (or other sensitive data) in an encrypted database, encrypted with a master password. The most common seems to be `gnome-keyring` which uses the derives a key from the login password to encrypt the data. Fortunately it can be used without pulling in anything else Gnome-related:

    sudo apt install --no-install-recommends gnome-keyring libpam-gnome-keyring

Also install `libsecret-tools` which allows access to the keyring using the `secret-tool` command.

    sudo apt install libsecret-tools

The installation adds an entry to `/etc/pam.d/common-password`, but you need to do some additional configuration if you want to start the keyring daemon when you log in on the console [^pam-auth-update]. I removed the entry and edited the `/etc/pam.d/login` and `/etc/pam.d/passwd` files as described in the [Arch Linux Wiki](https://wiki.archlinux.org/index.php/GNOME/Keyring#PAM_method) [^gkr-pam-problems].

[^gkr-pam-problems]: Setting the PAM stuff up seems quite temperamental and I had some problems. For example, the keyring daemon would be started but the keyring wouldn't be unlocked on login and I would still be prompted for my password when it was needed. So far, I can also only get `secret-tool` to work from within X-Windows.

[^pam-auth-update]: There is a tool called `pam-auth-update` which adds (or removes) the gnome keyring entry automatically. I guess it only adds the password entry since I'm using the console rather than a standard gnome setup.


## Email

My email setup is heavily influenced by [Pat Brisbin's](https://github.com/pbrisbin/dotfiles) [^pbmutt]. It uses

* mbsync for syncing gmail over IMAP
* msmtp for sending emails
* mutt for reading mails

[^pbmutt]: Note that the mutt/email articles in his blog are out of date compared to the current setup.

Passwords for accounts can be added to the keyring using secret tool:

    secret-tool store --label="Gmail password for myaccount" gmail myaccount

And looked up in `.mbsyncrc`:

    PassCmd "secret-tool lookup gmail myaccount"

and in `.msmtprc` [^msmtp-pass]:

    passwordeval secret-tool lookup gmail myaccount | awk 1

[^msmtp-pass]: `msmtp` is supposed to integrate directly with the keyring but I couldn't get it to work. In any case it makes more sense to share the same keyring entry between the two. The `passwordeval` option enables this. It fails without the `awk 1`, probably because it doesn't write out the required newline.

### Mail Address Lookup

When sending emails, programs will usually be able to lookup addresses either in a system address book or their own custom lists. With mutt, you need to decide how you want to maintain your contacts and tell it how to look them up. You can use a simple alias list, or [set up an external query command](https://wiki.archlinux.org/index.php/Mutt#Contact_management). I'm using `abook`, which is a step up up from simple aliases but still very basic. You can use it to query addresses and to add them directly from within mutt. I have the following added to my `.muttrc` file

    set query_command="abook --mutt-query '%s'"

    macro index,pager a "| abook --add-email\n"

The macro bound to 'a' overrides the default `create-alias` command. You can bind it to something else if you want.

## XWindows and XMonad

One of the reasons for switching systems was to be able to use [XMonad](http://xmonad.org). A tiling window manager just fits much better with a workflow which mostly involves using an editor and terminals. Indeed, it's hard to see the benefit of having to adjust window positions and sizes manually in any kind of workflow.

The required packages:

    sudo apt install xmonad dmenu xmobar xinit rxvt-unicode-256 x11-xserver-utils

Initial login still takes place on the console, which I prefer, but XMonad will be started when you run the `startx` command. You can customize the startup process by [writing your own `.xinitrc` file](https://github.com/tekul/dotfiles/blob/master/xinitrc) [^xinit].

[^xinit]: The default setup sources the files in `/etc/X11/Xsession.d` and ends up by running the script `/usr/bin/x-session-manager` which starts xmonad. For more information on the startup sequence, see the `startx` man page.

I also installed the font packages `fonts-inconsolata` for use in the console and `fonts-wqy-zenhei` for Chinese support.

I didn't need to do much to [customize XMonad](https://github.com/tekul/dotfiles/blob/master/xmonad/xmonad.hs). I added some key mappings to make the volume control keys work, some shortcut keys, and set `urxvt` to be the terminal, but other than that it's a very standard setup [^xdefaults]. `XMobar` provides a simple menu bar (which I can easily hide whenever I want) and `dmenu` makes it easy to run an application by typing its name (it is bound to the `Mod-p` key by default and provides text completion in the menu bar area).

[^xdefaults]: Details of fonts and other customizations for urxvt can be found in the [.Xdefaults file](https://github.com/tekul/dotfiles/blob/master/Xdefaults).

### Touchpad Sensitivity

The sensitivity of the touchpad can cause problems because you can accidentally brush against it and change focus while you are tying, either to a different point in the same file or to another window altogether. Needless to say this is very annoying. There are tools such as `syndaemon` which disables the touchpad while you are typing and there are also settings to adjust the sensitivity, but I still found this to be an issue so I just disabled the touchpad by adding

    synclient TouchPadOff=1

to my `.xinitrc` file. I also have a key mapping defined to toggle it on and off if I really want to use it.


## Sound

I'm still learning how the sound system works on Linux, but I installed the following packages

* `alsa-utils` command line utils for setting the volume and so on. Useful for binding to keys in `xmonad.hs`
* `sox` to be able to play and record from the command line (this duplicates some functionality in `alsa-utils` so may not be needed)

## Music

I've always detested iTunes. It's always been a dreadful music player and the only thing I missed when I switched from Windows to OSX was the Winamp music player. For years iTunes didn't even have a simple "add to queue" feature and it only plays Apple approved file formats. These days its priorities are acting as a front end to the app store and as a file manager for your iPhone (which I don't have).

Unsurprisingly there are plenty of music players on Linux. My favourite was [`cmus`](https://cmus.github.io/) which is a really nice terminal application with a very simple interface. The only customization I made was to set the colour scheme to "zenburn". It's also available on OSX via homebrew, so iTunes is history. Good riddance.

## Image Handling

### Viewing and Editing

[feh](https://feh.finalrewind.org/) is a command line tool which displays images in a simple window. It's handy if you're working in the terminal and generating images or graphs which you want to view immediately.

[Gimp](https://www.gimp.org/) is an obvious choice if you want to edit images. It as an optional dependency on the package `tcpd` which seems unnecessary. I also installed the `imagemagick` package which has lots of useful command line tools.

    sudo apt install --no-install-recommends gimp
    sudo apt install imagemagick

Gimp 2.8 comes with a single-window mode which works much better with XMonad tiling. This isn't the default, so you need to select it from the `Windows` menu or the tool windows will look weird.

### Managing Photo Collections

Loading and viewing photos is another requirement. My current choice is `gThumb` which seems to do everything I need and has fewer dependencies than the other major competitors I looked at.

    sudo apt install --no-install-recommends gthumb

I haven't been able to get it to download images from my phone yet, but I can do so using the command line tool `gphoto2`. For example, to download all files from a specific folder I use:

    gphoto2 -f /store_00010001/DCIM/Camera -P

## IRC and Instant Messaging

A lot of messaging these days takes place on phones and I'm fine with moving that way too, but there's a limit to how much I want to type on a phone. I still spend a lot of time in front of a computer and chat apps are pretty much essential if you collaborate with others remotely. I use Google chat with quite a few people so I needed a replacement for it. [Profanity](http://www.profanity.im) is an XMPP client which seems to work well. It's another simple terminal application, based on the IRC client [IRSSI](https://irssi.org). IRSSI was also easy to get running with my existing freenode account.

I do still have a lot of contacts in Skype but I don't use it as often as I used to and I haven't checked out the Linux version yet.

## Chinese Input on Linux

I've been learning (and re-learning) Chinese for a long time and the pinyin input method on OSX is very handy for people like me who can pronounce and recognise some characters, but not necessarily write them from memory.

The [`fcitx`](http://fcitx-im.org/) package seems to be pretty much the standard and I found it easy enough to install and get working.

    sudo apt install fcitx fcitx-pinyin

You can add other input methods as desired, for example:

    sudo apt install fcitx-table-wubi fcitx-sunpinyin fcitx-googlepinyin

and choose which ones are enabled using `fcitx-configtool` or by editing the file `~/.config/fcitx/profile`.

This was enough to get pinyin input working in all applications I tried (and the terminal) apart from Emacs, for which there is a [specific workaround](https://wiki.archlinux.org/index.php/Fcitx#Emacs).

### Adding Chinese Locale

Running `locale -a` showed that only English locales were available. To add simplified Chinese run:

    sudo locale-gen zh_CN.UTF-8

You can do this for any of the locales listed in `/usr/share/i18n/SUPPORTED`.

I have a shortcut in my `xmonad.hs` for running emacs. I changed this to set the variable beforehand:

    ((mod4Mask, xK_s), spawn "LC_CTYPE='zh_CN.UTF8' emacs24-x")

and `fcitx` then works in Emacs too (你看得见吗？). Emacs also has its own Chinese input method support but fcitx seems good enough for me.

## Further Resources

Most of the above was of course pilfered from other sources. Many of the programs I've mentioned have excellent websites and documentation and I found the [Arch Linux Wiki](https://wiki.archlinux.org/) really useful. Lots of people have also uploaded their configuration files to [github](https://github.com/) and you can easily find relevant material there using github's source search. This is great both for tuning your own files and for finding out about other useful things that people are using [^srndpty].

[^srndpty]: This is how I found out about `cmus`, for example. I think it was [in this repo](https://github.com/ssh0/dotfiles) which also has quite a sophisticated Xmonad setup.

## Conclusions

So far, I'm pretty happy with my setup. There have been benefits I expected and also some things I like which I hadn't anticipated. I have a simple system with the bare minimum of installed packages to do what I want -- there's none of the extra garbage that vendors feel the need to clutter their latest offerings with to attract attention. I had to learn various new commands to allow me to do things which would normally be done through a UI, but in the long term this is often more efficient. It's certainly no worse than having to remember the different "control panel" locations and sequences of menu items which a UI layers on top. Using the command line directly strips away this extra obfuscation and the knowledge is more portable than familiarity with different window-based utilities.

Working with XMonad is much nicer, particularly for programming, where a tiling window manager comes into its own. It's easy to fire up a terminal next to or below your current window without losing focus of what you were looking at before, whether it's a browser page or something in your editor. Working with multiple desktops and shunting windows between them quickly becomes a normal part of your workflow and there's no clutter taking up screen space unnecessarily. It's all super fast, without distractions such as animations, notification bars and so on. Since I haven't configured `mbsync` to pull down mail automatically, I also find that I now prefer to only read email when I choose, so there are no push notifications in the system at all and thus no interruptions when I'm working on something.

I can't really come up with anything I need that is seriously missing on Linux. Some things, like hotplugging cameras and downloading images is slicker on OSX, but that's more an issue with my minimal setup than Linux in general -- a standard distro installation would probably be better at this kind of thing. OSX has nicer fonts (Chinese sometimes mixes fonts and looks a bit rough in my terminal) but I'm still hoping to make improvements on that front.

I hope someone finds something useful here. If anyone has any suggestions for things I might be missing, please let me know.
