---
title: Linux Desktop Installation with XMonad on Thinkpad P50
author: Luke
date: 2015-05-31
tags: linux,xmonad
---

I've maintained small Linux server installations for many years, and am happy enough setting up postfix or nginx, but I've never really used Linux as a desktop environment until now. I decided to make the switch from OSX and bought a new Thinkpad P50 as my work machine. I wanted to use a lightweight XMonad setup rather than a stock distro desktop installation, which meant having to find out about a lot of things that would otherwise be taken care of automatically, but the result is that I have a simpler system and also a better understanding of how it actually works.

This article describes the installation process, what I learned along the way, and how I got to where I wanted to be (or near enough).


Install Ubuntu 16.04 Server
===========================

I used an Ubuntu server installation as a starting point. I wanted something reliable to build on which would be maintained long-term, but I didn't want a full Gnome/Unity setup.


Networking
==========

The installation was done with an ethernet connection and if this is missing on a subsequent boot, the system will hang waiting for the connection to come up (there is timeout configured in `/etc/TODO/failsafe`). The configuration is taken from the file `/etc/network/interfaces`. For a laptop, this isn't ideal, as the available networks may change and you want to switch between them automatically. The `network-manager` is a common solution. First comment out everything but the `lo` interface in the `/etc/network/interfaces` file:

```
    lo BLAH TODO
```

then install the package without the UI parts (I used the `nmcli` command-line tool to define connections):

    sudo apt install -no-install-recommends network-manager

To create a DHCP configured ethernet connection

    nmcli connection add conn-name WiredHome type ethernet autoconnect yes

To list available WIFI connections

    nmcli dev wifi

To create a new WIFI connection

    nmcli dev wifi con "My Favourite Cafe SSID" password thepassword name "Cafe"

To switch WIFI off

    nmcli radio wifi off


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

