# mmc-get
Mercury package management. Companion to https://mercury-in.space/packages/

## usage
```
$ mmc-get update
$ mmc-get list getr

Name             : getr
URL              : https://github.com/jrfondren/getr-mercury.git
Tags             : benchmarking
License          : mit
Builds           : getr
Dependencies     : []
Version Control  : git (releases: head)
FFI              : [c]
Description      : benchmarking wrapper around getrusage

$ mmc-get get getr
Cloning into 'getr'...
remote: Enumerating objects: 14, done.
remote: Counting objects: 100% (14/14), done.
remote: Compressing objects: 100% (10/10), done.
remote: Total 24 (delta 4), reused 12 (delta 4), pack-reused 10
Unpacking objects: 100% (24/24), done.

$ cd getr
$ ls
getr.m  LICENSE  Makefile  README.md  rusage.m  spawn.m
```
At present, mmc-get is good for finding and getting Mercury packages into the
current directory, and not much else. It's missing lots of nice-to-have
features from typical package managers.

## build
```
make
```

## install
```
make install
```

This installs 'mmc-get' into ~/bin

## deploy
```
make deploy
```

This installs 'lint' into ~/bin and 'postpackage' into ~/public\_html/packages/
as postpackage.cgi

## mmc-get
This is a CLI tool intended to work with hosted package lists.

## lint
This is an admin tool for mmc-get's server-side package lists.

## postpackage.cgi
This is a bare CGI script that accepts a Mercury term (and thus, not the normal
browser submission of an HTML form; this needs to either be input from a
non-browser script or an AJAX call) and then appends it to a file.

## package.m et al.
These are libraries supporting the above applications.

## caveats
- postpackage.cgi is not that secure and should, at a minimum, run as an unprivileged user with a filesystem quota.
- despite any suggestions from name 'mmc-get', it is not an official part of Mercury.
