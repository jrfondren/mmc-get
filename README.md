# mmc-get
Mercury package management

## build
```
make
```

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
browser submission of an HTML form; this needs to either by input from a
non-browser script or an AJAX call) and then appends it to a file.

## package.m et al.
These are libraries supporting the above applications.

## caveats
- postpackage.cgi is not that secure and should, at a minimum, run as an unprivileged user with a filesystem quota.
- despite any suggestions from name 'mmc-get', it is not an official part of Mercury.
