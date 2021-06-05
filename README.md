# FaaS web app for listing .epub books in a directory

This Haskell program extracts meta info and cover images, creates thumbnails of those and sends a JSON response to the client, which parses and displays it.

## Screenshots

### Desktop view

![Desktop screenshot](screenshots/screenshot-1.png?raw=true "Desktop view")

### Mobile view

![Mobile screenshot](screenshots/screenshot-2.jpg?raw=true "Mobile view")

## Install

  * Serve the `html` directory with your web server and enable CGI
  * Make `html` readable by the web server and make `html/media/covers`
    writeable. There is a script for doing both of those things on Debian:

```sh
./scripts/set-permissions-debian.sh
```

  * Compile the program and copy the binary to `html/app.cgi`

```sh
./scripts/build-and-install.sh
```

## Design

There is no database, since any modifications should also be present in
the epub files. Instead, a caching system is used.

## Ideas for future

  * Rewrite frontend in React? Current code is a poor man's
    version
  * Modernize FaaS technology?
  * Buttons for forcing reloads
  * Button for clearing search field
  * PDF support (tricky)
