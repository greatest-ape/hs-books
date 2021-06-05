# FaaS app for displaying a HTML list of .epub books in a directory

The Haskell program extracts meta info and cover images, creates thumbnails of those and sends a JSON response to the website, where it is parsed and displayed using Javascript (utilizing the very hip jQuery.)

## Screenshots

### Desktop view

![Desktop screenshot](screenshots/screenshot-1.png?raw=true "Desktop view")

### Mobile view

![Mobile screenshot](screenshots/screenshot-2.jpg?raw=true "Mobile view")

## Install

  * Serve the `html` directory with your web server and enable cgi there
  * Make it readable by the web server, and make `html/media/covers`
    writeable. There is a script for doing both of those things on Debian:

```sh
./scripts/set-permissions-debian.sh
```

  * Compile program, copy binary to `html/app.cgi` (CGI is the FaaS technology used):

```sh
./scripts/build-copy.sh
```

## Design

There is no database, since any modifications should also be present in
the epub files. Instead, there is a caching system.

## Future ideas

  * Rewrite frontend in React. Current code is basically a poor man's
    version
  * Buttons for forcing reloads
  * Button for clearing search field
  * PDF support (tricky)
