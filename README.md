# CGI app for displaying a HTML list of .epub books in a directory

The Haskell code extracts meta info and cover images, creates thumbnails of those and sends a JSON response to the website, where it is parsed and displayed using Javascript.

To install, serve the public_html directory with your web server, enable cgi and run

```sh
stack setup # Only necessary once
stack build && ./scripts/copy-cgi.sh
```
