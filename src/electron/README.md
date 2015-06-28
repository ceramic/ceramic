# Electron Driver

Electron driver for Ceramic.

# Overview

`tools.lisp` is a set of tools to download and extract [Electron][electron]
binaries.

`driver.lisp` is a driver that implements the Ceramic API using
[Electron][electron].

The `main.js` file that is inserted into an Electron release to run the main
process, and listen for commands (in the form of one-line JSON messages) from
the standard input stream.

The `driver.lisp` file has functions for applying the necessary changes to the
release (inserting the JavaScript, the `package.json`, deleting the default app
files), launching the Electron process, and sending commands (to open windows,
load URLs, etc).

[electron]: http://electron.atom.io/
