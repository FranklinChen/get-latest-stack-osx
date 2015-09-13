# Get latest Stack for OS X

[![Build Status](https://travis-ci.org/FranklinChen/get-latest-stack-osx.png)](https://travis-ci.org/FranklinChen/get-latest-stack-osx)

Haskell version of a [Python script](https://www.reddit.com/r/haskell/comments/3ksdoh/script_to_download_the_latest_stack/).

Just an illustration of how to write a Haskell script that
- downloads stuff from the Web
- parses JSON using Lens
- uncompresses GZip
- extracts files from a tar archive

## Instructions

```console
$ stack build
```

To run the program directly (don't do this if you don't actually want
to end up with a huge `stack` executable for Mac OS X in your current
directory!):

```console
$ stack exec get-latest-stack-osx
```

To install the program as `~/.local/bin/get-latest-stack-osx`:

```console
$ stack install
```
