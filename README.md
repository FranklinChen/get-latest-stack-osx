# Get latest Stack for OS X

[![Build Status](https://travis-ci.org/FranklinChen/get-latest-stack-osx.png)](https://travis-ci.org/FranklinChen/get-latest-stack-osx)

Haskell version of a [Python script](https://www.reddit.com/r/haskell/comments/3ksdoh/script_to_download_the_latest_stack/).

Just an illustration of how to write a Haskell script that
- downloads stuff from the Web
- parses JSON using Lens
- uncompresses GZip
- extracts a single file from a tar archive

## Instructions

```console
$ stack build
```

To install the program as `~/.local/bin/get-latest-stack-osx`:

```console
$ stack install
```

To run the program, downloading and extracting a huge `stack` executable for Mac OS X to standard output):

```console
$ get-latest-stack-osx > /path/to/new/stack && chmod a+x /path/to/new/stack
```
