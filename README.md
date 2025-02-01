# pinboard-notes-backup [![Build](https://github.com/bdesham/pinboard-notes-backup/actions/workflows/main.yaml/badge.svg)](https://github.com/bdesham/pinboard-notes-backup/actions/workflows/main.yaml)

Back up the notes you’ve saved to [Pinboard].

[Pinboard]: https://pinboard.in

## Installation

### Homebrew

If you’re using [Homebrew], you can just run

    brew install pinboard-notes-backup

[Homebrew]: https://brew.sh

### Nix

If you’re using the [Nix] package manager, run

    nix-env -i -A nixpkgs.pinboard-notes-backup

If you use NixOS, run

    nix-env -i -A nixos.pinboard-notes-backup

[Nix]: https://nixos.org/nix/

### Manual installation

Prebuilt binaries are available for macOS and Linux—find them on [the page for the most recent release][release]. To install the program from one of these,

1. Download the archive for your OS and unpack it.

2. Copy the `pnbackup` binary to some directory in your `PATH`, like `/usr/local/bin`.

3. *Optional:* Install the man page by running

       install -D --mode=444 pnbackup.1 /usr/local/share/man/man1

4. *Optional:* Generate and install a Bash completion script by running

       mkdir -p /usr/local/etc/bash_completion.d
       pnbackup --bash-completion-script "$(which pnbackup)" > \
           /usr/local/etc/bash_completion.d/pnbackup.bash

5. *Optional:* Generate and install a Zsh completion script by running

       mkdir -p /usr/local/share/zsh/site-functions
       pnbackup --zsh-completion-script "$(which pnbackup)" > \
           /usr/local/share/zsh/site-functions/_pnbackup

[release]: https://github.com/bdesham/pinboard-notes-backup/releases/latest

### Building from source

This is a Haskell program that you can build using [Stack]. With Stack installed, just type

    stack setup

to download and install GHC (the Haskell compiler) and then

    stack install --local-bin-path=/usr/local/bin

to build the `pnbackup` binary and install it. You may of course specify a different installation directory if you wish.

[Stack]: http://docs.haskellstack.org/en/stable/README/

You can also build the project with cabal-install, but I’m less familiar with that approach.

This project [uses GitHub Actions][actions] for automated building. The build is checked on the latest versions of macOS and Ubuntu Linux, with various combinations of Stack, cabal-install, and GHC 8.6.x, 8.8.x, 8.10.x, and 9.0.x. The project does *not* currently build under GHC 9.2.x because of [a temporary incompatibility] between [req] and that GHC version.

[actions]: https://github.com/bdesham/pinboard-notes-backup/actions
[a temporary incompatibility]: https://github.com/mrkkrp/req/pull/125
[req]: https://github.com/mrkkrp/req

## Usage

First you’ll need to grab your Pinboard API token from Pinboard’s [password settings] page. It’s a string like “maciej:abc123456”.

To save your notes to a file called “Notes.sqlite”, run

    pnbackup -t maciej:abc123456 Notes.sqlite

replacing the example API token with your own. This will put all of your notes into a SQLite database called “Notes.sqlite”.

Each time you run the program, it will fetch the list of your notes from the Pinboard server. It will then download any notes that are new or that have been updated on the server, and it will delete any notes that have been deleted from the server. The program does one-way synchronization only: it will update your local database to match what’s on the server but it will never make any changes on the server.

The [Pinboard API] requires a three-second wait time between each request, and the text of each note must be downloaded in a separate request, so the initial download of your notes may take a while. Subsequent syncs will generally be much shorter, depending on how often you add or modify notes and how often you run pnbackup. If you want to see exactly what `pnbackup` is doing as it works, pass it the `-v` or `--verbose` flags.

[password settings]: https://pinboard.in/settings/password
[Pinboard API]: https://pinboard.in/api/

## Data format

Your notes are stored in a table called “notes” with the following schema:

``` sql
CREATE TABLE notes (
    id TEXT NOT NULL UNIQUE,
    title TEXT NOT NULL,
    text TEXT NOT NULL,
    hash TEXT NOT NULL,
    created DATETIME NOT NULL,
    updated DATETIME NOT NULL
);
```

These columns correspond exactly to the fields listed on the [Pinboard API] page.

Why SQLite and not some plain-text format? In [the words of Paul Ford][Ford],

> SQLite is incredibly well-documented. It’s also instantly usable as a database from the command line with no pre-processing at all, even for very large files, and there are immediately usable SQLite APIs for every programming language. Plus it’s incredibly easy to turn SQLite data into plain text, it has freely available extensions for geo, full-text, and hierarchical data, and it’s tiny and public-domain.

[Ford]: https://trackchanges.postlight.com/usable-data-5d626d8a6b57

If you've used this application to back up your notes to a file called Notes.sqlite, you could use this quick and dirty Python script to print them in JSON format:

``` python
#!/usr/bin/env python3

from json import dumps
import sqlite3

conn = sqlite3.connect("Notes.sqlite")
conn.row_factory = sqlite3.Row
curs = conn.cursor()
curs.execute("SELECT * FROM notes")
print(dumps([dict(r) for r in curs.fetchall()]))
```

## Author

This program was created by [Benjamin Esham](https://esham.io).

This project is [hosted on GitHub](https://github.com/bdesham/pinboard-notes-backup). Please feel free to submit pull requests.

## Version history

See [CHANGELOG.md](CHANGELOG.md).

## License

Copyright © 2016–2017, 2019, 2021–2024 Benjamin D. Esham.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but **without any warranty;** without even the implied warranty of **merchantability** or **fitness for a particular purpose.** See the GNU General Public License for more details.

The GNU General Public License can be found in the file LICENSE.txt.
