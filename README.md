# pinboard-notes-backup [![Build Status](https://travis-ci.org/bdesham/pinboard-notes-backup.svg?branch=master)](https://travis-ci.org/bdesham/pinboard-notes-backup)

Back up the notes you’ve saved to [Pinboard].

[Pinboard]: https://pinboard.in

## Installation

If you’re using [Homebrew], you can just run

    brew install pinboard-notes-backup

Prebuilt binaries are available for OS X and Linux—find them on [the page for the most recent release][release]. Download the archive for your OS, unpack it, and copy the `pnbackup` binary to some directory in your `PATH`, like `/usr/local/bin`. You may also want to copy the man page, `pnbackup.1`, to a directory like `/usr/local/share/man/man1`.

[Homebrew]: https://brew.sh
[release]: https://github.com/bdesham/pinboard-notes-backup/releases/latest

### Building from source

This is a Haskell program that you can build using [Stack]. With Stack installed, just type

    stack setup

to download and install GHC (the Haskell compiler) and then

    stack install

to build the `pnbackup` binary and install it. (The default installation directory is `~/.local/bin`; you probably want to copy the executable from there into some directory that is listed in your `PATH`.)

[Stack]: http://docs.haskellstack.org/en/stable/README/

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

```
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

These version numbers approximately follow the [Haskell Package Versioning Policy (PVP)][PVP]. (I say “approximately” because this package contains no libraries—only an executable—and so it does not actually provide an API per se.)

[PVP]: https://pvp.haskell.org/

* 1.0.4 (2019-06-22)
    - Error messages should now be much more comprehensible, especially for the most common network errors.
    - Progress messages now go to standard output, not standard error.
    - The program now has a man page.
    - A few dependency changes mean that the binary should be smaller.
* 1.0.3 (2017-11-05): The program now builds against a wider set of dependencies. This will allow it to be installed using [Homebrew]. (There were no changes in functionality.)
* 1.0.2 (2016-12-05): If any notes had been changed on the server then the application would error out instead of updating the local copy.
* 1.0.1 (2016-07-20): Tweaked the help text and made some under-the-hood changes.
* 1.0.0 (2016-06-28): Initial release.

## License

The scripts in the “travis_scripts” folder were adapted from scripts written by Taylor Fausak for the [Octane] project. They are released under [the license of that project][Octane license].

[Octane]: https://github.com/tfausak/octane
[Octane license]: https://github.com/tfausak/octane/blob/04ea434f476d30c3c8327d8ed9afdc6ae246f2ae/LICENSE.markdown

The rest of the code in this repository is released under the following terms:

Copyright © 2016–2017, 2019 Benjamin D. Esham.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but **without any warranty;** without even the implied warranty of **merchantability** or **fitness for a particular purpose.** See the GNU General Public License for more details.

The GNU General Public License can be found in the file LICENSE.txt.
