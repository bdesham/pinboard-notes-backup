% PNBACKUP(1) pinboard-notes-backup

# NAME

**pinboard-notes-backup** -- Back up the notes you've saved to Pinboard

# SYNOPSIS

**pnbackup** `[-v | --verbose] -t <username>:<api token> <output file>`

# OPTIONS

The following parameters must always be given:

`-t <username>:<api token>`
: Your Pinboard username, a colon, and your Pinboard API token. You can find this on the Pinboard password settings page (https://pinboard.in/settings/password).

`<output file>`
: The name of the SQLite database file where your notes will be stored. This file will be created if it doesn't yet exist.

The following options are available:

`-v, --verbose`
: Display more information about the sync progress.

`-h, --help`
: Display the usage information and exit.

`--version`
: Display the version number and exit.

# DESCRIPTION

The program performs one-way synchronization between Pinboard and a local database: it will update your local file to match what's on the server but it will never make any changes on the server. Notes will be created, modified, or deleted as appropriate in your local database to match what's on the server. If the specified database file doesn't exist, pnbackup will create it for you.

The Pinboard API requires a three-second wait between each request, and the text of each note must be downloaded in a separate request, so the initial download of your notes may take a while. Subsequent syncs will generally be much shorter, depending on how often you add or modify notes and how often you run pnbackup.

# EXIT STATUS

pnbackup will return zero on success or nonzero on failure.

# EXAMPLE

`pnbackup -t maciej:abc123456 Notes.sqlite`

: will put all of your notes into a SQLite database called "Notes.sqlite", assuming that your Pinboard username is "maciej" and your API token is "abc123456".

# DATA FORMAT

The notes are stored in a SQLite database. The database contains a single table, "notes", which has the following schema:

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

These columns correspond exactly to the fields listed on the Pinboard API page (https://pinboard.in/api/).

SQLite is a binary format but there is a large ecosystem of tools that can work with it and, if you prefer, convert it to some text-based format instead. For example, if you've backed up your notes to a file called Notes.sqlite, you could use this quick and dirty Python script to print them in JSON format:

```
#!/usr/bin/env python3
from json import dumps
import sqlite3
conn = sqlite3.connect("Notes.sqlite")
conn.row_factory = sqlite3.Row
curs = conn.cursor()
curs.execute("SELECT * FROM notes")
print(dumps([dict(r) for r in curs.fetchall()]))
```

# AUTHOR

This program was created by Benjamin Esham (https://esham.io).

# WEBSITE

The project is hosted at <https://github.com/bdesham/pinboard-notes-backup>.

# LICENSE

Copyright © 2016–2017, 2019, 2021 Benjamin D.\ Esham.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but **without any warranty;** without even the implied warranty of **merchantability** or **fitness for a particular purpose.** See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.
