# pinboard-notes-backup

Efficiently back up the notes you’ve saved to [Pinboard].

[Pinboard]: https://pinboard.in

## Installation

This is a Haskell program that you can build using [Stack]. With Stack installed, just type

    stack install

to build the `pnbackup` binary and install it. (The default installation directory is `~/.local/bin`; you may want to copy the executable from there into some directory that is listed in your `PATH`.)

[Stack]: http://docs.haskellstack.org/en/stable/README/

## Usage

First you’ll need to grab your Pinboard API token from Pinboard’s [password settings] page. It’s a string like “maciej:abc123456”.

To save your notes to a file called “Notes.sqlite”, run

    pnbackup -t maciej:abc123456 Notes.sqlite

replacing the example API token with your own. This will put all of your notes into a SQLite database called “Notes.sqlite”.

Each time you run the program, it will fetch the list of your notes from the Pinboard server. It will then download any notes that are new or that have been updated on the server, and it will delete any notes that have been deleted from the server. The program does one-way synchronization only: it will update your local database to match what’s on the server but it will never make any changes on the server.

The [Pinboard API][API] requires a three-second wait time between each request, and the text of each note must be downloaded in a separate request, so the initial download of your notes may take a while. Subsequent syncs should be much shorter, though. If you want to see exactly what `pnbackup` is doing as it works, pass it the `-v` or `--verbose` flags.

[password settings]: https://pinboard.in/settings/password
[API]: https://pinboard.in/api/

## Author

This program was created by [Benjamin Esham](https://esham.io).

This project is [hosted on GitHub](https://github.com/bdesham/pinboard-notes-backup). Please feel free to submit pull requests.

## Version history

* 1.0.0 (2016-06-17): Initial release.

## License

Copyright © 2016 Benjamin D. Esham.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but **without any warranty;** without even the implied warranty of **merchantability** or **fitness for a particular purpose.** See the GNU General Public License for more details.

The GNU General Public License can be found in the file LICENSE.txt.
