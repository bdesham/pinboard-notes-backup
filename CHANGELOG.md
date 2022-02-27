# CHANGELOG for pinboard-notes-backup

These version numbers approximately follow the [Haskell Package Versioning Policy (PVP)][PVP]. (I say “approximately” because this package contains no libraries—only an executable—and so it does not actually provide an API per se.)

[PVP]: https://pvp.haskell.org/

## 1.0.5.4 (2022-02-26)

The program now builds against a wider set of dependencies. There were no changes in functionality.

## 1.0.5.3 (2021-03-24)

The program now builds against a wider set of dependencies. There were no changes in functionality.

## 1.0.5.2 (2021-02-28)

The program now builds against a wider set of dependencies. There were no changes in functionality.

## 1.0.5.1 (2021-02-27)

The program now builds against a wider set of dependencies. There were no changes in functionality.

## 1.0.5 (2019-09-15)

- The Bash and Zsh completion scripts are now told explicitly that the “FILE” argument should be completed with a filename. (This seems to have been the default behavior for both shells anyway, so this may not represent an actual change in functionality.)
- The application now builds against a wider range of dependencies.

## 1.0.4.1 (2019-07-19)

- The “built” version of the man page is now included in version control (in the `man` directory) and in the package produced by `cabal sdist`/`stack sdist`. (It was previously necessary to create this yourself by feeding the Markdown source file to Pandoc.)
- Changed the categories and the description in the cabal file.
- There were no changes to the code.

## 1.0.4 (2019-06-22)

- Error messages should now be much more comprehensible, especially for the most common network errors.
- Progress messages now go to standard output, not standard error.
- The program now has a man page.
- A few dependency changes mean that the binary should be smaller.

## 1.0.3 (2017-11-05)

The program now builds against a wider set of dependencies. This will allow it to be installed using [Homebrew]. (There were no changes in functionality.)

[Homebrew]: https://brew.sh

## 1.0.2 (2016-12-05)

If any notes had been changed on the server then the application would error out instead of updating the local copy.

## 1.0.1 (2016-07-20)

Tweaked the help text and made some under-the-hood changes.

## 1.0.0 (2016-06-28)

Initial release.
