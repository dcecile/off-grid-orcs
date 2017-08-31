# Off-Grid Orcs
_A pixelated real-time strategy game for #LOWREZJAM 2017_

## Playing

Visit the game's homepage at Itch.io to play in your browser:

> https://dcecile.itch.io/off-grid-orcs

## Developing

### Prerequisites

You'll need to install a JDK (to run the Scala compiler) and
[SBT](http://www.scala-sbt.org/) (to download the Scala compiler and
library dependencies).

### Workflow

Here are some important `sbt` commands:

- `~fastOptJS` (to continuously compile without optimization)
- `~fullOptJS` (to continuously compile __with__ optimization)
- `~test-quick` (to run only affected tests, and run them again when any
  source file changes)

Open `index.html` in your browser to play using the compiled JavaScript.

### Font changes

The font is set up via
[git-subrepo](https://github.com/ingydotnet/git-subrepo). Any changes to
the font need to be synchronized via `git subrepo pull` and `git subrepo
push`.

## License

This project is released under the MIT License (see
[LICENSE.md](LICENSE.md) for details).
