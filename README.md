# `Presentable`

A terminal application that lets you write slideshows in Markdown. The project
is still very much in its infancy, so the current feature set is small but the
ambitions are great.

## Installation

Presentable is written in [Haskell](https://www.haskell.org/)
using [stack](https://docs.haskellstack.org/en/stable/README/). If you do not
already have stack installed,
follow [these instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install)
, then simply

    git clone https://github.com/sjpeterson/presentable && cd presentable
    stack install

Be aware that the initial build time is often quite long.

Presentable is developed for and tested on Linux but should likely work on macOS
as well.

## Usage

Presentations are written in markdown with a few specific annotations.

    @copyright 2022 Author Example

    # Presentable

    Minimalist slideshows from readable markdown

    ## Bullet list
    
    - First item
    - Second item
    - Third item

You run the slideshow in your terminal by simply calling Presentable with the
path to the markdown file:

    presentable example/simple.md

You can check that the slideshow can be parsed by Presentable without running it
by using the `--check`- or `-c`-flag:

    presentable -c example/simple.md

## Design goals

The development of Presentable is guided by the following goals:

- Slideshow files should read well both raw and formatted on e.g. GitHub, 
  meaning
  - Annotations must be optional
  - Annotations must be readable
  - Annotations should be as sparse as possible
- Configuration should make sense regardless of programming background, i.e. no
  functional-specific terms or structures.

## Configuration

User-defined configuration can be written to 
`$XDG_CONFIG_HOME/presentable/default.yml`, for example

    draw:
      maxColumns: 80
      maxRows: 22

    styles:
      title:
        color: blue
        bold: yes
      slideTitle:
        color: blue
        bold: yes
      bullet:
        color: blue
        bold: yes
      error:
        color: red
        bold: yes
        italic: yes

The properties of `draw` should be self-explanatory. Supported members
of `styles` are

 - `title`
 - `subtitle`
 - `copyright`
 - `slideTitle`
 - `bullet`
 - `error`

and for each style, the configurable properties are

- `color` (one of `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`,
  and `white`)
- `bold`
- `italic`
 