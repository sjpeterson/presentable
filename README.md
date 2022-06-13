# Presentable

Presentable is a tool for creating minimalistic slideshows from markdown files.
It is written in [Haskell](https://www.haskell.org/)
using [brick](https://github.com/jtdaugherty/brick/) for the presentation layer.

## Example

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

- Slideshow files should read well both raw and formatted on e.g. GitHub
- All annotations should be optional, to use more advanced features
- Configuration should make sense regardless of programming background, i.e. no
  functional-specific terms or structures