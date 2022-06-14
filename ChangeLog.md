# Changelog for presentable

## Unreleased changes

## [0.1.0] - 2022-07-14

### Added

- Supported slideshow features
  - Title slide with a mandatory title and an optional subtitle
  - Bullet list slides supporting single, flat Markdown bullet lists
  - Copyright information though the `@copyright`-annotation
- Automatically fit to terminal dimensions
  - Split slides that are too long to fit
  - Stay in the same position
- Read default user configuration from file
  - Maximum slide dimensions
  - Text styles for
    - Slideshow title
    - Slideshow subtitle
    - Slide title
    - Bullets
    - Copyright information
    - Errors
- Validate a presentation without running it with the `--check` or `-c` flag
- Print the presentable version and exit with the `--version` or `-v` flag