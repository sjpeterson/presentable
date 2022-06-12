# Presentable roadmap

## 0.1.0 - Simple bullet list slides
- [x] Parse markdown
  - [x] Presentation title (Markdown Heading 1)
  - [x] Presentation subtitle (text line following title)
  - [x] Presentation author (top annotation @copyright)
  - [x] Slide title (Markdown heading 2)
  - [x] Bullet list (Markdown bullet list following a heading 2)
    - [x] Flat
    - [x] Nested
- [x] Present
  - [x] Copyright
  - [x] Title page
    - [x] Title, subtitle centered in the terminal
  - [x] Bullet list slide
    - [x] Left-aligned header
    - [x] Left-aligned bullets
      - [x] Flat
- [ ] Resize/fit
  - [x] Fit to initial size
  - [x] Split bullet list slides to fit
  - [x] Show error if unable to fit
  - [ ] Stay in same position
- [x] Navigation
  - [x] Press escape to exit
  - [x] Press h, right arrow or Enter to advance
  - [x] Press l, left arrow or backspace to go back
- [x] Configuration
  - [x] read from $XDG_CONFIG_HOME/default.yml if it exists
  - [x] max dimensions
  - [x] title style
  - [x] subtitle style
  - [x] bullet style
  - [x] error style
- [x] Options
  - [x] --check or -c to validate the slideshow file without presenting
  - [x] --version or -v to print the version

## 0.1.1 - Nested bullet lists
- [ ] Parse markdown
  - [ ] Bullet list (Markdown bullet list following a heading 2)
    - [ ] Nested
- [ ] Parse markdown
  - [ ] Bullet list slide
    - [ ] Left-aligned bullets
      - [ ] Nested
- [ ] Resize/fit
  - [ ] Split only on top-level items

## 0.1.2 - Plain text slides
- [ ] Parse markdown
  - [ ] Plain text blocks
- [ ] Present
  - [ ] Plain text slide
    - [ ] Left-aligned header
    - [ ] Left-aligned text blocks
- [ ] Resize/fit
  - [ ] Split plain text slides to fit, keeping text blocks intact

## Not yet prioritized

- [ ] Tables
- [ ] Code examples
- [ ] Runnable code examples
- [ ] Figures
- [ ] Graphs