# Presentable roadmap

## 0.1.0 - Simple bullet list slides
- [x] Parse markdown
  - [x] Presentation title (Markdown Heading 1)
  - [x] Presentation subtitle (text line following title)
  - [x] Presentation author (top annotation @copyright)
  - [x] Slide title (Markdown heading 2)
  - [x] Bullet list (Flat Markdown bullet list following a heading 2)
- [x] Present
  - [x] Copyright
  - [x] Title page
    - [x] Title, subtitle centered in the terminal
  - [x] Bullet list slide
    - [x] Left-aligned header
    - [x] Left-aligned flat bullet list items
- [x] Resize/fit
  - [x] Fit to initial size
  - [x] Split bullet list slides to fit
  - [x] Show error if unable to fit
  - [x] Stay in same position
- [x] Navigation
  - [x] Press escape to exit
  - [x] Press h, right arrow or Enter to advance
  - [x] Press l, left arrow or backspace to go back
- [x] Configuration
  - [x] read from $XDG_CONFIG_HOME/default.yml if it exists
  - [x] max dimensions
  - [x] title style
  - [x] subtitle style
  - [x] slide title style
  - [x] bullet style
  - [x] error style
  - [x] copyright notice style
- [x] Options
  - [x] --check or -c to validate the slideshow file without presenting
  - [x] --version or -v to print the version

## 0.2.0 - Nested bullet lists
- [x] Parse markdown
  - [x] Nested bullet list (Markdown bullet list following a heading 2)
- [x] Present
  - [x] Bullet list slide
    - [x] Left-aligned bullets
      - [x] Nested
- [x] Resize/fit
  - [x] Split only on top-level items

## 0.3.0 - Plain text slides
- [x] Parse markdown
  - [x] Plain text blocks
- [x] Present
  - [x] Plain text slide
    - [x] Left-aligned header
    - [x] Left-aligned text blocks
- [x] Resize/fit
  - [x] Split plain text slides to fit, keeping text blocks intact

## Not yet prioritized

- [ ] Inline formatting (bold, italic)
  - [ ] Full words
  - [ ] Part of word
- [ ] Ordered lists
- [ ] Tables
- [ ] Code examples
- [ ] Runnable code examples
- [ ] Figures
- [ ] Graphs