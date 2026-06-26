# zander-chown.com (Zig + WASM)

The boids simulation runs in WebAssembly compiled from Zig; the page is plain HTML/CSS; the text content is driven by a single markdown file.

## Files

```
website/
├── build.zig            # builds the WASM module
├── src/
│   ├── vec2.zig         # pure 2D vector math
│   ├── boid.zig         # Boid struct + flocking behavior
│   └── main.zig         # world state + JS-facing exports
├── content.md           # site text content (sections, nav)
├── index.html           # minimal scaffolding
├── style.css            # styles
├── app.js               # entry point — boots the two subsystems
├── boids.js             # WASM loader + animation loop
└── content.js           # markdown loader + renderer
```

## Build

Requires Zig 0.15.2, because of Zigs pre 1.0 status no other version is guaranteed to work.

```
zig build
```

Produces `zig-out/bin/boids.wasm`. `boids.js` fetches that path directly,
so no copying is needed.

## Run

```
python3 -m http.server 8000
```

Then open <http://localhost:8000>.

## Editing the content

Everything text-related lives in `content.md`. Each section begins with a
`#` heading. The `{#anchor}` suffix is optional — without it, the anchor is
auto-derived from the heading text.

```markdown
# Zander Chown {#home}

Some intro text.

# About

Auto-anchored to "about".
```

The first section renders as the page title (large `<h1>`); the rest render
as section headings (`<h2>`). Nav links are generated from the section list
in order, so adding a section to the markdown automatically adds it to the nav.

Supported inline formatting: `**bold**`, `*italic*`, `` `code` ``,
`[text](url)`, `![alt](url)` (image).

## Images

Drop an image file anywhere under the website folder (e.g.
`images/me.jpg`) and reference it with standard markdown syntax:

```markdown
![Photo of me](./images/me.jpg)
```

Images render centered with rounded corners and capped at the section
width. Image-as-link works too: `[![alt](thumb.jpg)](https://full-url)`.

## CV

The home and contact sections link to `./cv.pdf`. Drop a compiled PDF
named `cv.pdf` next to `index.html` and it'll be served alongside the
site. (If you don't want a CV link, just remove the two references in
`content.md`.)

## Architecture

Three concerns, three boundaries:

- **`src/vec2.zig`** — generic 2D vector math, reusable in any project.
- **`src/boid.zig`** — a `Boid` knows how to compute its next state given
  the current flock and a world size. No globals, no WASM, no I/O.
- **`src/main.zig`** — owns the flock array, the RNG, and the render buffer.
  This is the only file with `export` functions.

On the JS side:

- **`boids.js`** loads the WASM, creates one `<polygon>` per boid, and per
  frame reads `(x, y, angle)` triples out of the WASM render buffer and
  applies them as SVG transforms.
- **`content.js`** fetches `content.md`, parses sections, and populates
  both the nav and the content area.
- **`app.js`** is just the entry — it imports both modules and starts them.

The two subsystems are independent; you can swap either out without touching
the other.

## Tweaking the flock

Simulation constants (`PERCEPTION_RADIUS`, weights, speed limits, etc.) are
at the top of `src/boid.zig`. `NUM_BOIDS` is in `src/main.zig`. Re-run
`zig build` after changing any of them.
