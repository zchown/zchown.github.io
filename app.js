import { startBoids } from './boids.js';
import { loadContent } from './content.js';

startBoids({
    svgId: 'boids-svg',
    wasmUrl: './zig-out/bin/boids.wasm',
}).catch(err => console.error('Boids failed to start:', err));

loadContent({
    url: './content.md',
    contentEl: document.getElementById('content'),
    navEl: document.getElementById('nav-list'),
}).catch(err => console.error('Content failed to load:', err));
