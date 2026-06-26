export async function startBoids({ svgId, wasmUrl }) {
    const bytes = await fetch(wasmUrl).then(r => r.arrayBuffer());
    const { instance } = await WebAssembly.instantiate(bytes, {});
    const wasm = instance.exports;
    const mem = wasm.memory;

    let w = window.innerWidth;
    let h = window.innerHeight;
    const seed = (Math.random() * 0xFFFFFFFF) >>> 0;
    wasm.init(w, h, seed);

    const svg = document.getElementById(svgId);
    svg.setAttribute('viewBox', `0 0 ${w} ${h}`);

    const n = Number(wasm.getNumBoids());
    const boidEls = [];
    for (let i = 0; i < n; i++) {
        const p = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
        p.setAttribute('points', '0,-8 32,0 0,8');
        p.setAttribute('fill', '#D6DEEB');
        p.setAttribute('stroke', '#7E57C25A');
        p.setAttribute('stroke-width', '5');
        svg.appendChild(p);
        boidEls.push(p);
    }

    function loop() {
        wasm.tick();
        const ptr = Number(wasm.getRenderBuffer());
        const data = new Float32Array(mem.buffer, ptr, n * 3);
        for (let i = 0; i < n; i++) {
            const x = data[i * 3];
            const y = data[i * 3 + 1];
            const a = data[i * 3 + 2];
            boidEls[i].setAttribute('transform', `translate(${x},${y}) rotate(${a})`);
        }
        requestAnimationFrame(loop);
    }

    window.addEventListener('resize', () => {
        w = window.innerWidth;
        h = window.innerHeight;
        wasm.setSize(w, h);
        svg.setAttribute('viewBox', `0 0 ${w} ${h}`);
    });

    requestAnimationFrame(loop);
}
