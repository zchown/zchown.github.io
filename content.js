// Loads a markdown file and populates the page from it.
//
// Markdown conventions:
//   # Title {#anchor}    -> new section, with id="anchor"
//   # Title              -> new section, id auto-derived from title
//   ## Subheading        -> sub-heading inside a section (h3)
//   blank line           -> block break
//   - item               -> list item (lines starting with `- ` form a list)
//   **bold** *italic*    -> inline emphasis
//   `code`               -> inline code
//   [text](url)          -> link
//   ![alt](url)          -> image

export async function loadContent({ url, contentEl, navEl }) {
    const md = await fetch(url).then(r => r.text());
    const sections = parseMarkdown(md);
    renderNav(sections, navEl);
    renderContent(sections, contentEl);
}

function parseMarkdown(md) {
    const sections = [];
    let current = null;

    for (const rawLine of md.split('\n')) {
        const line = rawLine.replace(/\s+$/, '');
        const heading = line.match(/^#\s+(.+?)(?:\s+\{#([^}]+)\})?\s*$/);
        if (heading) {
            if (current) sections.push(current);
            const title = heading[1].trim();
            const id = heading[2] || slugify(title);
            current = { title, id, body: '' };
        } else if (current) {
            current.body += rawLine + '\n';
        }
    }
    if (current) sections.push(current);
    return sections;
}

function slugify(s) {
    return s.toLowerCase()
        .replace(/[^a-z0-9]+/g, '-')
        .replace(/^-+|-+$/g, '');
}

function renderNav(sections, navEl) {
    navEl.innerHTML = '';
    for (const s of sections) {
        const li = document.createElement('li');
        const a = document.createElement('a');
        a.href = '#' + s.id;
        a.textContent = s.title;
        li.appendChild(a);
        navEl.appendChild(li);
    }
}

function renderContent(sections, contentEl) {
    contentEl.innerHTML = '';
    sections.forEach((s, i) => {
        const section = document.createElement('section');
        section.id = s.id;

        const heading = document.createElement(i === 0 ? 'h1' : 'h2');
        heading.textContent = s.title;
        section.appendChild(heading);

        for (const block of parseBlocks(s.body)) {
            section.appendChild(renderBlock(block));
        }

        contentEl.appendChild(section);
    });
}

function parseBlocks(body) {
    const blocks = [];
    for (const chunk of body.split(/\n\s*\n/)) {
        const trimmed = chunk.replace(/^\s+|\s+$/g, '');
        if (!trimmed) continue;
        const lines = trimmed.split('\n');

        if (lines.length === 1 && /^##\s+/.test(lines[0])) {
            blocks.push({
                type: 'subheading',
                text: lines[0].replace(/^##\s+/, '').trim(),
            });
            continue;
        }

        if (lines.every(l => /^\s*-\s+\S/.test(l))) {
            blocks.push({
                type: 'list',
                items: lines.map(l => l.replace(/^\s*-\s+/, '')),
            });
            continue;
        }

        blocks.push({ type: 'paragraph', text: lines.join(' ') });
    }
    return blocks;
}

function renderBlock(block) {
    if (block.type === 'subheading') {
        const h3 = document.createElement('h3');
        h3.textContent = block.text;
        return h3;
    }
    if (block.type === 'list') {
        const ul = document.createElement('ul');
        for (const item of block.items) {
            const li = document.createElement('li');
            li.innerHTML = inlineFormat(item);
            ul.appendChild(li);
        }
        return ul;
    }
    const p = document.createElement('p');
    p.innerHTML = inlineFormat(block.text);
    return p;
}

function escapeHtml(s) {
    return s.replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;');
}

function inlineFormat(s) {
    let out = escapeHtml(s);
    out = out.replace(/`([^`]+)`/g, '<code>$1</code>');
    out = out.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
    out = out.replace(/\*([^*]+)\*/g, '<em>$1</em>');
    out = out.replace(/!\[([^\]]*)\]\(([^)\s]+)\)/g, '<img src="$2" alt="$1">');
    out = out.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, '<a href="$2">$1</a>');
    return out;
}
