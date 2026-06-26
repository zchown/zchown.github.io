# Zander Chown {#home}

I'm a CS PhD student at the University of Oregon, working on satellite
networking research with Prof. Ram Durairajan in the Oregon Networking
Research Group.

Outside of research I tend to build things that don't strictly need to
exist — a chess engine in Zig, Tak engines and a 3D Tak UI, a few Neovim
plugins, a graphics experiment or two. Mostly Zig lately.

If you want the long version, here's my [CV (PDF)](./cv.pdf).

# Research {#research}

Right now I'm thinking about LEO satellite constellations: how to
schedule when satellites get deployed and decommissioned, and how to
design constellation geometry around real network demand instead of
blanketing the sky. I work with Ram Durairajan and Chris Misa.

## Publications

- *SS-Plane: A Heliocentric Satellite Constellation for Demand-Aware Networking* — under review (2026)
- *Falling Into Line: Scheduling (Re)Deployment and Transition in LEO Satellite Constellations* — under review (2026)
- *[Privacy Norms for Fertility Data in the Roe v. Wade Era](https://dl.acm.org/doi/pdf/10.1145/3658644.3691406)* — ACM CCS 2024 (poster)

Before I got into networks, I spent a few years at Skidmore leading an
undergraduate research project on the privacy and security of
period-tracking and fertility apps. We used the contextual integrity
framework to figure out what users actually expect, ran an IRB-approved
survey of several hundred people, and prototyped a multi-party
computation approach to handling fertility data without trusting the
server.

# Projects {#projects}

**[Ursus](https://github.com/zchown/Ursus)** — A UCI chess engine I
wrote in Zig. Hovers around 3300 Elo and plays on Lichess as
[@Ursus_bot](https://lichess.org/@/Ursus_bot). Alpha-beta with an NNUE
eval trained from self-play, plus the usual modern search tricks —
null-move pruning, futility pruning, late-move reductions, singular
extensions — all verified with SPRT.

**Tak** — The abstract strategy game Tak, kind of everywhere. There's
*Haliax*, a TEI-compliant Tak engine in Zig that plays on playtak.com
with an optimized alpha-beta search and a hand-crafted evaluation.
There's a 3D web UI in BabylonJS with a Haskell server that brokers
between human and AI players. And there's an MCTS AI in C with a
neural-network policy and value function trained from self-play,
compiled to Apple's Accelerate framework for fast on-device inference.

**Neovim plugins**

- [Continuity.nvim](https://github.com/zchown/Continuity.nvim) — automatic per-project session management. Saves and restores my editing sessions so I never lose where I was.
- [nvim-ipynb](https://github.com/zchown/nvim-ipynb) — runs and reports Python results notebook-style without leaving Neovim.

**MindMNIST** — A team project classifying EEG data with Python, NumPy,
and TensorFlow. Cleaned the signals with FFT-based bandpass filtering
and then compared CNNs against simpler baselines (K-means, KNN).
[Writeup on Medium](https://medium.com/@caelenhilty/mindnist-29f6fdda948d).

**Weather Wear** — An iOS app that mashes up weather, alarms, and
clothing recommendations. Swift, real-time weather APIs, the classic
OO patterns. A team project from undergrad.

**[World Terrain Generation](https://benno-lueders.de/dis/computer-graphics/2024spring-terrainmorph/)** —
A collaborative 3D graphics project from DIS Copenhagen — custom GLSL
shaders for dynamic lighting and texturing of procedural terrain.

# Teaching {#teaching}

- *Graduate Teaching Assistant*, UO (Fall 2025) — taught Unix and C lab sessions for undergrads, wrote and graded exams.
- *Peer Academic Coach*, Skidmore (2023–2025) — one-on-one tutoring, group sessions, and workshops for CS students.
- *TA for Intro to CS*, Skidmore (Fall 2022 & 2024) — office hours and one-on-one help.

# Contact {#contact}

The best way to reach me is email at
[zchown@uoregon.edu](mailto:zchown@uoregon.edu). My code lives on
[github.com/zchown](https://github.com/zchown), and you can grab my CV
as a PDF [here](./cv.pdf).
