const Vec2 = @import("vec2.zig");
const Boid = @import("boid.zig").Boid;
const std = @import("std");

const NUM_BOIDS: usize = 200;

var flock: [NUM_BOIDS]Boid = undefined;
var scratch: [NUM_BOIDS]Boid = undefined;
var world_w: f32 = 1024;
var world_h: f32 = 768;

var render_buffer: [NUM_BOIDS * 3]f32 = undefined;

var rng: u32 = 1;

fn nextRand() f32 {
    rng = rng *% 1664525 +% 1013904223;
    return @as(f32, @floatFromInt(rng)) / 4294967295.0;
}

fn randRange(lo: f32, hi: f32) f32 {
    return lo + nextRand() * (hi - lo);
}

export fn init(width: f32, height: f32, seed: u32) void {
    world_w = width;
    world_h = height;
    rng = if (seed == 0) 1 else seed;

    for (&flock) |*b| {
        const angle = randRange(0, 2 * std.math.pi);
        const speed = randRange(Boid.MIN_SPEED, Boid.MAX_SPEED);
        b.* = .{
            .position = .{ .x = randRange(0, width), .y = randRange(0, height) },
            .velocity = .{ .x = speed * @cos(angle), .y = speed * @sin(angle) },
        };
    }
}

export fn setSize(width: f32, height: f32) void {
    world_w = width;
    world_h = height;
}

export fn getNumBoids() usize {
    return NUM_BOIDS;
}

export fn getRenderBuffer() usize {
    return @intFromPtr(&render_buffer);
}

export fn tick() void {
    for (flock, 0..) |b, i| {
        scratch[i] = b.step(i, flock[0..], world_w, world_h);
    }
    flock = scratch;

    for (flock, 0..) |b, i| {
        render_buffer[i * 3 + 0] = b.position.x;
        render_buffer[i * 3 + 1] = b.position.y;
        render_buffer[i * 3 + 2] = b.angleDegrees();
    }
}
