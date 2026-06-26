const std = @import("std");
const math = std.math;
const Vec2 = @import("vec2.zig");

pub const Boid = struct {
    const Self = @This();

    // Simulation tunables
    pub const PERCEPTION_RADIUS: f32 = 30.0;
    pub const SEPARATION_WEIGHT: f32 = 1.0;
    pub const ALIGNMENT_WEIGHT: f32 = 1.0;
    pub const COHESION_WEIGHT: f32 = 1.0;
    pub const MAX_SPEED: f32 = 5.0;
    pub const MIN_SPEED: f32 = 2.0;
    pub const MAX_STEER_FORCE: f32 = 1.5;
    pub const MAX_FOV: f32 = 135.0 * math.pi / 180.0;

    position: Vec2,
    velocity: Vec2,

    pub fn angleDegrees(self: Self) f32 {
        if (self.velocity.length() == 0) return 0;
        return math.atan2(self.velocity.y, self.velocity.x) * 180.0 / math.pi;
    }

    pub fn canSee(self: Self, other: Self) bool {
        const to_other = other.position.sub(self.position);
        const d = to_other.length();
        if (d >= PERCEPTION_RADIUS) return false;

        const dir_self = self.velocity.normalize();
        const dir_to_other = to_other.normalize();
        if (dir_to_other.length() == 0) return true;

        // Clamp before acos to avoid NaN from FP error.
        var c = dir_self.dot(dir_to_other);
        if (c > 1.0) c = 1.0;
        if (c < -1.0) c = -1.0;
        return math.acos(c) <= MAX_FOV;
    }

    pub fn step(self: Self, idx: usize, flock: []const Self, world_w: f32, world_h: f32) Self {
        var sep_sum = Vec2{ .x = 0, .y = 0 };
        var align_sum = Vec2{ .x = 0, .y = 0 };
        var coh_sum = Vec2{ .x = 0, .y = 0 };
        var n: f32 = 0;

        for (flock, 0..) |other, j| {
            if (j == idx) continue;
            if (!self.canSee(other)) continue;

            const diff = self.position.sub(other.position);
            const d = diff.length();
            if (d > 0) sep_sum = sep_sum.add(diff.scale(1.0 / (d * d)));
            align_sum = align_sum.add(other.velocity);
            coh_sum = coh_sum.add(other.position);
            n += 1;
        }

        var sep = Vec2{ .x = 0, .y = 0 };
        var ali = Vec2{ .x = 0, .y = 0 };
        var coh = Vec2{ .x = 0, .y = 0 };
        if (n > 0) {
            if (sep_sum.length() > 0) sep = sep_sum.normalize();
            const avg_vel = align_sum.scale(1.0 / n);
            if (avg_vel.length() > 0) ali = avg_vel.normalize();
            const center = coh_sum.scale(1.0 / n);
            const desired = center.sub(self.position);
            if (desired.length() > 0) coh = desired.normalize();
        }

        var steer = sep.scale(SEPARATION_WEIGHT)
            .add(ali.scale(ALIGNMENT_WEIGHT))
            .add(coh.scale(COHESION_WEIGHT));

        const sm = steer.length();
        if (sm > MAX_STEER_FORCE and sm > 0) steer = steer.scale(MAX_STEER_FORCE / sm);

        var v = self.velocity.add(steer);
        const vm = v.length();
        if (vm > MAX_SPEED and vm > 0) {
            v = v.scale(MAX_SPEED / vm);
        } else if (vm < MIN_SPEED and vm > 0) {
            v = v.scale(MIN_SPEED / vm);
        } else if (vm == 0) {
            v = .{ .x = MIN_SPEED / 2, .y = MIN_SPEED / 2 };
        }

        var p = self.position.add(v);
        if (p.x < 0) p.x += world_w;
        if (p.x > world_w) p.x -= world_w;
        if (p.y < 0) p.y += world_h;
        if (p.y > world_h) p.y -= world_h;

        return .{ .position = p, .velocity = v };
    }
};
