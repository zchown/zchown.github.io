const Self = @This();

x: f32,
y: f32,

pub fn add(self: Self, other: Self) Self {
    return .{ .x = self.x + other.x, .y = self.y + other.y };
}

pub fn sub(self: Self, other: Self) Self {
    return .{ .x = self.x - other.x, .y = self.y - other.y };
}

pub fn scale(self: Self, s: f32) Self {
    return .{ .x = self.x * s, .y = self.y * s };
}

pub fn length(self: Self) f32 {
    return @sqrt(self.x * self.x + self.y * self.y);
}

pub fn normalize(self: Self) Self {
    const l = self.length();
    return if (l > 0) self.scale(1.0 / l) else .{ .x = 0, .y = 0 };
}

pub fn dot(self: Self, other: Self) f32 {
    return self.x * other.x + self.y * other.y;
}
