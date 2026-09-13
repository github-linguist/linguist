// Isometric render:   --scale 0.05 --pitch 80 --roll 130 --center=0,0,-14 --isometric
// Perspective render: --scale 0.05 --pitch 80 --roll 130 --center=0,0,-14 --perspective=0.2 --zflatten 2
let logs = x.abs() - 10 -  max(0.05, 1.0 - (y % 2 - 1).square()).sqrt();
let cabin = max(
    logs.remap(x, z, y),
    logs.remap(y, z, x)
);

let roof = z/2 + y.abs()/2 - 15;

// extrude the roof
let lower_roof = -roof;
let upper_roof = roof - 1 + (((-y.abs()/2 + z/2) % 1) - 0.5).abs() / 4;
let cabin = max(cabin, upper_roof);
let roof = max(lower_roof,  upper_roof);
let x_clamp = x.abs() - 13;
let roof = max(max(x_clamp, roof), 18 - z);

// Build a door frame
let door_frame = max(y.abs() - 5, z - 14);
let door_frame_inner = -(door_frame + 1.5);
let door_width = max(10 - x, x - 11);
let door_frame = max(max(door_frame, door_frame_inner),
    door_width);
let doorknob = (x.square() + y.square() + z.square()).sqrt() - 0.6;
let door = min(door_frame, doorknob.remap(x - 10.5, y - 2, z - 6));
let cabin = max(cabin, -max(-door_frame_inner, door_width));
let cabin = min(cabin, door);

// Build a window
let window_root = max(x.abs(), (z - 9).abs());
let cabin = max(cabin, -max(window_root - 4, 10 - y.abs())); // cut out window
let window_cross = max(window_root - 4, min(x.abs() - 0.2, (z - 9).abs() - 0.2));
let window_frame = max(window_root - 4, 3 - window_root);
let cabin = min(cabin, max(min(window_frame, window_cross), y.abs() - 11));
let cabin = min(cabin, max(window_root - 3, y.abs() - 10.6));

let cabin = min(max(-z, cabin), roof);
draw(cabin)
