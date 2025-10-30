"""Enum for shape types"""

impl ShapeType {
    CIRCLE = "Circle",
    UNKNOWN = "Unknown"
}

"""Function to calculate the area of a circle."""
impl calculate_area
(radius: float) -> float {
    return math.pi * radius * radius;
}

impl Circle.init
(radius: float) {
    self.radius = radius;
    super.init(ShapeType.CIRCLE);
}

"""Overridden method to calculate the area of the circle."""
impl Circle.area -> float {
    return math.pi * self.radius * self.radius;
}

impl main_run {
    print(
        f"Area of a circle with radius {RAD} using function: {calculate_area(RAD)}"
    );
    print(
        f"Area of a {c.shape_type.value} with radius {RAD} using class: {c.area()}"
    );
}
