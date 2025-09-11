"""
This module demonstrates a simple circle class and a function to calculate
the area of a circle in all of Jac's glory.
"""

import math;
# Module-level global var

glob RAD = 5;

"""Function to calculate the area of a circle."""
def calculate_area(radius: float) -> float {
    return math.pi * radius * radius;
}
#* (This is multiline comments in Jac)
Above we have the demonstration of a function to calculate the area of a circle.
Below we have the demonstration of a class to calculate the area of a circle.
*#

"""Enum for shape types"""
enum ShapeType {
    CIRCLE = "Circle",
    UNKNOWN = "Unknown"
}

"""Base class for a shape."""
obj Shape {
    has shape_type: ShapeType;

    """Abstract method to calculate the area of a shape."""
    def area -> float abs;
}

"""Circle class inherits from Shape."""
obj Circle(Shape) {
    def init(radius: float) {
        super.init(ShapeType.CIRCLE);
        self.radius = radius;
    }

    """Overridden method to calculate the area of the circle."""
    override def area -> float {
        return math.pi * self.radius * self.radius;
    }
}

with entry {
    c = Circle(RAD);
}
# Global also works here

with entry:__main__ {
    # To run the program functionality
    print(
        f"Area of a circle with radius {RAD} using function: {calculate_area(RAD)}"
    );
    print(
        f"Area of a {c.shape_type.value} with radius {RAD} using class: {c.area()}"
    );
}
# Unit Tests!

glob expected_area = 78.53981633974483;

test calc_area {
    check almostEqual(calculate_area(RAD), expected_area);
}

test circle_area {
    c = Circle(RAD);
    check almostEqual(c.area(), expected_area);
}

test circle_type {
    c = Circle(RAD);
    check c.shape_type == ShapeType.CIRCLE;
}