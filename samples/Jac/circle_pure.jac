"""
This module demonstrates a simple circle class and a function to calculate
the area of a circle in all of Jac's glory.
"""

import math;

enum ShapeType;

def calculate_area(radius: float) -> float;
def main_run;

"""Base class for a shape."""
obj : priv Shape {
    has shape_type: ShapeType;

    def area -> float abs;
}

"""Circle class inherits from Shape."""
obj Circle(Shape) {
    has radius: float;

    def init(radius: float);
    override def area -> float;
}
# Radius of the demo circle

glob RAD = 5, c = Circle(radius=RAD);

"""Here we run the main program."""
with entry:__main__ {
    main_run();
}
