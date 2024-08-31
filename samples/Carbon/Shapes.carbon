package Shapes api;
import Math;

// Circle

class Circle {
    var Radius: f32 = 1;
    const var Diameter: f32 = self.Radius * 2;

    const var Pi: f32 = Math.Pi;

    fn Area() -> self;
    fn Circumference() -> self;
}

fn Circle.Area() -> self {
    return Math.Pi * .Radius ^ 2
}

fn Circle.Circumference() -> self {
    return 2 * Math.Pi * .Radius
}

// Rectangle

class Rectangle {
    var Width: f32 = 3;
    var Height: f32 = 1;

    fn Area() -> self;
}

fn Rectangle.Area() -> self {
    return .Width * .Height;
}

// Square (Note: Provides same functions as "Rectangle" class.)

class Square {
    var Width: f32 = 3;
    var Height: f32 = 1;

    fn Area() -> self;
}

fn Square.Area() -> self {
    return .Width * .Height;
}

// Triangle

class Triangle {
    var Width: f32 = 3;
    var Height: f32 = 3;

    fn Area() -> self;
}

fn Triangle.Area() -> self {
    return (.Width * .Height) / 2;
}

// Hexagon

class Hexagon {
    var Side: f32 = 5;

    fn Area() -> self;
}

fn Hexagon.Area() -> self {
    return ((3 * 1.732) / 2) * .Side ^ 2
}
