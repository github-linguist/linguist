package Shapes;
import Math;

// Circle

class Circle {
    var Radius: f32;
    var Diameter: f32 = self.Radius * 2;

    fn Area(self) -> f32;
    fn Circumference(self) -> f32;
}

fn Circle.Area(self) -> f32 {
    return Math.Pi * self.Radius * self.Radius;
}

fn Circle.Circumference(self) -> f32 {
    return 2 * Math.Pi * self.Radius;
}

// Rectangle

class Rectangle {
    var Width: f32;
    var Height: f32;

    fn Area(self) -> f32;
}

fn Rectangle.Area(self) -> f32 {
    return self.Width * self.Height;
}

// Square (Note: Provides same functions as "Rectangle" class.)

class Square {
    var Width: f32;
    var Height: f32;

    fn Area(self) -> f32;
}

fn Square.Area(self) -> f32 {
    return self.Width * self.Height;
}

// Triangle

class Triangle {
    var Width: f32;
    var Height: f32;

    fn Area(self) -> f32;
}

fn Triangle.Area(self) -> f32 {
    return (self.Width * self.Height) / 2;
}

// Hexagon

class Hexagon {
    var Side: f32;

    fn Area(self) -> f32;
}

fn Hexagon.Area(self) -> f32 {
    return ((3 * 1.732) / 2) * self.Side * self.Side;
}
