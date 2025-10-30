glob expected_area = 78.53981633974483;

test a1 {
    assert almostEqual(calculate_area(RAD), expected_area);
}

test a2 {
    c = Circle(RAD);
    assert almostEqual(c.area(), expected_area);
}

test a3 {
    c = Circle(RAD);
    assert c.shape_type == ShapeType.CIRCLE;
}
