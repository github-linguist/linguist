Point = Struct.new(:x,:y) do
  def to_s; "(#{x}, #{y})" end
end

def sutherland_hodgman(subjectPolygon, clipPolygon)
  # These inner functions reduce the argument passing to
  # "inside" and "intersection".
  cp1, cp2, s, e = nil
  inside = proc do |p|
    (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
  end
  intersection = proc do
    dcx, dcy = cp1.x-cp2.x, cp1.y-cp2.y
    dpx, dpy = s.x-e.x, s.y-e.y
    n1 = cp1.x*cp2.y - cp1.y*cp2.x
    n2 = s.x*e.y - s.y*e.x
    n3 = 1.0 / (dcx*dpy - dcy*dpx)
    Point[(n1*dpx - n2*dcx) * n3, (n1*dpy - n2*dcy) * n3]
  end

  outputList = subjectPolygon
  cp1 = clipPolygon.last
  for cp2 in clipPolygon
    inputList = outputList
    outputList = []
    s = inputList.last
    for e in inputList
      if inside[e]
        outputList << intersection[] unless inside[s]
        outputList << e
      elsif inside[s]
        outputList << intersection[]
      end
      s = e
    end
    cp1 = cp2
  end
  outputList
end

subjectPolygon = [[50, 150], [200, 50], [350, 150], [350, 300],
                  [250, 300], [200, 250], [150, 350], [100, 250],
                  [100, 200]].collect{|pnt| Point[*pnt]}

clipPolygon = [[100, 100], [300, 100], [300, 300], [100, 300]].collect{|pnt| Point[*pnt]}

puts sutherland_hodgman(subjectPolygon, clipPolygon)
