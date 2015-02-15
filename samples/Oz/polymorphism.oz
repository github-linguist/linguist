class Point
   feat
      x
      y

   meth init(x:X<=0.0 y:Y<=0.0)
      self.x = X
      self.y = Y
   end

   meth print
      {System.showInfo
       "Point("#
       "x:"#self.x#
       ", y:"#self.y#
       ")"}
   end
end

class Circle
   feat
      center
      r

   meth init(center:C<={New Point init} r:R<=1.0)
      self.center = C
      self.r = R
   end

   meth print
      {System.showInfo
       "Circle("#
       "x:"#self.center.x#
       ", y:"#self.center.y#
       ", r:"#self.r#
       ")"}
   end
end
