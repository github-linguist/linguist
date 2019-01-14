function [xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5] = Lagr(mu)
% [xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5] = Lagr(mu)
% Lagr This function computes the coordinates of the Lagrangian points,
% given the mass parameter
yl1=0;
yl2=0;
yl3=0;
yl4=sqrt(3)/2;
yl5=-sqrt(3)/2;
c1=roots([1 mu-3 3-2*mu -mu 2*mu -mu]);
c2=roots([1 3-mu 3-2*mu -mu -2*mu -mu]);
c3=roots([1 2+mu 1+2*mu mu-1 2*mu-2 mu-1]);
xl1=0;
xl2=0;
for i=1:5
    if isreal(c1(i))
        xl1=1-mu-c1(i);
    end
    if isreal(c2(i))
        xl2=1-mu+c2(i);
    end
    if isreal(c3(i))
        xl3=-mu-c3(i);
    end
end
xl4=0.5-mu;
xl5=xl4;
end