% Plot dei Lagrangian points
n=5;
mu=linspace(0,0.5,n);
for i=1:n
    [xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5] = Lagr(mu(i));
    figure (1)
    hold all
    plot(xl1, yl1, 's')
    plot(xl2, yl2, 's')
    plot(xl3, yl3, 's')
    plot(xl4, yl4, 's')
    plot(xl5, yl5, 's')
    plot(-mu,0,'o')
    plot(1-mu,0, 'o')
    plot([-mu(i) xl4],[0 yl4])
end