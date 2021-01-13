x_0=linspace(0,100,101);
vx_0=linspace(0,100,101);
z=zeros(101,101);
for i=1:101
    for j=1:101
        z(i,j)=x_0(i)*vx_0(j);
    end
end

figure
pcolor(x_0,vx_0,z)
shading flat