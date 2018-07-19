function move(n, src, dst, via)
    if n > 0 then
        move(n - 1, src, via, dst)
        print(src, 'to', dst)
        move(n - 1, via, dst, src)
    end
end

move(4, 1, 2, 3)
