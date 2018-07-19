function walk(maze, cell, visited = {})
  push!(visited, cell)
  for neigh in shuffle(neighbors(cell, size(maze)))
    if !(neigh in visited)
       maze[int((cell+neigh)/2)...] = 0
       walk(maze, neigh, visited)
    end
  end
  maze
end

neighbors(c,b,d=2) = filter(check(b),map(m->c+d*m, {[0,1],[-1,0],[0,-1],[1,0]}))

check(bound) = cell -> all([1,1] .<= cell .<= [bound...])

maze(w, h) = walk([i%2|j%2 for i=1:2w+1,j=1:2h+1], 2*[rand(1:w),rand(1:h)])

pprint(maze) = print(mapslices(x-> [join(x)], maze, [2]))

function mprint(maze, wall = CharString("╹╸┛╺┗━┻╻┃┓┫┏┣┳╋"...))
  pprint([ maze[i,j] == 0 ? ' ' :
           wall[sum(c-> 2.0^.5(3c[1]+c[2]+3),
                    filter(x -> maze[x...] != 0,
                           neighbors([i,j],[size(maze)...],1)) .- {[i,j]})]
           for i = 1:2:size(maze,1), j = 1:size(maze,2)])
end
