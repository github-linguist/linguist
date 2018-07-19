tree = {1, {2, {4, {7,{},{}},
                   {}},
               {5, {},{}}},
           {3, {6, {8,{},{}},
                   {9,{},{}}},
               {}}}

preorder(t,f) = if !isempty(t)
                  f(t[1]) ; preorder(t[2],f) ; preorder(t[3],f)
                end

inorder(t,f) = if !isempty(t)
                   inorder(t[2],f) ; f(t[1]) ; inorder(t[3],f)
                end

postorder(t,f) = if !isempty(t)
                   postorder(t[2],f) ; postorder(t[3],f) ; f(t[1])
                end

levelorder(t,f) = while !isempty(t)
                    t = mapreduce(x->isa(x,Number)? (f(x);{}): x, vcat, t)
                  end
