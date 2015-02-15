declare
  Tree = n(1
           n(2
             n(4 n(7 e e) e)
             n(5 e e))
           n(3
             n(6 n(8 e e) n(9 e e))
             e))

  fun {Concat Xs}
     {FoldR Xs Append nil}
  end

  fun {Preorder T}
     case T of e then nil
     [] n(V L R) then
        {Concat [[V]
                 {Preorder L}
                 {Preorder R}]}
     end
  end

  fun {Inorder T}
     case T of e then nil
     [] n(V L R) then
        {Concat [{Inorder L}
                 [V]
                 {Inorder R}]}
     end
  end

  fun {Postorder T}
     case T of e then nil
     [] n(V L R) then
        {Concat [{Postorder L}
                 {Postorder R}
                 [V]]}
     end
  end

  local
     fun {Collect Queue}
        case Queue of nil then nil
        [] e|Xr then {Collect Xr}
        [] n(V L R)|Xr then
           V|{Collect {Append Xr [L R]}}
        end
     end
  in
     fun {Levelorder T}
        {Collect [T]}
     end
  end
in
  {Show {Preorder Tree}}
  {Show {Inorder Tree}}
  {Show {Postorder Tree}}
  {Show {Levelorder Tree}}
