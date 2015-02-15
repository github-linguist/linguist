   pickyPicky =: verb define
     if. y-:'bad argument' do.
        throw.
     else.
        'thanks!'
     end.
   )

   tryThis  =: verb define
     try.
        pickyPicky y
     catcht.
        'Uh oh!'
     end.
   )

   tryThis 'bad argument'
Uh oh!
