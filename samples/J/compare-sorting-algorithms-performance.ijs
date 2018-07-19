NB. extracts from other rosetta code projects
ts=: 6!:2, 7!:2@]
radix =: 3 : 0
256 radix y
:
a=. #{. z =. x #.^:_1 y
e=. (-a) {."0 b =. i.x
x#.1{::(<:@[;([: ; (b, {"1) <@}./. e,]))&>/^:a [ z;~a-1
NB. , ([: ; (b, {:"1) <@(}:"1@:}.)/. e,])^:(#{.z) y,.z
)
bubble=:  (([ (<. , >.) {.@]) , }.@])/^:_
insertion=:((>: # ]) , [ , < #])/
sel=: 1 : 'x # ['
quick=: 3 : 0
 if.  1 >: #y do.  y
 else.
  e=. y{~?#y
  (quick y <sel e),(y =sel e),quick y >sel e
 end.
)
gaps      =: [: }: 1 (1+3*])^:(> {:)^:a:~ #
insert    =: (I.~ {. ]) , [ , ] }.~ I.~
gapinss   =: #@] {. ,@|:@(] insert//.~ #@] $ i.@[)
shell =: [: ; gapinss &.>/@(< ,~ ]&.>@gaps)
builtin =: /:~



NB. characterization of the sorting algorithms.

sorts =: bubble`insertion`shell`quick`radix`builtin
generators =: #&1`(i.@-)`(?.~) NB. data generators

round =: [: <. 1r2&+

ll =: (<_1 0)&{  NB. verb to extract lower left which holds ln data length
lc =: (<_1 1)&{  NB. verb to fetch lower center which holds most recent time

NB. maximum_time characterize ln_start_size
NB. characterize returns a rank 4 matrix with successive indexes for
NB. algorithm, input arrangement, max number of tests in group, length time space
characterize =: 4 : 0
  max_time =. x
  start =. 1 3{.<:y
  for_sort. sorts do.
    for_generator. generators do.                                           NB. limit time  and  paging prevention
      t =: }. (, (, [: ts 'sort@.0 (generator@.0)' , ":@round@^)@>:@ll) ^: ((lc < max_time"_) *. ll < 17"_) ^:_ start
      if. generator -: {.generators do.
        g =. ,:t
      else.
        g =. g,t
      end.
    end.
    if. sort -: {.sorts do.
      s =. ,:g
    else.
      s =. s,g
    end.
  end.
)

NB. character cell graphics

NB. From j phrases 10E. Approximation
d3=: 1&,.@[ %.~ ]	NB. a and b such that y is approx. a + b*x

NB. domain and range 0 to 14.
D=:14

plot =: 1 : '(=/ round@(u&.(*&(D%<:y))))i.y' NB. function plot size
points =: 4 : '1(<"1|:|.round y*D%~<:x)}0$~2#x'  NB. size points x,:y

show =: [: |. [: '0'&~:@{:} ' ' ,: ":

plt =: 3 : 0
30 plt y NB. default size 30
:
n =. >:i.-# experiments =. <@(#~"1 (0&<)@{.)"2 y
pts =. n +./ .*x&points@>experiments
coef =. d3/@>experiments
(_*pts) + n +./ .*1 0 2|:coef&(p."1) plot x
)
