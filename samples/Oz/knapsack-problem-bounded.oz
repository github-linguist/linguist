declare
  %% maps items to tuples of
  %% Weight(hectogram), Value and available Pieces
  Problem = knapsack('map':9#150#1
                     'compass':13#35#1
                     'water':153#200#2
                     'sandwich':50#60#2
                     'glucose':15#60#2
                     'tin':68#45#3
                     'banana':27#60#3
                     'apple':39#40#3
                     'cheese':23#30#1
                     'beer':52#10#3
                     'suntan cream':11#70#1
                     'camera':32#30#1
                     't-shirt':24#15#2
                     'trousers':48#10#2
                     'umbrella':73#40#1
                     'waterproof trousers':42#70#1
                     'waterproof overclothes':43#75#1
                     'note-case':22#80#1
                     'sunglasses':7#20#1
                     'towel':18#12#2
                     'socks':4#50#1
                     'book':30#10#2
                    )

  %% item -> Weight
  Weights = {Record.map Problem fun {$ X} X.1 end}
  %% item -> Value
  Values =  {Record.map Problem fun {$ X} X.2 end}

  proc {Knapsack Solution}
     %% a solution maps items to finite domain variables
     %% whose maximum values depend on the item type
     Solution = {Record.map Problem fun {$ _#_#Max} {FD.int 0#Max} end}
     %% no more than 400 hectograms
     {FD.sumC Weights Solution '=<:' 400}
     %% search through valid solutions
     {FD.distribute naive Solution}
  end

  proc {PropagateLargerValue Old New}
     %% propagate that new solutions must yield a higher value
     %% than previously found solutions (essential for performance)
     {FD.sumC Values New '>:' {Value Old}}
  end

  fun {Value Candidate}
     {Record.foldL {Record.zip Candidate Values Number.'*'} Number.'+' 0}
  end

  fun {Weight Candidate}
     {Record.foldL {Record.zip Candidate Weights Number.'*'} Number.'+' 0}
  end

  [Best] = {SearchBest Knapsack PropagateLargerValue}
in
  {System.showInfo "Items: "}
  {Record.forAllInd Best
   proc {$ I V}
      if V > 0 then
	 {System.showInfo I#": "#V}
      end
   end
  }
  {System.printInfo "\n"}
  {System.showInfo "total value: "#{Value Best}}
  {System.showInfo "total weight: "#{Weight Best}}
