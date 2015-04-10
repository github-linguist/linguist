;;;***************************
;;;* DEFFACTS KNOWLEDGE BASE *
;;;***************************

(deffacts MAIN::knowledge-base
   (welcome (message WelcomeMessage))
   (goal (variable type.animal))
   (legalanswers (values yes no))
   (displayanswers (values "Yes" "No"))
   (rule (if backbone is yes) 
         (then superphylum is backbone))
   (rule (if backbone is no) 
         (then superphylum is jellyback))
   (question (variable backbone)
             (query backbone.query))
   (rule (if superphylum is backbone and
          warm.blooded is yes) 
         (then phylum is warm))
   (rule (if superphylum is backbone and
          warm.blooded is no) 
         (then phylum is cold))
   (question (variable warm.blooded)
             (query warm.blooded.query))
   (rule (if superphylum is jellyback and
          live.prime.in.soil is yes) 
         (then phylum is soil))
   (rule (if superphylum is jellyback and
          live.prime.in.soil is no) 
         (then phylum is elsewhere))
   (question (variable live.prime.in.soil)
             (query live.prime.in.soil.query))
   (rule (if phylum is warm and
          has.breasts is yes) 
         (then class is breasts))
   (rule (if phylum is warm and
          has.breasts is no) 
         (then type.animal is bird))
   (question (variable has.breasts)
             (query has.breasts.query))
   (rule (if phylum is cold and
          always.in.water is yes) 
         (then class is water))
   (rule (if phylum is cold and
          always.in.water is no) 
         (then class is dry))
   (question (variable always.in.water)
             (query always.in.water.query))
   (rule (if phylum is soil and
          flat.bodied is yes) 
         (then type.animal is flatworm))
   (rule (if phylum is soil and
          flat.bodied is no) 
         (then type.animal is worm.leech))
   (question (variable flat.bodied)
             (query flat.bodied.query))
   (rule (if phylum is elsewhere and
          body.in.segments is yes) 
         (then class is segments))
   (rule (if phylum is elsewhere and
          body.in.segments is no) 
         (then class is unified))
   (question (variable body.in.segments)
             (query body.in.segments.query))
   (rule (if class is breasts and
          can.eat.meat is yes) 
         (then order is meat))
   (rule (if class is breasts and
          can.eat.meat is no) 
         (then order is vegy))
   (question (variable can.eat.meat)
             (query can.eat.meat.query))
   (rule (if class is water and
          boney is yes) 
         (then type.animal is fish))
   (rule (if class is water and
          boney is no) 
         (then type.animal is shark.ray))
   (question (variable boney)
             (query boney.query))
   (rule (if class is dry and
          scaly is yes) 
         (then order is scales))
   (rule (if class is dry and
          scaly is no) 
         (then order is soft))
   (question (variable scaly)
             (query scaly.query))
   (rule (if class is segments and
          shell is yes) 
         (then order is shell))
   (rule (if class is segments and
          shell is no) 
         (then type.animal is centipede.millipede.insect))
   (question (variable shell)
             (query shell.query))
   (rule (if class is unified and
          digest.cells is yes) 
         (then order is cells))
   (rule (if class is unified and
          digest.cells is no) 
         (then order is stomach))
   (question (variable digest.cells)
             (query digest.cells.query))
   (rule (if order is meat and
          fly is yes) 
         (then type.animal is bat))
   (rule (if order is meat and
          fly is no) 
         (then family is nowings))
   (question (variable fly)
             (query fly.query))
   (rule (if order is vegy and
          hooves is yes) 
         (then family is hooves))
   (rule (if order is vegy and
          hooves is no) 
         (then family is feet))
   (question (variable hooves)
             (query hooves.query))
   (rule (if order is scales and
          rounded.shell is yes) 
         (then type.animal is turtle))
   (rule (if order is scales and
          rounded.shell is no) 
         (then family is noshell))
   (question (variable rounded.shell)
             (query rounded.shell.query))
   (rule (if order is soft and
          jump is yes) 
         (then type.animal is frog))
   (rule (if order is soft and
          jump is no) 
         (then type.animal is salamander))
   (question (variable jump)
             (query jump.query))
   (rule (if order is shell and
          tail is yes) 
         (then type.animal is lobster))
   (rule (if order is shell and
          tail is no) 
         (then type.animal is crab))
   (question (variable tail)
             (query tail.query))
   (rule (if order is cells and
          stationary is yes) 
         (then family is stationary))
   (rule (if order is cells and
          stationary is no) 
         (then type.animal is jellyfish))
   (question (variable stationary)
             (query stationary.query))
   (rule (if order is stomach and
          multicelled is yes) 
         (then family is multicelled))
   (rule (if order is stomach and
          multicelled is no) 
         (then type.animal is protozoa))
   (question (variable multicelled)
             (query multicelled.query))
   (rule (if family is nowings and
          opposing.thumb is yes) 
         (then genus is thumb))
   (rule (if family is nowings and
          opposing.thumb is no) 
         (then genus is nothumb))
   (question (variable opposing.thumb)
             (query opposing.thumb.query))
   (rule (if family is hooves and
          two.toes is yes) 
         (then genus is twotoes))
   (rule (if family is hooves and
          two.toes is no) 
         (then genus is onetoe))
   (question (variable two.toes)
             (query two.toes.query))
   (rule (if family is feet and
          live.in.water is yes) 
         (then genus is water))
   (rule (if family is feet and
          live.in.water is no) 
         (then genus is dry))
   (question (variable live.in.water)
             (query live.in.water.query))
   (rule (if family is noshell and
          limbs is yes) 
         (then type.animal is crocodile.alligator))
   (rule (if family is noshell and
          limbs is no) 
         (then type.animal is snake))
   (question (variable limbs)
             (query limbs.query))
   (rule (if family is stationary and
          spikes is yes) 
         (then type.animal is sea.anemone))
   (rule (if family is stationary and
          spikes is no) 
         (then type.animal is coral.sponge))
   (question (variable spikes)
             (query spikes.query))
   (rule (if family is multicelled and
          spiral.shell is yes) 
         (then type.animal is snail))
   (rule (if family is multicelled and
          spiral.shell is no) 
         (then genus is noshell))
   (question (variable spiral.shell)
             (query spiral.shell.query))
   (rule (if genus is thumb and
          prehensile.tail is yes) 
         (then type.animal is monkey))
   (rule (if genus is thumb and
          prehensile.tail is no) 
         (then species is notail))
   (question (variable prehensile.tail)
             (query prehensile.tail.query))
   (rule (if genus is nothumb and
          over.400 is yes) 
         (then species is 400))
   (rule (if genus is nothumb and
          over.400 is no) 
         (then species is under400))
   (question (variable over.400)
             (query over.400.query))
   (rule (if genus is twotoes and
          horns is yes) 
         (then species is horns))
   (rule (if genus is twotoes and
          horns is no) 
         (then species is nohorns))
   (question (variable horns)
             (query horns.query))
   (rule (if genus is onetoe and
          plating is yes) 
         (then type.animal is rhinoceros))
   (rule (if genus is onetoe and
          plating is no) 
         (then type.animal is horse.zebra))
   (question (variable plating)
             (query plating.query))
   (rule (if genus is water and
          hunted is yes) 
         (then type.animal is whale))
   (rule (if genus is water and
          hunted is no) 
         (then type.animal is dolphin.porpoise))
   (question (variable hunted)
             (query hunted.query))
   (rule (if genus is dry and
          front.teeth is yes) 
         (then species is teeth))
   (rule (if genus is dry and
          front.teeth is no) 
         (then species is noteeth))
   (question (variable front.teeth)
             (query front.teeth.query))
   (rule (if genus is noshell and
          bivalve is yes) 
         (then type.animal is clam.oyster))
   (rule (if genus is noshell and
          bivalve is no) 
         (then type.animal is squid.octopus))
   (question (variable bivalve)
             (query bivalve.query))
   (rule (if species is notail and
          nearly.hairless is yes) 
         (then type.animal is man))
   (rule (if species is notail and
          nearly.hairless is no) 
         (then subspecies is hair))
   (question (variable nearly.hairless)
             (query nearly.hairless.query))
   (rule (if species is 400 and
          land.based is yes) 
         (then type.animal is bear.tiger.lion))
   (rule (if species is 400 and
          land.based is no) 
         (then type.animal is walrus))
   (question (variable land.based)
             (query land.based.query))
   (rule (if species is under400 and
          thintail is yes) 
         (then type.animal is cat))
   (rule (if species is under400 and
          thintail is no) 
         (then type.animal is coyote.wolf.fox.dog))
   (question (variable thintail)
             (query thintail.query))
   (rule (if species is nohorns and
          lives.in.desert is yes) 
         (then type.animal is camel))
   (rule (if species is nohorns and
          lives.in.desert is no and
          semi.aquatic is no) 
         (then type.animal is giraffe))
   (rule (if species is nohorns and
          lives.in.desert is no and
          semi.aquatic is yes) 
         (then type.animal is hippopotamus))
   (question (variable lives.in.desert)
             (query lives.in.desert.query))
   (question (variable semi.aquatic)
             (query semi.aquatic.query))
   (rule (if species is teeth and
          large.ears is yes) 
         (then type.animal is rabbit))
   (rule (if species is teeth and
          large.ears is no)
         (then type.animal is rat.mouse.squirrel.beaver.porcupine))
   (question (variable large.ears)
             (query large.ears.query))
   (rule (if species is noteeth and
          pouch is yes) 
         (then type.animal is kangaroo.koala.bear))
   (rule (if species is noteeth and
          pouch is no) 
         (then type.animal is mole.shrew.elephant))
   (question (variable pouch)
             (query pouch.query))
   (rule (if subspecies is hair and
          long.powerful.arms is yes) 
         (then type.animal is orangutan.gorilla.chimpanzee))
   (rule (if subspecies is hair and
          long.powerful.arms is no) 
         (then type.animal is baboon))
   (question (variable long.powerful.arms)
             (query long.powerful.arms.query))
   (rule (if species is horns and
          fleece is yes) 
         (then type.animal is sheep.goat))
   (rule (if species is horns and
          fleece is no) 
         (then subsubspecies is nofleece))
   (question (variable fleece)
             (query fleece.query))
   (rule (if subsubspecies is nofleece and
          domesticated is yes) 
         (then type.animal is cow))
   (rule (if subsubspecies is nofleece and
          domesticated is no) 
         (then type.animal is deer.moose.antelope))
   (question (variable domesticated)
             (query domesticated.query))
   (answer (prefix "I think your animal is a ") (variable type.animal) (postfix ".")))
