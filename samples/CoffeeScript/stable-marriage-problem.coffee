class Person
  constructor: (@name, @preferences) ->
    @mate = null
    @best_mate_rank = 0
    @rank = {}
    for preference, i in @preferences
      @rank[preference] = i

  preferred_mate_name: =>
    @preferences[@best_mate_rank]

  reject: =>
    @best_mate_rank += 1

  set_mate: (mate) =>
    @mate = mate

  offer_mate: (free_mate, reject_mate_cb) =>
    if @mate
      if @covets(free_mate)
        console.log "#{free_mate.name} steals #{@name} from #{@mate.name}"
        reject_mate_cb @mate
        free_mate.set_mate @
        @set_mate free_mate
      else
        console.log "#{free_mate.name} cannot steal #{@name} from #{@mate.name}"
        reject_mate_cb free_mate
    else
      console.log "#{free_mate.name} gets #{@name} first"
      free_mate.set_mate @
      @set_mate free_mate

  happiness: =>
    @rank[@mate.name]

  covets: (other_mate) =>
    @rank[other_mate.name] <= @rank[@mate.name]

persons_by_name = (persons) ->
  hsh = {}
  for person in persons
    hsh[person.name] = person
  hsh

mate_off = (guys, gals) ->
  free_pursuers = (guy for guy in guys)
  guys_by_name = persons_by_name guys
  gals_by_name = persons_by_name gals

  while free_pursuers.length > 0
    free_pursuer = free_pursuers.shift()
    gal_name = free_pursuer.preferred_mate_name()
    gal = gals_by_name[gal_name]
    reject_mate_cb = (guy) ->
      guy.reject()
      free_pursuers.push guy
    gal.offer_mate free_pursuer, reject_mate_cb


report_on_mates = (guys) ->
  console.log "\n----Marriage Report"
  for guy, i in guys
    throw Error("illegal marriage") if guy.mate.mate isnt guy
    console.log guy.name, guy.mate.name, \
      "(his choice #{guy.happiness()}, her choice #{guy.mate.happiness()} )"

report_potential_adulteries = (guys) ->
  for guy1, i in guys
    gal1 = guy1.mate
    for j in [0...i]
      guy2 = guys[j]
      gal2 = guy2.mate
      if guy1.covets(gal2) and gal2.covets(guy1)
        console.log "#{guy1.name} and #{gal2.name} would stray"
      if guy2.covets(gal1) and gal1.covets(guy2)
        console.log "#{guy2.name} and #{gal1.name} would stray"

perturb = (guys) ->
  # mess up marriages by swapping two couples...this is mainly to drive
  # out that report_potential_adulteries will actually work
  guy0 = guys[0]
  guy1 = guys[1]
  gal0 = guy0.mate
  gal1 = guy1.mate
  console.log "\nPerturbing with #{guy0.name}, #{gal0.name}, #{guy1.name}, #{gal1.name}"
  guy0.set_mate gal1
  guy1.set_mate gal0
  gal1.set_mate guy0
  gal0.set_mate guy1


Population = ->
  guy_preferences =
   abe:  ['abi', 'eve', 'cath', 'ivy', 'jan', 'dee', 'fay', 'bea', 'hope', 'gay']
   bob:  ['cath', 'hope', 'abi', 'dee', 'eve', 'fay', 'bea', 'jan', 'ivy', 'gay']
   col:  ['hope', 'eve', 'abi', 'dee', 'bea', 'fay', 'ivy', 'gay', 'cath', 'jan']
   dan:  ['ivy', 'fay', 'dee', 'gay', 'hope', 'eve', 'jan', 'bea', 'cath', 'abi']
   ed:   ['jan', 'dee', 'bea', 'cath', 'fay', 'eve', 'abi', 'ivy', 'hope', 'gay']
   fred: ['bea', 'abi', 'dee', 'gay', 'eve', 'ivy', 'cath', 'jan', 'hope', 'fay']
   gav:  ['gay', 'eve', 'ivy', 'bea', 'cath', 'abi', 'dee', 'hope', 'jan', 'fay']
   hal:  ['abi', 'eve', 'hope', 'fay', 'ivy', 'cath', 'jan', 'bea', 'gay', 'dee']
   ian:  ['hope', 'cath', 'dee', 'gay', 'bea', 'abi', 'fay', 'ivy', 'jan', 'eve']
   jon:  ['abi', 'fay', 'jan', 'gay', 'eve', 'bea', 'dee', 'cath', 'ivy', 'hope']

  gal_preferences =
   abi:  ['bob', 'fred', 'jon', 'gav', 'ian', 'abe', 'dan', 'ed', 'col', 'hal']
   bea:  ['bob', 'abe', 'col', 'fred', 'gav', 'dan', 'ian', 'ed', 'jon', 'hal']
   cath: ['fred', 'bob', 'ed', 'gav', 'hal', 'col', 'ian', 'abe', 'dan', 'jon']
   dee:  ['fred', 'jon', 'col', 'abe', 'ian', 'hal', 'gav', 'dan', 'bob', 'ed']
   eve:  ['jon', 'hal', 'fred', 'dan', 'abe', 'gav', 'col', 'ed', 'ian', 'bob']
   fay:  ['bob', 'abe', 'ed', 'ian', 'jon', 'dan', 'fred', 'gav', 'col', 'hal']
   gay:  ['jon', 'gav', 'hal', 'fred', 'bob', 'abe', 'col', 'ed', 'dan', 'ian']
   hope: ['gav', 'jon', 'bob', 'abe', 'ian', 'dan', 'hal', 'ed', 'col', 'fred']
   ivy:  ['ian', 'col', 'hal', 'gav', 'fred', 'bob', 'abe', 'ed', 'jon', 'dan']
   jan:  ['ed', 'hal', 'gav', 'abe', 'bob', 'jon', 'col', 'ian', 'fred', 'dan']

  guys = (new Person(name, preferences) for name, preferences of guy_preferences)
  gals = (new Person(name, preferences) for name, preferences of gal_preferences)
  [guys, gals]

do ->
  [guys, gals] = Population()
  mate_off guys, gals
  report_on_mates guys
  report_potential_adulteries guys
  perturb guys
  report_on_mates guys
  report_potential_adulteries guys
