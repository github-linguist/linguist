local men =
{
abe={'abi','eve','cath','ivy','jan','dee','fay','bea','hope','gay'},
bob={'cath','hope','abi','dee','eve','fay','bea','jan','ivy','gay'},
col={'hope','eve','abi','dee','bea','fay','ivy','gay','cath','jan'},
dan={'ivy','fay','dee','gay','hope','eve','jan','bea','cath','abi'},
ed={'jan','dee','bea','cath','fay','eve','abi','ivy','hope','gay'},
fred={'bea','abi','dee','gay','eve','ivy','cath','jan','hope','fay'},
gav={'gay','eve','ivy','bea','cath','abi','dee','hope','jan','fay'},
hal={'abi','eve','hope','fay','ivy','cath','jan','bea','gay','dee'},
ian={'hope','cath','dee','gay','bea','abi','fay','ivy','jan','eve'},
jon={'abi','fay','jan','gay','eve','bea','dee','cath','ivy','hope'}
}

local women =
{
abi={'bob','fred','jon','gav','ian','abe','dan','ed','col','hal'},
bea={'bob','abe','col','fred','gav','dan','ian','ed','jon','hal'},
cath={'fred','bob','ed','gav','hal','col','ian','abe','dan','jon'},
dee={'fred','jon','col','abe','ian','hal','gav','dan','bob','ed'},
eve={'jon','hal','fred','dan','abe','gav','col','ed','ian','bob'},
fay={'bob','abe','ed','ian','jon','dan','fred','gav','col','hal'},
gay={'jon','gav','hal','fred','bob','abe','col','ed','dan','ian'},
hope={'gav','jon','bob','abe','ian','dan','hal','ed','col','fred'},
ivy={'ian','col','hal','gav','fred','bob','abe','ed','jon','dan'}
}

local engagements = {}

local singlemen = 0

local function single(name)

  local partner = engagements[name]

  if partner then
    engagements[name] = nil
    engagements[partner] = nil
  end


  if men[name] then
    singlemen = singlemen + 1
  end
end

for guys,_   in pairs(men) do single(guys) end
for ladies,_ in pairs(women) do single(ladies) end --that is, ahem, ALL the single ladies.


local function engage(man,woman)
  engagements[man] = woman
  engagements[woman] = man
  singlemen = singlemen - 1
end


local attemptedEngagementsByMan = {}
for name,list in pairs(men) do
  attemptedEngagementsByMan[name] = {}
end


while singlemen > 0 do
  local man
  local woman

  --get a single man
  for singleman,prefs in pairs(men) do
    if not engagements[singleman] then
      man = singleman; break
    end
  end


  --get his most preferred untried lady
  local myAttempts = attemptedEngagementsByMan[man]
  for i,lady in ipairs(men[man]) do
    if not myAttempts[lady] then
      woman = lady; break
    end
  end


  --propose
  myAttempts[woman] = true
  local totalJerk = engagements[woman]
  if not totalJerk then
    engage(man,woman)
  else
    for i,herPreference in ipairs(women[woman]) do
      if herPreference == man then
        single(totalJerk)
        single(woman)
        engage(man,woman)
        break --leaves the jerk at the altar!
      elseif herPreference == totalJerk then
        break --shot down
      end
    end
  end
end

for name,_ in pairs(men) do
  print(name, " liked it so he put a ring on ", engagements[name])
end
