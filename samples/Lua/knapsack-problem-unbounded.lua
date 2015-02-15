items = {   ["panaea"] = { ["value"] = 3000, ["weight"] = 0.3, ["volume"] = 0.025 },
            ["ichor"]  = { ["value"] = 1800, ["weight"] = 0.2, ["volume"] = 0.015 },
            ["gold"]   = { ["value"] = 2500, ["weight"] = 2.0, ["volume"] = 0.002 }
        }

max_weight = 25
max_volume = 0.25

max_num_items = {}
for i in pairs( items ) do
   max_num_items[i] = math.floor( math.min( max_weight / items[i].weight, max_volume / items[i].volume ) )
end

best = { ["value"] = 0.0, ["weight"] = 0.0, ["volume"] = 0.0 }
best_amounts = {}

for i = 1, max_num_items["panaea"] do
    for j = 1, max_num_items["ichor"] do
        for k = 1, max_num_items["gold"] do
            current = { ["value"]  = i*items["panaea"]["value"] + j*items["ichor"]["value"] + k*items["gold"]["value"],
                        ["weight"] = i*items["panaea"]["weight"] + j*items["ichor"]["weight"] + k*items["gold"]["weight"],
                        ["volume"] = i*items["panaea"]["volume"] + j*items["ichor"]["volume"] + k*items["gold"]["volume"]
                      }

            if current.value > best.value and current.weight <= max_weight and current.volume <= max_volume then
                best = { ["value"] = current.value, ["weight"] = current.weight, ["volume"] = current.volume }
                best_amounts = { ["panaea"] = i, ["ichor"] = j, ["gold"] = k }
            end
        end
    end
end

print( "Maximum value:", best.value )
for k, v in pairs( best_amounts ) do
    print( k, v )
end
