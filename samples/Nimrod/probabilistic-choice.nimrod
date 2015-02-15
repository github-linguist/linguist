import tables, math, strutils, times

const
   num_trials = 1000000
   precsn     = 6

var start = cpuTime()

var probs = initTable[string,float](16)
probs.add("aleph",  1/5.0)
probs.add("beth",   1/6.0)
probs.add("gimel",  1/7.0)
probs.add("daleth", 1/8.0)
probs.add("he",     1/9.0)
probs.add("waw",    1/10.0)
probs.add("zayin",  1/11.0)
probs.add("heth",   1759/27720)

var samples = initTable[string,int](16)
for i, j in pairs(probs):
    samples.add(i,0)

randomize()
for i in 1 .. num_trials:
    var z = random(1.0)

    for j,k in pairs(probs):
        if z < probs[j]:
            samples[j] = samples[j] + 1
            break
        else:
             z = z - probs[j]

var s1, s2: float

echo("Item  ","\t","Target  ","\t","Results  ","\t","Difference")
echo("====  ","\t","======  ","\t","=======  ","\t","==========")
for i, j in pairs(probs):
    s1 += samples[i]/num_trials*100.0
    s2 += probs[i]*100.0
    echo( i,
             "\t", formatFloat(probs[i],ffDecimal,precsn),
             "\t", formatFloat(samples[i]/num_trials,ffDecimal,precsn),
             "\t", formatFloat(100.0*(1.0-(samples[i]/num_trials)/probs[i]),ffDecimal,precsn),"%")
echo("======","\t","======= ","\t","======== ")
echo("Total:","\t",formatFloat(s2,ffDecimal,2),"  \t",formatFloat(s1,ffDecimal,2))
echo("\n",formatFloat(cpuTime()-start,ffDecimal,2)," secs")
