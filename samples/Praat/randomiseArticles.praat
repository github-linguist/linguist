form Querying
    integer tier 1
    integer nrFormants 10
endform

idSounds = Create Strings as file list: "Sounds", "*.flac"
idGrids = Create Strings as file list: "Grids", "*.TextGrid"
nrGrids = Get number of strings

for i to nrGrids
    selectObject: idSounds
    sound$ = Get string: i
    idSound = Read from file: sound$

    idFormant = To Formant (burg): 0, 5, 5500, 0.025, 50

    selectObject: idGrids
    grid$ = Get string: i

    idTable = Create Table with column names: grid$, 0, "Index Vowel Start End Duration F1 F2"
    idGrid = Read from file: grid$

    nrInt = Get number of intervals: tier
    k = 1

    for j to nrInt
        lab$ = Get label of interval: tier, j

        if lab$ = "aa" or lab$ = "u" or lab$ = "i"

            index$ = string$(k)
            start = Get start time of interval: tier, j
            start$ = string$(start)
            end = Get end time of interval: tier, j
            end$ = string$(end)
            dur = end - start
            dur$ = string$(dur)

            for l to nrFormants
                row = l + (k-1)*nrFormants
                time = start + dur*(l/nrFormants)

                selectObject: idFormant
                f1$ = Get value at time: 1, time, "hertz", "linear"
                f2$ = Get value at time: 2, time, "hertz", "linear"

                selectObject: idTable
                Insert row: row
                Set string value: row, "Index", index$
                Set string value: row, "Vowel", lab$
                Set string value: row, "Start", start$
                Set string value: row, "End", end$
                Set string value: row, "Duration", dur$
                Set string value: row, "F1", f1$
                Set string value: row, "F2", f2$

            endfor

            selectObject: idGrid
            k = k + 1
        endif

    endfor

    name$ = selected$("TextGrid")
    selectObject: idTable
    Save as tab-separated file: name$ + ".dat"

    removeObject: idSound, idTable, idFormant, idGrid

endfor

removeObject: idGrids, idSounds
