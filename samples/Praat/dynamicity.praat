form Textgrid Convolution
    comment calculate a Moving Average across a textgrid.
    comment does not perform an fft convolution, instead using a dumb algorithm.
    comment presumes the textgrid is selected.
    real steps_per_second 5
    real window_length_sec 5
    optionmenu kernel 1
        option moving_average
#       option gaussian
    optionmenu normalisation 1
        option amount
        option window_length
        option none
    endform

nrTiers    = Get number of tiers
isInterval = Is interval tier: 1

if nrTiers != 1
    exitScript: "Can only use dynamicity.praat on a TextGrid with exactly one Tier"
    endif

if isInterval == 1
    @preposess_intervals
    @calculate_nr_steps
    @interval_convolution
    @write_to_textgrid
else
    @calculate_nr_steps
    @point_convolution
    @write_to_textgrid
    endif


procedure preposess_intervals
    nrIntervals = Get number of intervals: 1

    for i to nrIntervals
        label$ = Get label of interval: 1, i
        if label$ <> "" and label$ != "MISSING"
            variable = number(label$)
            Set interval text: 1, i, string$(variable)
        endif
    endfor
endproc

procedure calculate_nr_steps
    start = Get start time
    end   = Get end time
    duration = end - start
    nrSteps = floor((duration-window_length_sec)*steps_per_second)
    if nrSteps < 0
        nrSteps = 0
        endif
endproc

procedure interval_convolution
    k=1
    movingStart = start
    movingEnd = movingStart + window_length_sec
    nrIntervals = Get number of intervals: 1

    if nrIntervals == 0
        nrSteps = 0
        k = 0
        endif

    result# = zero#(nrSteps)
    for i to nrSteps
        intervalEnd = Get end time of interval: 1, k
        while (intervalEnd < movingStart) and (k < nrIntervals)
            k+=1
            intervalEnd = Get end time of interval: 1, k
            endwhile

        movingSum = 0
        amountSummed = 0

        j=0
        intervalStart = Get start time of interval: 1, k+j
        while (intervalStart < movingEnd) and (k+j <= nrIntervals)
            label$ = Get label of interval: 1, k+j
            if label$ <> "" and label$ != "MISSING"
                movingSum += number(label$)
                amountSummed+= 1
                endif

            j+=1
            if (k+j <= nrIntervals)
                intervalStart = Get start time of interval: 1, k+j
                endif
            endwhile

        movingStart+=(1/steps_per_second)
        movingEnd+=(1/steps_per_second)

        if normalisation == 1
            result#[i] = movingSum/amountSummed
        elsif normalisation == 2
            result#[i] = movingSum/window_length_sec
        elsif normalisation == 3
            result#[i] = movingSum
            endif
        endfor
endproc

procedure point_convolution
    k=1
    movingStart = start
    movingEnd = movingStart + window_length_sec
    nrPoints = Get number of points: 1

    if nrPoints == 0
        nrSteps = 0
        k = 0
        endif

    result# = zero#(nrSteps)
    for i to nrSteps
        pointStart = Get time of point: 1, k
        while (pointStart < movingStart) and (k < nrPoints)
            k+=1
            pointStart = Get time of point: 1, k
            endwhile

        movingSum = 0
        amountSummed = 0

        j=0
        pointEnd = Get time of point: 1, k+j
        while (pointEnd < movingEnd) and (k+j <= nrPoints)
            movingSum += 1

            j+=1
            if (k+j <= nrPoints)
                pointEnd = Get time of point: 1, k+j
                endif
            endwhile

        movingStart+=(1/steps_per_second)
        movingEnd+=(1/steps_per_second)

        result#[i] = movingSum
        endfor
endproc

procedure write_to_textgrid
    tierName$ = Get tier name: 1
    Insert interval tier: 2, tierName$ + "Dyn"

    movingBoundary = (window_length_sec/2) - ((1/steps_per_second)/2)

    if movingBoundary <= 0
        start = 0.01
    else
        start = movingBoundary
        endif

    Insert boundary: 2, start

    for i to nrSteps
        movingBoundary+= 1/steps_per_second

        if movingBoundary >= end
            movingBoundary = end - (0.01 * nrSteps - (i + 1))
            endif

        Insert boundary: 2, movingBoundary
        if result#[i] == undefined
            Set interval text: 2, i+1, ""
        else
            Set interval text: 2, i+1, string$(result#[i])
            endif
        endfor
endproc
