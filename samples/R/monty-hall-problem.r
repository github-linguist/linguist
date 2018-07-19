# Since R is a vector based language that penalizes for loops, we will avoid
#     for-loops, instead using "apply" statement variants (like "map" in other
#     functional languages).

set.seed(19771025)   # set the seed to set the same results as this code
N <- 10000  # trials
true_answers <- sample(1:3, N, replace=TRUE)

# We can assme that the contestant always choose door 1 without any loss of
#    generality, by equivalence.  That is, we can always relabel the doors
#    to make the user-chosen door into door 1.
# Thus, the host opens door '2' unless door 2 has the prize, in which case
#    the host opens door 3.

host_opens <- 2 + (true_answers == 2)
other_door <- 2 + (true_answers != 2)

## if always switch
summary( other_door == true_answers )
## if we never switch
summary( true_answers == 1)
## if we randomly switch
random_switch <- other_door
random_switch[runif(N) >= .5] <- 1
summary(random_switch == true_answers)



## To go with the exact parameters of the Rosetta challenge, complicating matters....
##  Note that the player may initially choose any of the three doors (not just Door 1),
##     that the host opens a different door revealing a goat (not necessarily Door 3), and
##     that he gives the player a second choice between the two remaining unopened doors.

N <- 10000  #trials
true_answers <- sample(1:3, N, replace=TRUE)
user_choice <- sample(1:3, N, replace=TRUE)
## the host_choice is more complicated
host_chooser <- function(user_prize) {
    # this could be cleaner
    bad_choices <- unique(user_prize)
    # in R, the x[-vector] form implies, choose the indices in x not in vector
    choices <- c(1:3)[-bad_choices]
    # if the first arg to sample is an int, it treats it as the number of choices
    if (length(choices) == 1) {  return(choices)}
    else { return(sample(choices,1))}
}

host_choice <- apply( X=cbind(true_answers,user_choice), FUN=host_chooser,MARGIN=1)
not_door <- function(x){ return( (1:3)[-x]) }  # we could also define this
                                                # directly at the FUN argument following
other_door  <- apply( X = cbind(user_choice,host_choice), FUN=not_door, MARGIN=1)


## if always switch
summary( other_door == true_answers )
## if we never switch
summary( true_answers == user_choice)
## if we randomly switch
random_switch <- user_choice
change <- runif(N) >= .5
random_switch[change] <- other_door[change]
summary(random_switch == true_answers)
