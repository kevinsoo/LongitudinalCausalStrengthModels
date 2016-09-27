###############################################################
########## Base functions used by DeltaP and PowerPC ##########
###############################################################

#### converts binary cause (0/1) and effect (0/1) into states in 2*2 contingency format
# used for dP and powPC

convertToStates <- function(c, e) {
    i <- length(c) # num of observations
    state <- rep(NA, i) # array for 'states' = A, B, C, or D
    for (p in 1:i) {
        if (c[p]==1) {
            if (e[p]==1) { state[p] <- "A" } # e+, c+
            else if (e[p]==0) { state[p] <- "B" } # e-, c+
        }
        else if (c[p]==0) {
            if (e[p]==1) { state[p] <- "C" } # e+, c-
            else if (e[p]==0) { state[p] <- "D" } # e-, c-
        }
    }
    # return states in A, B, C, D format
    return(state)
}

#### computes conditional probabilities,  P(e+|c+) and P(e+|c-)
# used for dP and powPC
# takes in array of states from convertToStates()

condProb <- function(state) {
    i <- length(state) # num of observations
    pEC <- rep(NA, i) # array for P(e+|c+)
    pEc <- rep(NA, i) # array for P(e+|c-)
    
    # compute contingencies for each time point
    for (p in 1:i) {
        cont.tmp <- table(state[1:p]) # contingencies up to that point
        pEC[p] <- as.numeric(table(state[1:p])["A"])/(as.numeric(table(state[1:p])["A"])+as.numeric(table(state[1:p])["B"]))
        pEc[p] <- as.numeric(table(state[1:p])["C"])/(as.numeric(table(state[1:p])["C"])+as.numeric(table(state[1:p])["D"]))
    }
    # return data frame of conditional probabilities
    return(data.frame(pEC, pEc))
}

##############################################
########## DeltaP and PowerPC model ##########
##############################################

#### dP calculates causal contingency for elemental cause-effect relation
#### powPC calculates causal power for elemental cause-effect relation
# powPC computes power for generative causes by default, set valence to "neg" for preventive cause
# no parameters

dP.powPC <- function(c, e, valence="pos") {
    i <- length(c) # num of observations
    state <- convertToStates(c, e) # converts obervations to A, B, C, D format
    cps <- condProb(state) # compute conditional probabilities
    dP <- cps$pEC - cps$pEc # computes deltaP at each trial
    
    # compute causal power at each trial, differently for generative/preventive causes
    if (valence == "pos") { powPC <- dP/(1-cps$pEc) }
    else if (valence == "neg") { powPC <- (-1)*(dP/cps$pEc) }
    
    # return contingency strength in data frame WITH original data frame
    t <- c(1:i)
    return(data.frame(t, c, e, state, dP, powPC))
}