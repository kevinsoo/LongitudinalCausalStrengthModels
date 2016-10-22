######################################################################
########## Base functions used by Temporal-Difference model ##########
######################################################################

#### computes eligibility traces for each cause at each time point
# cause = data frame of cause(s)
# beta parameter is at .8 by default, following Sutton & Barto's (1987) simulations

traces <- function(cause, beta=.8) {
    i <- dim(cause)[1] # num of observations
    j <- dim(cause)[2] # num of causes

    # set up vectors for computing xBar
    xBar <- createOutputCols("xBar", cause, extra=TRUE)

    # compute eligibility trace for each time point after t = 1, where xBar = 0
    for (t in 2:i) {
        for (n in 1:j) { # compute xBar for each cause
            xBar[t,n] <- beta*xBar[t-1,n] + (1-beta)*cause[t-1,n]
        }
    }
    return(xBar)
}

#### computes prediction at t as the weighted sum of each cause
# cause = data frame of cause(s)
# w = data frame of weights for cause(s)
# specify if prediction is computed from "past" or "present" observations of cause(s)

pred <- function(cause, w, t, time) {
    i <- dim(cause)[1] # num of observations
    j <- dim(cause)[2] # num of causes
    
    # prediction from present or past observation of cause(s)
    if (time=="present") {
        tmp.pred <- w[t,]*cause[t,] # products of weights at t with causes at t
        predSum <- sum(tmp.pred)
    }
    else if (time=="past") {
        tmp.pred <- w[t,]*cause[t-1,] # product of weights at t with causes at t-1
        predSum <- sum(tmp.pred)
    }
    
    # return prediction
    if (predSum <= 0) { return(0) }
    else { return(predSum) }
}

######################################################################
########## Temporal-Difference model (Sutton & Barto, 1987) ##########
######################################################################

#### calculates weights, W, updated at each time point
# df = data frame of cause(s) and effect
# effectCol = column number of effect
# c = scaling parameter, default of .01
# startW is an optional list of starting weights (length should match number of causes)
# gamma = parameter weighting presence vs. change in cause(s), defaults to .95 giving preference to presence of cause(s)

TD <- function(df, effectCol, c=.01, gamma=.95, beta=.8, final=FALSE, startW=NULL) {
    e <- df[,effectCol] # extracts effect
    cause <- df[,-effectCol] # extracts cause(s)
    i <- dim(cause)[1] # num of observations
    j <- dim(cause)[2] # num of causes
    
    # set up vectors for computing weights
    w <- createOutputCols("w", cause, extra=TRUE)
    
    # give custom starting weights
    if (is.null(startW)==FALSE) {
        for (p in 1:length(startW)) {
            w[1,p] <- startW[p]
        }
    }

    # create other vectors
    predPast <- numeric(i+1) # vector for predictions from past values of cause(s)
    predPres <- numeric(i+1) # vector for predictions from present values of cause(s)
    
    # compute trace
    xBar <- traces(cause, beta)
    
    for (t in 1:i) {
        # calculate predictions from past and present values of cause(s)
        predPres[t] <- pred(cause, w, t, time="present")
        if (t>1) { predPast[t] <- pred(cause, w, t, time="past") }
        # update weights at next time point
        for (n in 1:j) { # for weight of each cause
            w[t+1, n] <- w[t, n] + c*(e[t] + gamma*predPres[t] - predPast[t])*xBar[t,n]
        }
    }
    
    # input end values
    predPast[i+1] <- pred(cause, w, i+1, time="past")
    predPres[i+1] <- NA
    xBar[i+1,] <- NA
    df[i+1,] <- NA
    t <- 1:(i+1)
    y <- df$e + predPres # TD output
    present <- df$e + gamma*predPres # present prediction with e
    error <- present - predPast # error in prediction
    
    # return weights and predictions in data frame WITH original data frame
    td <- data.frame(t, df, xBar, predPres, predPast, present, error, w, y)
    if (final==TRUE) { return(td) } # by default, final row is returned
    else if (final==FALSE) { return(td[1:i,]) }
}
