##############################################################################
########## Generate binary observations for single cause and effect ##########
##############################################################################

#### function to generate data for binary cause (0/1) and effect (0/1) based on causal strength and base rates
# used for dP and powPC

createObs <- function(c.base, c.strength, e.base, n=1) {
    # generate data
    cause <- runif(n)
    background <- runif(n)
    x <- ifelse(cause<c.base,1,0) # p(c=1)=c.base
    y <- ifelse(x==0, ifelse(background<e.base,1,0), # p(e=1|c=0)=e.base
                ifelse(runif(n)<(1-(1-c.strength)*(1-e.base)),1,0)) # p(e=1|c=1)=1-(1-c.strength)*(1-e.base)
    return(data.frame(x,y))
}

#####################################################
########## Create columns for model output ##########
#####################################################

#### function to create columns for model outputs used by different models
# specify number of columns/rows and the names of those columns

createOutputCols <- function(name, c, extra=FALSE) {
    i <- dim(c)[1] # num of observations
    j <- dim(c)[2] # num of causes
    if (is.null(i)==TRUE) { i <- length(c) } # if only one cause
    if (is.null(j)==TRUE) { j <- 1 } # if only one cause
    
    # if you want an extra row at the end
    if (extra==TRUE) { i <- i + 1 }
    
    # set up vectors for computing V
    vectors <- character(j) # names of all columns
    for (k in 1:length(vectors)) {
        vectors[k] <- paste(name, ".", colnames(c)[k], sep="") # V columns
    }
    
    # create vectors
    outputCols <- matrix(0, nrow=i, ncol=j) # output vectors for all causes
    colnames(outputCols) <- vectors # name the columns
    
    return(data.frame(outputCols))
}
