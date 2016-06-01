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