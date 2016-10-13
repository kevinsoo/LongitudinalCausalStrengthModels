# load libraries
library("tidyverse")
theme_set(theme_bw())

############# generate data
# parameters for generating data
i <- 1000 # num of trials
j <- 2 # num of causes (excluding background cue)
causes <- character() # vector for cause labels
weights <- runif(j,-1,1) # weights for causes

# generate cause(s)
c0 <- rep(1,i) # background cue
for (p in 1:j) { 
    causes[p] <- paste("c", as.character(p), sep="") # cause name(s)
    assign(causes[p], sample(c(0,1), i, replace = TRUE)) # generate binary cause(s)
}

# generate effect from cause(s) ala multiple regression
e <- numeric(i)
for (q in 1:i) {
    tmp <- numeric(j)
    for (r in 1:j) {
        tmp[r] <- weights[r]*get(causes[r])[q] # compute product of cause(s) and weight(s) for that trial
    }
    e[q] <- 1*c0[q] + sum(tmp) # e = w0*c0 + w1*c1 + w2*c2...
}

# combine all variables, change based on what causes there are
df <- data.frame(e, c0, c1, c2)

############# rw simulation
td <- TD(df, effectCol=1, c=.05)

ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1, color="w.c1")) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_point(aes(y=c1, colour="c1"), size=4) + geom_point(aes(y=e, colour="e"))

