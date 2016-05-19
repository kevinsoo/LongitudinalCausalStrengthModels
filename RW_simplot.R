#### Simulation for RW model with multiple causes

# load libraries, be sure to source the RW_model file
library("tidyr")
library("dplyr")
library("ggplot2")
theme_set(theme_bw())

# parameters for generating data
i <- 100 # num of trials
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

#### sample output
rw <- RW(alpha=.1, df, effectCol=1)

#### plots
# plot error by trial
ggplot(rw, aes(x=t, y=error)) + geom_line() + ggtitle("Prediction error over time")

# plot strength (V) by trial
rw.V <- rw %>% gather(Vcue, V, Vc0:VTotal) %>% arrange(t, Vcue) %>% filter(Vcue != "VTotal")
ggplot(rw.V, aes(x=t, color=Vcue, y=V)) + geom_line() + ggtitle("Associative strength over time")

# plot deltaV by trial
# rw.dV <- rw %>% gather(dVcue, dV, dVc0:dVc2) %>% arrange(t, dVcue)
# ggplot(rw.dV, aes(x=t, color=dVcue, y=dV)) + geom_line() 
