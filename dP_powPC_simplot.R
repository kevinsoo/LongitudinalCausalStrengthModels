############# simulation for elemental causal induction with dP and powPC
# load libraries
library("tidyverse")
theme_set(theme_bw())

############# generate data
# parameters for generating data
i <- 1000 # num of trials
c.strength <- .5 # power
c.base <- .1 # base rate of cause
e.base <- .1 # base rate of effect

# generate observations of c and e according to particular causal strength (function outputs x and y)
df <- createObs(c.base, c.strength, e.base, i)
c <- df$x
e <- df$y

############# dP and powPC simulation
df <- dP.powPC(c, e)

############# plots
# plot dP and powPC by trial
causalStrength <- df %>% gather(model, strength, dP:powPC)
ggplot(causalStrength, aes(x=t, color=model, y=strength)) + 
    geom_line() + 
    geom_hline(yintercept=c.strength, linetype="dashed") +
    ggtitle("Causal strength over time")