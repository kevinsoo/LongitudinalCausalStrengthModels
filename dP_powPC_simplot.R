#### simulation for elemental causal induction with dP and powPC

# load libraries
library("tidyr")
library("dplyr")
library("ggplot2")
theme_set(theme_bw())

# parameters for generating data
i <- 100 # num of trials

#### generate observations of c and e according to particular causal strength (function outputs x and y)
df <- createObs(0.1,0.8,0.1,i)
c <- df$x
e <- df$y

#### sample output
dP.powPC <- dP.powPC(c, e)
powPC <- powPC(c, e)

#### plots
causalStrength <- dP.powPC %>% gather(model, strength, dP:powPC)
ggplot(causalStrength, aes(x=t, color=model, y=strength)) + geom_line() + ggtitle("Causal strength over time")