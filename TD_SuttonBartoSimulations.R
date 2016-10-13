# the following are simulations presented in Sutton & Barto (1987)

##########################################################
########## single CS acquisition and extinction ##########
########## p363 Figure 3 #################################
##########################################################

########## data
# each 'trial' = 10t + 50t (ISI of 2.5 seconds)
c1 <- rep(c(rep(1,4), rep(0,6), rep(0,50)), 170) # 170 trials
c0 <- rep(0, length(c1)) # background
e <- c(rep(c(rep(0,8), rep(1,2), rep(0,50)), 70), rep(0, 60*100)) # 70 trials reinforced, 100 trials non-reinforced
df <- data.frame(e,c0,c1)
trial <- sort(rep(1:170, 60)) # number of trials

########## simulation
td <- TD(df, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE)
td <- data.frame(trial, td)
aveData <- aggregate(td, by=list(trial), FUN=mean)
ggplot(data=aveData, aes(x=trial, y=w.c1)) + geom_line()

##########################################################
########## serial-compound conditioning ##################
########## p367 Figure 6 #################################
##########################################################

########## data
# each 'trial' = 6t + 44t
c1 <- rep(c(1,1,1,1,0,0, rep(0,44)), 80)
c1 <- c(c1, rep(c(1,1,1,1,0,0, rep(0,44), rep(0,50)), 25))
c2 <- rep(c(rep(0,50), 1,1,1,1,0,0, rep(0,44)), 65)
c0 <- rep(1, length(c1)) # background
e <- rep(c(0,0,0,0,1,1, rep(0,44), rep(0,50)), 40)
e <- c(e, rep(0, 50*50))
df <- data.frame(e,c0,c1,c2)
trial <- sort(rep(1:130, 50)) # number of trials

########## simulation
td <- TD(df, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE)
td <- data.frame(trial, td)
aveData <- aggregate(td, by=list(trial), FUN=mean)
ggplot(data=aveData, aes(x=trial, y=w.c1)) + geom_line() + geom_line(aes(y=w.c2))

##################################################
########## serial-compound conditioning ##########
########## p368 Figure 7 #########################
##################################################

########## data
# each 'trial' = 10t + 50t
c1 <- rep(c(1,1,1,1, rep(0,6), rep(0,50)), 80)
c2 <- rep(c(0,0,0,0,1,1,1,1,0,0, rep(0,50)), 80)
c0 <- rep(0, length(c1)) # background
e <- rep(c(rep(0,8),1,1,rep(0,50)),80)
xWith <- data.frame(e,c0,c1,c2)
xWithout <- data.frame(e,c0,c1)
trial <- sort(rep(1:80, 60)) # number of trials

########## simulation
td0 <- TD(xWithout, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE)
td1 <- TD(xWith, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE)
td0 <- data.frame(trial, td0)
td1 <- data.frame(trial, td1)
aveData0 <- aggregate(td0, by=list(trial), FUN=mean)
aveData1 <- aggregate(td1, by=list(trial), FUN=mean)
ggplot() + geom_line(data=aveData0, aes(x=trial, y=w.c1)) + geom_line(data=aveData1, aes(x=trial, y=w.c1), color="red")

##################################################
########## serial-compound conditioning ##########
########## p369 Figure 8 #########################
##################################################

########## data
# each 'trial' = 10t + 50t
c1 <- rep(c(1,1,1,1,1,1,1,1,0,0, rep(0,50)), 80)
c2 <- rep(c(0,0,0,0,1,1,1,1,0,0, rep(0,50)), 80)
c0 <- rep(0, length(c1)) # background
e <- rep(c(rep(0,8),1,1, rep(0,50)), 80)
xWith <- data.frame(e,c0,c1,c2)
xWithout <- data.frame(e,c0,c2)
trial <- sort(rep(1:80, 60)) # number of trials

########## simulation
td0 <- TD(xWithout, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE)
td1 <- TD(xWith, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE)
td0 <- data.frame(trial, td0)
td1 <- data.frame(trial, td1)
aveData0 <- aggregate(td0, by=list(trial), FUN=mean)
aveData1 <- aggregate(td1, by=list(trial), FUN=mean)
ggplot() + geom_line(data=aveData0, aes(x=trial, y=w.c2)) + geom_line(data=aveData1, aes(x=trial, y=w.c2), color="red")

########## for graph on p370 Figure 9, set initial weights of c2 to be fully conditioned
td0 <- TD(xWithout, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE, w1=1.653)
td1 <- TD(xWith, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE, w2=1.653)
td0 <- data.frame(trial, td0)
td1 <- data.frame(trial, td1)
aveData0 <- aggregate(td0, by=list(trial), FUN=mean)
aveData1 <- aggregate(td1, by=list(trial), FUN=mean)
ggplot() + geom_line(data=aveData0, aes(x=trial, y=w.c2)) + geom_line(data=aveData1, aes(x=trial, y=w.c2), color="red")

##################################################
########## serial-compound conditioning ##########
########## p371 Figure 10 ########################
##################################################

########## Data
# Each 'trial' = 10t + 50t
c1 <- rep(c(1,1,1,1,0,0,0,0,0,0, rep(0,50)), 50)
c2 <- rep(c(0,0,0,0,1,1,1,1,0,0, rep(0,50)), 50)
c0 <- rep(0, length(c1)) # background
e <- rep(c(rep(0,60)), 50)
df <- data.frame(e,c0,c1,c2)
trial <- sort(rep(1:50, 60)) # number of trials

########## simulation
td <- TD(df, effectCol=1, c=.1, beta=.8, gamma=.95, final=FALSE, w2=1.653)
td <- data.frame(trial, td)
aveData <- aggregate(td, by=list(trial), FUN=mean)
ggplot(data=aveData, aes(x=trial, y=w.c1)) + geom_line() + geom_line(aes(y=w.c2), color="red")