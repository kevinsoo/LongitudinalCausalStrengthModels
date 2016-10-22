# stimuli components
one<-rep(1,20) # presence of cue for t=10
zero<-rep(0,20) # absence of cue for t=10
iti<-rep(0,40) # inter-trial interval - t=20 to allow trace to return to 0
delay<-rep(0,5) # change delay between C and E as needed

#### A/B
c1 <- c(one,one,one,one,one,one)
e <- c(one,one,one,zero,zero,zero)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 > 0 during [A], then it decreases during [B]
# decreases more with lower gamma
td <- TD(df, effectCol=1, startW=c(0,0), c=.5, gamma=1, beta=.8)
# if w1 =< 0 during [A], then there's barely a decrease during [B]
td <- TD(df, effectCol=1, startW=c(0,-2), c=.5, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### B/B
c1 <- c(one,one,one,one,one,one)
e <- c(zero,zero,zero,zero,zero,zero)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 then there's no decrease
td <- TD(df, effectCol=1, startW=c(0,0), c=.5, gamma=1, beta=.8)
# if w1 > 0 during [A], then there's a gradual decrease, because prediction is < 1
td <- TD(df, effectCol=1, startW=c(0,1), c=.5, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### C/B How do you get an increase?
c1 <- c(zero,zero,zero,one,one,one)
e <- c(one,one,one,zero,zero,zero)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 then there's no decrease
td <- TD(df, effectCol=1, startW=c(0,0), c=.5, gamma=1, beta=.8)
# if w1 > 0 during [A], then there's a gradual decrease, because prediction is < 1
td <- TD(df, effectCol=1, startW=c(0,1), c=.5, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### D/B How do you get an increase?
c1 <- c(zero,zero,zero,one,one,one)
e <- c(zero,zero,zero,zero,zero,zero)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 then there's no decrease
td <- TD(df, effectCol=1, startW=c(0,0), c=.1, gamma=.95, beta=.8)
# if w1 > 0 during [A], then there's a gradual decrease, because prediction is < 1
td <- TD(df, effectCol=1, startW=c(0,1), c=.1, gamma=.95, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### B/C
c1 <- c(one,one,one,zero,zero,zero)
e <- c(zero,zero,zero,one,one,one)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 then there's an increase
td <- TD(df, effectCol=1, startW=c(0,0), c=.5, gamma=1, beta=.8)
# if w1 > 0 during [C], then there's an increase
td <- TD(df, effectCol=1, startW=c(0,1), c=.5, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### A/C
c1 <- c(one,zero)
e <- c(one,one)
c1 <- c(1,1,1,1,1,0,0,0,0,0)
e <- c(1,1,1,1,1,1,1,1,1,1)
c1 <- c(one,one,one,zero,zero,zero)
e <- c(one,one,one,one,one,one)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 at time of transition then there's no change (stops increasing)
td <- TD(df, effectCol=1, startW=c(0,-10), c=.1, gamma=1, beta=.8)
# if w1 > 0 during [C], then there's a decrease followed by no change
td <- TD(df, effectCol=1, startW=c(0,0), c=.5, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### D/C
c1 <- c(zero,zero,zero,zero,zero,zero)
e <- c(zero,zero,zero,one,one,one)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 at time of transition then there's no change (stops increasing)
td <- TD(df, effectCol=1, startW=c(0,0), c=.1, gamma=1, beta=.8)
# if w1 > 0 during [C], no change
td <- TD(df, effectCol=1, startW=c(0,1), c=.1, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

#### C/C
c1 <- c(one,zero,zero,zero,zero,zero,zero)
e <- c(one,one,one,one,one,one,one)
c0 <- rep(0, length(c1))
df <- data.frame(e, c0, c1)
# if w1 =< 0 at time of transition then there's no change (stops increasing)
td <- TD(df, effectCol=1, startW=c(0,0), c=.1, gamma=.95, beta=.8)
# if w1 > 0 during [C], no change
td <- TD(df, effectCol=1, startW=c(0,1), c=.1, gamma=1, beta=.8)
ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1), size=4) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_line(aes(y=error, color="error"), linetype="dashed") +
    geom_point(aes(y=c1, colour="c1"), size=2) + geom_point(aes(y=e, colour="e"))

