# C to B
c1 <- c(rep(1, 10), rep(0,10))
c0 <- rep(1, length(c1))
e <- c(rep(1,10), rep(0,10))
df <- data.frame(e, c0, c1)

td <- TD(df, effectCol=1, c=.1, w1=0)

ggplot(data=td, aes(x=t)) + 
    geom_line(aes(y=w.c1, color="w.c1")) + 
    geom_line(aes(y=w.c0, color="w.c0")) + 
    geom_line(aes(y=xBar.c1, colour="xBar.c1"), linetype="dashed") + 
    geom_point(aes(y=c1, colour="c1"), size=4) + geom_point(aes(y=e, colour="e"))
