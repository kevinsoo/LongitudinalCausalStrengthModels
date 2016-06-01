c <- sample(c(0,1), 2000, replace=TRUE)
e <- sample(c(0,1), 2000, replace=TRUE)
c0<- rep(1,2000)
df <- data.frame(c0,c,e)

dP <- dP(c,e)
powPC <- powPC(c,e)
RW <- RW(alpha=0.1, df=df, effectCol = 3)

df <- data.frame(dP, select(powPC, powPC), select(RW[1:2000,],Vc))
df.plot <- df %>% gather(model, strength, dP:Vc)

ggplot(df.plot, aes(x=t, y=strength, color=model)) + geom_path()
