# R code to construct, FN,TN,FP,TP curves
# FN threshold is added (red doshed line)
# pretest probability of 50% is addedd (grey line)

sens = 0.95
spec = 1

prev <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 
          0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75,
          0.8,0.85, 0.90, 0.95, 1)
x <- prev
length(x)

# pretest-probability = prevalence = prev
# sick
sick <- prev*100
length(sick)

# notsick
notsick = 100 - sick
length(notsick)

# TP = true positives
TP <- sick * sens  
length(TP)

# TN = true negatives
TN = spec * notsick  
length(TN)

# FN = false negatives = sick - TP
FN = sick -TP

# FP
FP= notsick - TN

#### summary all variables in een data frame
# check sick = sumsick
# check notsick = sumnotsicktest
sumsicktest = TP+FN
sumnotsicktest = TN+FP
ds <- data.frame(sick,notsick,FN,TP, sumsicktest, FP,TN, sumnotsicktest)
ds
# end summary all variables, 


# FNPV = FN/(FN +TN) 
numerator = FN
denominator = FN + TN
FNPV <- numerator/denominator
FNPV <- round(FNPV,2)
FNPV

numerator = TN
TN
FN 
denominator = FN + TN
NPV <- numerator/denominator
NPV <- round(NPV,2)
NPV

numerator = FP
denominator = FP + TP
FPPV <- numerator/denominator
FPPV <- round(FPPV,2)
FPPV

numerator = TP
FP = notsick - TN
FP
denominator = FP + TP
PPV <- numerator/denominator
PPV <- round(PPV,2)
PPV

# add all PVs to de datas set ds
ds <- data.frame(sick,notsick,FN,TP, sumsicktest, FP,TN, sumnotsicktest,
                 FNPV,NPV,FPPV,PPV)
ds

# alltogehter
op <- par(mfrow = c(1, 2), pty = "s")   
# FNPV and TNPV

x1 <- x*100
y <- ds$FNPV*100

# save text in plot as variable text
a =paste("sens = ",sens)
b =paste("spec = ",spec)
# save text

plot(x1,y, type = "b", xlim = c(0,100), 
     ylim = c(0,100), main= "FNPV and NPV", 
     sub = paste(a,b,sep=", "),
     xlab = "pre-test prob",
     ylab="PV", col = "darkred",
     axes = FALSE, lwd=2)
y2<- ds$NPV*100
lines(x1,y2, type = "b",col= "darkgreen", lwd=2)
legend(1, 60, legend=c("FNPV", "TNPV"),
       col=c("darkred", "darkgreen"), lty=1, cex=0.8)
abline(v=50, col ="grey", lty=2)
abline(h=5, col ="red", lty=2)

axis(side=1, at=seq(0,100, by=10))
axis(side=2, at=seq(0,100, by=10))

# end FNPV and TNPV

# FPPV and TPPV
y <- ds$FPPV*100
plot(x1,y, type = "b", xlim = c(0,100), 
     ylim = c(0,100), main= "FPPV and PPV",
     sub = paste(a,b,sep=", "),
     xlab = "pre-test prob",
     ylab = "PV", col = "darkred",
     axes = FALSE,lwd=2)

y2=ds$PPV*100
lines(x1,y2, type = "b",col= "darkgreen", lwd=2)
legend(1, 60, legend=c("FPPV", "TPPV"),
       col=c("darkred", "darkgreen"), lty=1, cex=0.8)

axis(side=1, at=seq(0,100, by=10))
axis(side=2, at=seq(0,100, by=10))
# end FPPV and TPPV

op <- par(mfrow = c(1,1), pty = "s")  



