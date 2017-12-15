# It needs these four files:
# c("time-bruteforce", "time-endo", "time-foldl1", "time-comprehension")
# a way to create them is (in bash):
# n=30; while [ $n -gt 0 ]; do n=$((n-1)); (time ./Chinese) 2>&1 | grep real | tail -c 7 | head -c 5; echo ; done 2>&1 | tee time-comprehension

b <- read.table("time-bruteforce", col.names=c("t"))
e <- read.table("time-endo", col.names=c("t"))
f <- read.table("time-foldl1", col.names=c("t"))
c <- read.table("time-comprehension", col.names=c("t"))

df <- data.frame(bruteforce=b$t, endo=e$t, foldl=f$t, comprehension=c$t)

# Plot to a png.
# The delicate balance between `res`, `pointsize`, and `outcex` later makes the
# lines and the text sizes reasonable.
# `mar` and `mgp` give the labels space.
# `tcl` is the size of the ticks (relative to the text's height. What a hell.)
png("plots.png", width=1024, height=768, units="px", res=120, pointsize=2)
par(mar=c(24,30,10,10), xaxs="i", yaxs="i", cex.axis=10, mgp=c(0,14,0), tcl=-6)
layout(rbind(c(1,1,1,2)))

dl <- subset(df, select=c("bruteforce","endo","comprehension"))
ym=0.95*min(as.vector(as.matrix(dl)))
yM=1.05*max(as.vector(as.matrix(dl)))
boxplot(dl, show.names=TRUE, outcex=10, ylim=c(ym,yM))

dr <- subset(df, select=c("foldl"))
ym=0.95*min(as.vector(as.matrix(dr)))
yM=1.05*max(as.vector(as.matrix(dr)))
boxplot(dr, show.names=TRUE, outcex=10, ylim=c(ym,yM))

invisible(dev.off())
