hb <- read.table(header=TRUE, sep=",", text="
x,y
20.9, 27.6
28.3, 27.6
29.8, 39.9
31.5, 11.5
31.9, 27.6
33.5, 27.6
35.0, 47.4
36.9, 8.1
38.6, 27.6
41.0, 27.6
41.3, 31.0
42.4, 21.4
42.9, 27.6
44.3, 19.0
46.3, 50.8
46.6, 8.5
48.7, 31.2
49.6, 27.6
61.2, 27.6
62.4, 34.9
65.0, 13.1
66.5, 48.4
67.5, 28.8
68.2, 31.5
71.0, 5.4
71.8, 39.3
72.7, 27.6
87.7, 27.6
")

x <- seq(20,90, length.out=500)
shb <- data.frame(x, dnorm(x, mean=55, sd=5))

png("heartbeat.png", width=15, height=9, units="cm", res=500)
layout(matrix(1:6,nrow=3,ncol=2), widths=c(6,4))
par(mar=c(0,2,1,0), oma=c(0,0,0,2), bg=1, ann=F)
plot(hb,                 type="l", col="green", lwd=2)
plot(c(20,90), rep(1,2), type="l", col="green", lwd=2, asp=1)
plot(shb, type="n", ylim=c(-0.06,0.06)) ; lines(shb, col="green", lwd=2, xpd=T)
plot(1, type="n"); text(1,1, "regular heartbeat",      col="green", cex=2)
plot(1, type="n"); text(1,1, "no heartbeat",           col="green", cex=2)
plot(1, type="n"); text(1,1, "statistician heartbeat", col="green", cex=2)
dev.off()
