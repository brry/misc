# houses
# http://junkcharts.typepad.com/junk_charts/2017/08/details-details-details-giving-zillow-a-pie-treatment.html

house <- read.table(header=TRUE, sep=",", stringsAsFactors=FALSE, text=
"delinquent, underwater, region, state
10.1, 31.4, ----- United States, --
8.3, 55.2, Atlanta, GA
8.1, 22, Boston, MA
12.7, 41.1, Chicago, IL
6.7, 30.7, Dallas - Fort Worth, TX
6.1, 29, Denver, CO
6.3, 49.8, Detroit, MI
14.3, 71, Las Vegas, NV
12.1, 30, Los Angeles, CA
26.8, 46.4, Miami - Fort Lauderdale, FL
20.6, 21.3, New Fork, NY
19.5, 53.9, Orlando, FL
9.1, 55.5, Phoenix, AZ
12.3, 53.4, Riverside, CA
9, 51.2, Sacramento, CA
9.7, 30.7, San Francisco, CA
10.2, 39.6, Seattle, WA
10.6, 32.4, Washington, DC
")

house$under_delin <- house$delinquent/100*house$underwater
house$under_cur <- house$underwater - house$under_delin
house$above <- 100-house$underwater
house <- house[order(house$underwater),]

png("housing.png", width=7, height=5, units="in", res=500)
par(mar=c(3,11,2,1), mgp=c(2,0.6,0))
barplot(t(as.matrix(house[,5:7])), horiz=TRUE, col=c("red","orange","lightblue"),
        names.arg=paste(house$region,house$state,sep=","), las=1 )
mtext("underwater\ndelinquent", adj=0, line=-0.5, col="red")
mtext("underwater\ncurrent",           line=-0.5, col="orange")
mtext("above water",            adj=1, line=0   , col="lightblue")
dev.off()

#openFile("housing.png")
