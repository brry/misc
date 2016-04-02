# The cover for the album "Unknown Pleasures" by Joy Division (1979)
# recreated in base R (in style, with fake data)
# by Berry Boessenkool, berry-b@gmx.de, 2016-04-02, after reading
browseURL("http://alexwhan.com/2016-03-24-joy-division-plot")

# fake data, in a data.frame / matrix
Time <- seq(1,10,len=150) # x-axis values
set.seed(007)
fake_mu <- rnorm(50, mean=5.5, sd=0.5)
fake_sd <- rbeta(50, shape1=4, shape2=3) # shapes selected with  berryFunctions::betaPlotComp()
fake_signal <- function(i) # fake signal data for each of 50 signals
  {
  base <- dnorm(Time, mean=fake_mu[i], sd=fake_sd[i])
  factor <- 5 # factor determining height of single spike-lines
  noise <- rnorm(length(Time), sd=0.1)
  base*factor+noise
  }
fake_data <- sapply(1:50, fake_signal)

# actual plot:
op <- par(mar=c(0,0,0,0), bg="black")
plot(1:10, type="n", ylim=c(1,55), axes=F)
for(i in 50:1)
  {
  polygon(x=c(Time,1), y=c(fake_data[,i],0)+i, col="black")
  lines(Time, fake_data[,i]+i, col="white")
  }
par(op)
