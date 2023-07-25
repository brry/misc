# time series - missing entries
# Berry Boessenkool, June 2023, berry-b@gmx.de

# fake data:
values <- c("2023-07-26 11:20", "2023-08-02 19:40")
values <- strptime(values, "%F %H:%M")
values <- seq(values[1], values[2], by="20 min")
values <- data.frame(time=values)
values$A <- round(300+cumsum(rnorm(530)),1)
values$B <- round( 20*cumsum(rnorm(530)),0)
values <- values[-sample(1:530, 40), ] # fake gaps

# time differences:
values$diff <- c(0, diff(values$time))
View(values[order(values$diff,decreasing=TRUE),])

# Visualize gaps:
plot(diff~time, data=values, type="l", las=1)
plot(A~time, data=values, type="l", las=1)
points(A~time, data=values[values$diff>20,], col="red")

# completize time series:
allvalues <- seq(values$time[1], tail(values$time,1), by="20 min")
allvalues <- data.frame(time=allvalues)
allvalues <- merge(allvalues, values, all.x=TRUE)
allvalues$diff <- NULL # now obsolete (and wrong)
rm(values) # if no longer needed, remove this for a clean workspace
# to avoid accidentally accessing it later...

# impute missing values:
# in lieu of complex proper methods, here are some quick options:
if(!requireNamespace("zoo", quietly=TRUE)) install.packages("zoo")
allvalues$A_linear  <- zoo::na.approx   (allvalues$A) # interpolation
allvalues$A_spline  <- zoo::na.spline   (allvalues$A)
allvalues$A_last    <- zoo::na.locf     (allvalues$A)
allvalues$A_mean    <- zoo::na.fill     (allvalues$A, mean(allvalues$A,na.rm=TRUE))
allvalues$A_median  <- zoo::na.aggregate(allvalues$A, FUN=median)
# na.aggregate does the same as na.fill here, with less code + more options

plot(A~time, data=head(allvalues,50), type="l", las=1)
lines(A_spline~time, data=head(allvalues,50), col="red")
lines(A_mean~time,   data=head(allvalues,50), col="blue")
lines(A_linear~time, data=head(allvalues,50), col="orange")
lines(A~time, data=head(allvalues,50))

# Often, simple linear interpolation is the best of these options in my opinion.
# If anyhow possible, work with methods that can handle NAs and skip imputation.
# Only the black line is truth!
# In real life, if at all, more complex imputation may be needed.
# this is a failing starting point: zoo::na.StructTS(zoo::read.zoo(allvalues))

# Please do not linearly interpolate daily rainfall over 3 months.
# Annual sums will be way too high.
# Looking at you, Austrian weather service...

