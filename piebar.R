
berryFunctions::pdfpng({
par(mfrow=c(1,2), mar=c(2,3,1,4.5), las=1)
vals <- c(2, 4, 5, 6, 6, 7, 8, 9, 10, 11, 15, 17)
names(vals) <- rev(c("These", "are", "labels", "showing", "not", "to", "create", "pie", "charts", "with", "many", "slices"))
cols <- sample(colors(), 12)
pie(vals, col=cols)
par(mar=c(2,3,1,0.5), mgp=c(2, 0.3, 0))
barplot(vals, horiz=TRUE, col=cols)}, "piebar", pdf=F, overwrite=T)
