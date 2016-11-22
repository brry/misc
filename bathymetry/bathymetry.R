
# Bathymetry plotting: hue appears useful
# Berry Boessenkool, Nov 2016, berry-b@gmx.de

# install.packages("berryFunctions")
# berryFunctions::instGit("brry/berryFunctions") # update to development version
# install.packages("geoR") # if you also want the spatial interpolation part

bathy <- read.table("bathymetry.txt", header=TRUE)
lake <- read.table("bathymetry_lake.txt", header=TRUE)

png("bathymetry.png", width=4.5, height=5, units="in", pointsize=14, res=200)
par(mar=c(3,3,2,0.5), mgp=c(1.8, 0.7, 0))
plot(lake, type="l", las=1, xlab="East [m]", ylab="North [m]")
title(main="Lake XYZ", adj=0)
berryFunctions::colPoints(x,y,depth, data=bathy, pch="+", zlab="Water depth [m]", y1=0.85)
dev.off()


# Spatial Interpolation - Kriging:

library(geoR)
geodata <- as.geodata(bathy)#rbind(bathy, data.frame(lake[-1,],depth=0))
vario_emp <- variog(geodata, max.dist=40)
vario_fit <- variofit(vario_emp) 
ngrid <- 200
grid <- expand.grid(seqR(geodata$coords[,1], extend=0.1, len=ngrid),
                    seqR(geodata$coords[,2], extend=0.1, len=ngrid))
kri_control <- krige.control(type.krige="OK", obj.model=vario_fit)
kri_object  <- krige.conv(geodata, loc=grid, krige=kri_control)

# spatially interpolated regular grid map:
colors <- berryFunctions::seqPal(100)

png("bathymetry_kriging.png", width=4.5, height=5, units="in", pointsize=14, res=200)
par(mar=c(3,3,2,0.5), mgp=c(1.8, 0.7, 0))
plot(lake, type="l", las=1, xlab="East [m]", ylab="North [m]")
title(main="Lake XYZ", adj=0)
image(kri_object, col=colors, add=TRUE)
lines(lake)
box()
colPointsLegend(bathy$depth, title="Water depth [m]", y1=0.85, density=FALSE)
dev.off()
