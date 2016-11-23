# Convert ESRI shapefile to raster in R
# Berry Boessenkool, Nov 2016, berry-b@gmx.de

# you can load this function with
#     source("http://raw.githubusercontent.com/brry/misc/master/shp2raster.R")

if(!requireNamespace("raster", quietly=TRUE)) install.packages("raster")
if(!requireNamespace("rgdal",  quietly=TRUE)) install.packages("rgdal")
library(raster)
library(rgdal)

shp2raster <- function(
 shpname,       # character string like "coolstuff.shp"
 cellsize,      # cell size in coordinate units (usually degrees or m)
 column,        # name of column to use for z dimension in raster. Leave away for interactive selection.
 ascname=sub(".shp", ".asc", shpname), # output file name
 verbose=FALSE, # Report readOGR progress?
 ...)           # more arguments passed to raster::rasterize, like overwrite=TRUE
{
# read shapefile:
namepart <- basename(tools::file_path_sans_ext(shpname))
shp <- rgdal::readOGR(shpname, namepart, verbose=verbose)
# target raster extend and resolution:
e <- extent(shp) 
nx <- diff(c(e@xmin, e@xmax))/cellsize
ny <- diff(c(e@ymin, e@ymax))/cellsize
r <- raster(ncol=nx, nrow=ny)
extent(r) <- extent(shp)
resdif <- abs((yres(r) - xres(r)) / yres(r) )
if(resdif > 0.01) stop("Horizontal (",round(xres(r),3),") and vertical (",
                       round(yres(r),3),") resolutions are too different (diff=",
   round(resdif,3), ", but must be <0.010).\n  Use a smaller cell size to achieve this",
   " (currently ",cellsize,").")
# column selection
if(missing(column)) column <- ""
n <- names(shp)
while(!column %in% n) column <- readline(paste0("Column '",column, 
                         "' is not in Shapefile. Select one of\n",toString(n),"\nType, then hit ENTER: "))
# actually convert and write to file:
ras <- raster::rasterize(shp, r, column, filename=ascname, proj=shp@proj4string, ...)
# return output
ras
}

# Example (not executed when sourcing): 
if(FALSE){

owd <- setwd(tempdir()); getwd()
browseURL("http://www.gadm.org/download")
download.file("http://biogeo.ucdavis.edu/data/gadm2.8/shp/DEU_adm_shp.zip", "Shape.zip") 
unzip("shape.zip")

DEU1 <- shp2raster("DEU_adm1.shp", cell=5) # cell size error
DEU1 <- shp2raster("DEU_adm1.shp", cell=0.1, column="ID_1", overwrite=TRUE)
plot(DEU1)

setwd(owd)
}