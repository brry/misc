# Convert ESRI shapefile to raster in R
# Berry Boessenkool, Nov 2016, berry-b@gmx.de

# you can load and use this function with
#    source("http://raw.githubusercontent.com/brry/misc/master/shp2raster.R")
#    shp_ras <- shp2raster("ShapeName.shp", cellsize=500, column="z_value")



# 1. Examples ----
# Not executed when sourcing 
if(FALSE){
 
source("http://raw.githubusercontent.com/brry/misc/master/shp2raster.R")

# example shapefiles:
browseURL("http://www.gadm.org/download")
download.file("http://biogeo.ucdavis.edu/data/gadm2.8/shp/DEU_adm_shp.zip", "Shape.zip") 
unzip("shape.zip", exdir="Shapes")

# use with filename:
DEU1 <- shp2raster("Shapes/DEU_adm1.shp", cell=5) # cell size error
DEU1 <- shp2raster("Shapes/DEU_adm1.shp", cell=0.1, column="ID_1", overwrite=TRUE)
plot(DEU1)

# use with object:
DEU0 <- rgdal::readOGR("Shapes/DEU_adm0.shp", "DEU_adm0")
# leave away column, and you will be asked interactively:
DEU0r <- shp2raster(shp=DEU0, ascname="Shapes/DEU0.asc", cellsize=0.2)
plot(DEU0)
plot(DEU0r, add=TRUE)

# use with several files:
unlink("Shapes/DEU_adm1.asc")
if(!requireNamespace("pbapply", quietly=TRUE)) install.packages("pbapply")
fnames <- dir("Shapes", pattern="*.shp", full.names=TRUE)
DEU <- pbapply::pblapply(fnames, shp2raster, cellsize=0.2, column="ID_1")
plot(DEU[[4]])
DEU[[5]]

} # end of source-ignored examples



# 2. Packages ----
if(!requireNamespace("raster", quietly=TRUE)) install.packages("raster")
if(!requireNamespace("rgdal",  quietly=TRUE)) install.packages("rgdal")
library(raster)
library(rgdal)



# 3. actual function ----

shp2raster <- function(
 shpname="",    # character string like "coolstuff.shp"
 shp=rgdal::readOGR(shpname,namepart,verbose=verbose), # Shapefile Object Spatial*DataFrame
 cellsize,      # cell size in coordinate units (usually degrees or m)
 column,        # name of column to use for z dimension in raster. Leave away for interactive selection.
 ascname=sub(".shp", ".asc", shpname), # output file name
 verbose=FALSE, # Report readOGR progress?
 ...)           # more arguments passed to raster::rasterize, like overwrite=TRUE
{
# if shp is missing/default, to read shapefile:
namepart <- basename(tools::file_path_sans_ext(shpname))
shp <- shp # now the default is evaluated
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
if(!column %in% n) message("Column '",column, "' is not in Shapefile. Select one of\n", 
                           paste(strwrap(toString(n)), collapse="\n"))
while(!column %in% n) column <- readline(paste0("Nonexistent column '",column, 
                                         "'. Type desired name, then hit ENTER: "))
# actually convert and write to file:
ras <- raster::rasterize(shp, r, column, filename=ascname, proj=shp@proj4string, ...)
# return output
ras
}

