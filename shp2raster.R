# Convert ESRI shapefile to raster in R
# Berry Boessenkool, Nov 2016, berry-b@gmx.de

# you can load and use this function with

#    source("http://raw.githubusercontent.com/brry/misc/master/shp2raster.R")
#    shp_ras <- shp2raster("ShapeName.shp", column="z_value", ncells=300)

# shp2raster tries to find a suitable resolution automatically.
# It gives useful warnings, errors, and interactive choices (e.g if column is missing)

# Please report any bugs you might still find! (berry-b@mx.de / file issue at
# https://github.com/brry/misc/blob/master/shp2raster.R )

# Content:
# 1. Examples (not sourced)
# 2. Packages
# 3. actual function



# 1. Examples ----
# Not executed when sourcing 
if(FALSE){
 
source("http://raw.githubusercontent.com/brry/misc/master/shp2raster.R")

# example shapefiles:
browseURL("http://www.gadm.org/download")
download.file("http://biogeo.ucdavis.edu/data/gadm2.8/shp/DEU_adm_shp.zip", "Shape.zip") 
unzip("shape.zip", exdir="Shapes")

# use with filename:
DEU1 <- shp2raster("Shapes/DEU_adm1.shp", cellsize=5) # cell size error
DEU1 <- shp2raster("Shapes/DEU_adm1.shp", column="ID_1")
plot(DEU1)
# warning with interactive decision:
DEU0 <- shp2raster("Shapes/DEU_adm0.shp", column="ID_0", cellsize=0.05, ncellwarn=150)
DEU0 # result depends on decision!

# use with object:
DEU0 <- rgdal::readOGR("Shapes/DEU_adm0.shp", "DEU_adm0")
# leave away column, and you will be asked interactively:
DEU0r <- shp2raster(shp=DEU0, ncells=50, overwrite=TRUE)
plot(DEU0)
plot(DEU0r, add=TRUE)
unlink("DEU0.asc") # written directly in getwd(), if ascname is NA (the default)

# use with several files:
unlink("Shapes/DEU_adm1.asc") ; unlink("Shapes/DEU_adm0.asc")
if(!requireNamespace("pbapply", quietly=TRUE)) install.packages("pbapply")
fnames <- dir("Shapes", pattern="*.shp", full.names=TRUE)
#DEU <- pbapply::pblapply(fnames, shp2raster, cellsize=0.2, column="ID_1")
# with changing inputs:
columns <- paste("ID", 0:4, sep="_")
DEU <- pbapply::pblapply(seq_along(fnames), function(i) shp2raster(fnames[i], 
                                             cellsize=0.2, column=columns[i]))
plot(DEU[[4]])
DEU[[5]]

# On large rasters, you might want to use gdalUtils::gdal_rasterize, see
# comments to http://stackoverflow.com/a/14992508/1587132


} # end of source-ignored examples



# 2. Packages ----
if(!requireNamespace("raster", quietly=TRUE)) install.packages("raster")
if(!requireNamespace("rgdal",  quietly=TRUE)) install.packages("rgdal")
library(raster)
library(rgdal)



# 3. actual function ----

shp2raster <- function(
 shpname="",    # single file name like "coolstuff.shp". Ignored if shp is given.
 shp=NULL,      # Shapefile Object Spatial*DataFrame. If NULL, it reads shpname with rgdal::readOGR.
 ncells=99,     # Approximate number of cells in either direction to determine cellsize.
 cellsize=NA,   # Cell size in coordinate units (usually degrees or m). Computed from ncells if NA.
 ncellwarn=1000,# Warn if there will be more cells than this. To prevent e.g. accidental degrees instead of km.
 column="",     # Name of column to use for z dimension in raster. Empty string for interactive selection.
 ascname=NA,    # Output file name. If NA, inferred from shpname or shp.
 verbose=FALSE, # Report readOGR progress?
 ...)           # More arguments passed to raster::rasterize, like overwrite=TRUE
{
# if shp is missing/default, read shpname:
if(is.null(shp)) 
 {
 shp <- rgdal::readOGR(dsn=shpname, 
                       layer=basename(tools::file_path_sans_ext(shpname)),
                       verbose=verbose)
 if(is.na(ascname)) ascname <- sub(".shp", ".asc", shpname)
 } else
 if(is.na(ascname)) ascname <- paste0(deparse(substitute(shp)),".asc")
# target raster extend and resolution:
e <- extent(shp) 
if(is.na(cellsize)) cellsize <- mean(c((e@xmax-e@xmin), (e@ymax-e@ymin))/ncells)
nx <- (e@xmax-e@xmin)/cellsize # this seems revertive from the previous line, but
ny <- (e@ymax-e@ymin)/cellsize # is needed because ncells differ in both directions
cont <- TRUE # continue by default
if(max(nx,ny)>ncellwarn) cont <- readline(paste0("Raster will be large: nx=",
    round(nx,1), ", ny=",round(ny,1)," (with cellsize=", round(cellsize,4),", xmin=",
    round(e@xmin,2), ", xmax=",round(e@xmax,2),"). Continue? y/n: "))
cont <- tolower(cont) %in% c("y", "yes", "t", "true", "")
if(!cont) return(list(nx=nx, ny=ny, cellsize=cellsize, extend_shp=e))
r <- raster(ncol=nx, nrow=ny)
extent(r) <- extent(shp)
resdif <- abs((yres(r) - xres(r)) / yres(r) )
if(resdif > 0.01) stop("Horizontal (",round(xres(r),3),") and vertical (", round(yres(r),3),
    ") resolutions are too different (diff=",round(resdif,3), ", but must be <0.010).\n",
    "  Use a smaller cell size to achieve this (currently ",round(cellsize,1),").")
# column selection
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

