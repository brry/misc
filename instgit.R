
# Quickly install a package from github without having to install devtools
# Berry Boessenkool, December 2015, feedback welcome at berryb@gmx.de

# Tell your github users this:
## source("https://raw.githubusercontent.com/brry/misc/master/instgit.R")
## instgithub("brry/extremeStat")
## library(extremeStat)

# works only:
# - for pure R package structure repositories from the master branch
# - with read-write-remove permission at getwd()
# Installs package dependencies listed in 'Imports' and 'Depends', but ignores version requirements!
# tested only on windows 7 with R3.2.2

# Note: devtools::install_github is much more extensive!
# Note: drat is also much better than this quick hack.
# http://dirk.eddelbuettel.com/code/drat.html
# https://github.com/eddelbuettel/drat
# http://eddelbuettel.github.io/drat/DratForPackageAuthors.html


instgithub <- function(
   pk, # "user/package"
   cleanup=TRUE, # remove downloaded zipfile and folder with source code
   ...) # Further arguments passed to install.packages, untested so far
{
pkn <- strsplit(pk, "/")[[1]][2] # package name part
# Download the zip file with the source code:
suppressWarnings(
download.file(url=paste0("https://github.com/",pk,"/archive/master.zip"),
              destfile=paste0(pkn,".zip"))
) # suppress warnings like downloaded length 350226 != reported length 200
# unzip and rename the folder:
unzip(paste0(pkn,".zip"))
file.rename(paste0(pkn,"-master"), pkn)
# Find out dependencies - really not elegant at all!:
deps <- read.dcf(paste0(pkn, "/DESCRIPTION"), fields="Imports")
deps2<- read.dcf(paste0(pkn, "/DESCRIPTION"), fields="Depends")
deps <- paste(deps,deps2, sep=",")
deps <- gsub("\n", "", deps) # remove line breaks
while(grepl(" ", deps)) deps <- gsub(" ", "", deps) # remove spaces
deps <- gsub("NA", "", deps) # remove NAs for packgages not listing fields
deps <- strsplit(deps, ",")[[1]] # split entries
deps <- deps[deps!=""] # NA leftover
deps <- sapply(strsplit(deps, "(", fixed=T), "[", 1) # remove version restrictions
deps <- deps[deps!="R"] # remove R (often in depends with version)
isinst <- deps %in% rownames(installed.packages())
depsinst <- deps[isinst]
deps <- deps[!isinst] # install only new packages
# tell user about installing dependencies:
if(!all(is.na(depsinst)))
   message("--- The following dependencies were already installed, but versions are unchecked: \n ",
           paste(depsinst, collapse=", "))
if(!all(is.na(deps)))
   message("--- instgithub will now install the following dependencies: \n ",
           paste(deps, collapse=", "))
flush.console()
# install dependencies:
dummy <- lapply(deps, install.packages, ...)
# actually install the package itself:
message("--- instgithub will now install ", pkn, " ...")
flush.console()
install.packages(pkn, repos=NULL, type="source", ...)
# clean up:
if(cleanup)
  {
  unlink(pkn, recursive=TRUE)
  unlink(paste0(pkn,".zip"))
  }
} #end of function


# Worked fine on my computer for:
if(FALSE){
instgithub("talgalili/installr")
instgithub("hadley/readxl")
instgithub("mages/googleVis") # many dependencies!
instgithub("twitter/AnomalyDetection")
instgithub("yihui/knitr")
instgithub("ramnathv/slidify")

# didn't work, neither with devtools::install_github("jrnold/ggthemes"):
instgithub("jrnold/ggthemes") # many many dependencies! # could not find function "ggproto"
}
