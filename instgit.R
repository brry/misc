
# Quickly install a package from github without having to install devtools
# Berry Boessenkool, December 2015, feedback welcome at berryb@gmx.de

# Tell your github users this:
## source("https://raw.githubusercontent.com/brry/misc/master/instgit.R")
## instgithub("brry/extremeStat")
## library(extremeStat)

# works only:
# - for pure R package structure repositories
# - from the master branch
# - with read-write-remove permission at getwd()
# - for package dependencies all listed in 'Imports' ('Depends' etc might be added later)
# tested only:
# - on windows 7 with R3.2.2

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
download.file(url=paste0("https://github.com/",pk,"/archive/master.zip"),
              destfile=paste0(pkn,".zip"))
unzip(paste0(pkn,".zip"))
file.rename(paste0(pkn,"-master"), pkn)
# Dependencies - really not elegant at all!
deps <- read.dcf(paste0(pkn, "/DESCRIPTION"), fields="Imports")
deps <- strsplit(deps, ", ")[[1]]
deps <- sapply(strsplit(deps, " ", fixed=T), "[", 1) # remove version restrictions
deps <- deps[!deps %in% rownames(installed.packages())] # install only new packages
# install dependencies
dummy <- lapply(na.omit(deps), install.packages, ...)
# actually install the package itself:
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
instgithub("mages/googleVis")
}
