
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
}

# example test cases, work fine on windows 7 with current R3.2.2 + write permission at getwd:
if(FALSE){
instgithub(pk="brry/extremeStat")
library(extremeStat)
instgithub("talgalili/installr")
instgithub("hadley/readxl")
}
