# Berlin R User Group API challenge, March 2017
# What proportion of members are women?
# Solution from Berry Boessenkool, berry-b@gmx.de

# https://github.com/wolass/BRUG_API


# Needed packages
installifneeded <- function(n) for(i in n) if(!requireNamespace(i,quietly=TRUE)) install.packages(i)
installifneeded(c("genderizeR","berryFunctions","devtools","rvest","pbapply"))
rm(installifneeded)
devtools::install_github("rladies/meetupr")

# Get your meetup API key here:
browseURL("https://secure.meetup.com/meetup_api/key")
api_key <- "57***************************9"

# Get member names:
members <- meetupr::get_members("Berlin-R-Users-Group", api_key)
members <- unlist(lapply(members, "[", "name"))
members <- data.frame(full=members, stringsAsFactors=F)
members$full <- gsub("Dr.", "", members$full)
members$full <- berryFunctions::removeSpace(members$full)

# Get first names:
members$given <- sapply(strsplit(members$full, "[ _.-]"), "[", 1)
members$nsep <-  sapply(strsplit(members$full, "[ _.-]"), length)-1
members$nchar <- nchar(members$given)
length(unique(members$given)) # 801

# Estimate gender (max 1000 name requests per day in free plan)
gender <- genderizeR::findGivenNames(unique(members$given)) # 798 unique names checked
# save(gender, file="gender.Rdata")
load("gender.Rdata")

# Merge:
members$given <- tolower(members$given)
members <- merge(members, gender, by.x="given", by.y="name", all=TRUE)


# Harvesting Wikipedia data - code modified from code by Andreas Busjahn
wikigender <- pbapply::pblapply(LETTERS, function(l) # takes only a few seconds
{ 
url <- paste0('https://de.wikipedia.org/wiki/Liste_von_Vornamen/',l)
download.file(url, destfile='name_temp.html', quiet=TRUE)
htmlsource <- xml2::read_html('name_temp.html')
htmlnodes <- rvest::html_nodes(htmlsource,'p')
names <- rvest::html_text(htmlnodes)
names <- names[grep('[\U{2642}\U{2640}]',names)[-1]]
names <- lapply(names, FUN=strsplit, split=',')
names <- unlist(names)
gm <- grepl('\U{2642}',names)
gf <- grepl('\U{2640}',names)
gb <- gm & gf
gm <- gm & !gf
gf <- !gm & gf
names <- gsub("\U{2642}","",names)
names <- gsub("\U{2640}","",names)
names <- berryFunctions::removeSpace(names)
names <- data.frame(name=names, stringsAsFactors=FALSE)
names$wikigender <- ""
names$wikigender[gm] <- "male"
names$wikigender[gf] <- "female"
names$wikigender[gb] <- "both"
names
})
unlink('name_temp.html')

wikigender <- do.call(rbind, wikigender)  # 9'549 names
table(wikigender$wikigender)
#  both female   male 
#   483   3511   5555 
# This list may be biased towards male names...

# Merge:
wikigender$name <- tolower(wikigender$name)
members <- merge(members, wikigender, by.x="given", by.y="name", all.x=TRUE)

# save(members, file="members.Rdata")
load("members.Rdata")

members$probability <- as.numeric(members$probability)
hist(members$probability, breaks=50, col="orange") # most estimates are quite certain

members$gender[is.na(members$gender)] <- "unknown"
members$wikigender[is.na(members$wikigender)] <- "unknown"

table(members$gender)
# female    male unknown 
#    259     725     148 

table(members$wikigender)
#   both  female    male unknown 
#     58     139     506     429 

259/(259+725)*100 # 26 %
139/(139+506)*100 # 22 %

# About a quarter of the registered members is female
