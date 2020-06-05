# spaced Learning
# by Berry Boessenkool, berry-b@gmx.de, Apr 2019
# Based on https://ncase.me/remember by Nicky Case
# Found through https://flowingdata.com/2018/10/29/interactive-comic-to-remember-things-better

# Wort Ideen:
# https://fr.wikipedia.org/wiki/Wikip%C3%A9dia:Accueil_principal

learnVocab <- function()
{
# Testwunsch abfragen wenn mind. 3 Stunden seit dem letzten Test:
now <- "nein"
lastchange <- as.POSIXct(readLines("counter.txt")[2], format="%F %T")
tdiff <- as.numeric(difftime(Sys.time(), lastchange, units="hours"))
if(tdiff>3) now <- readline("Möchtest du jetzt französische Vokabeln üben? j/n (ENTER=ja): ")


if(now %in% c("ja","j","")) 
{
owd <- setwd("C:/Dropbox/R/misc/spacedLearning")

# Neue Vokabeln:
nnew <- 3
for(i in 1:nnew){
new <- readline(paste0("Neue Vokabel (F;D) ",i,"/",nnew,": "))
if(new!="") cat(paste(new, ";1\n"), file="Vokabeln.csv", append=TRUE)
}

# Welche Elemente werden heute abgefragt:
counter <- as.numeric(readLines("counter.txt")[1])
if(counter==64) counter <- 0
cat(counter+1,as.character(Sys.time()), file="counter.txt", sep="\n")
levels_to_test <- which(c(
TRUE               , # 1: always
counter %%  2 ==  1, # 2: test nr 1 3 5 7 9 11 ...
counter %%  4 ==  2, # 3: 2 6 10 14 18 22 26 30 34 38 ...
counter %%  8 ==  4, # 4: 4 13 20 29 36 45 52 61
counter %% 16 == 12, # 5: 12 28 44 60
counter %% 32 == 24, # 6: 24 59
counter == 56     )) # 7: 56

# Vokabeln tatsächlich abfragen:
voc <- read.csv2("Vokabeln.csv", stringsAsFactors=FALSE)

# sprintf(paste0("%",max(nchar(voc$DE)),"s"), "due")

for(r in which(voc$LEVEL %in% levels_to_test))
{
cat("\014")
de <- paste0("DE: ", removeSpace(voc$DE[r]), " (ENTER -> Lösung)")
known <- readline(de)
cat("\014")
known <- readline(paste0(de, ", FR: ", removeSpace(voc$FR[r]), ". Gewusst? j/n (ENTER = ja): "))
known <- known %in% c("ja","j","")
voc$LEVEL[r] <- if(known) voc$LEVEL[r] +1   else   1
}

write.csv2(voc, file="Vokabeln.csv", row.names=FALSE, quote=FALSE)

setwd(owd)
}
} # end of function
