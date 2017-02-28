# El Nino plot, color intensity signifies how many regions show a strong anomaly
# Berry Boessenkool, Feb 2017, berry-b@gmx.de

# Data ----

# Oceanic Nino Index: Sea surface temperature (SST):
if(!exists("ONI"))
   ONI <- read.table("http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices", header=TRUE)
ONI$date <- as.Date(paste(ONI$YR,ONI$MON,15, sep="-"))
# colnames always similar structure:
colnames(ONI)[colnames(ONI)=="ANOM"] <- "ANOM.0"


# Package ----

# install.packages("berryFunctions") 
library(berryFunctions) # for function addAlpha


# Plot setup ----

png("ElNino.png", width=7*200, height=5*200, res=300)
par(mar=c(3.3,3.5,2,0.5), mgp=c(2,0.7,0), las=1)
plot(ONI$date, ONI$ANOM.3, type="n", xaxt="n", ylim=c(-2.5, 4),
     ylab="Sea Surface Temperature Anomaly  [\U{00B0}C]", xlab="",
     main="ENSO - Oceanic Nino Index in 4 Regions")
mtext("http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices", 1, 2, adj=1)
# more date axis labels:
axis.Date(1, at=seq(as.Date("1985-01-01"), max(ONI$date), by="5 year"))
abline(h=0)

# Actually plot polygons and lines ----

for(n in 0:3)
  {
  t <- ONI[,paste0("ANOM.",n)]
  # add lines for all 4 regions in semitranparent color:
  lines(x=ONI$date, t, col=addAlpha("black"))
  # set zeros at the end to avoid weird polygons
  t[length(t)] <- 0
  polygon(x=ONI$date, replace(t, t< 0.5, 0.5), col=addAlpha("blue"), border=NA)
  polygon(x=ONI$date, replace(t, t>-0.5,-0.5), col=addAlpha("red"),  border=NA)
  }
rm(n,t)

# Add explanation right into plot
legend("top",    "        El Nino", text.col="blue", bty="n")
legend("bottom", "La Nina", text.col="red" , bty="n")

dev.off()


# El Nino Region ----

reg <- read.table(header=TRUE, as.is=TRUE, text="
region minlat maxlat minlon maxlon    col lablat lablon
1+2       -10      0    -90    -80  'green'     -5    -85
3          -5      5   -150    -90    'red'      0   -110
4          -5      5   -200   -150   'blue'      0   -190 
3.4        -5      5   -170   -120 'orange'      0   -140 ")
reg$pop <- apply(reg, MARGIN=1, function(x)paste0(names(x[1:5]),": ",x[1:5], collapse="<br>"))

library(leaflet)
map <- leaflet(reg) %>% addTiles() %>% 
 addRectangles(~minlon,~minlat,~maxlon,~maxlat, popup=~pop, col=~col) %>% 
 addLabelOnlyMarkers(~lablon,~lablat,label=~region, labelOptions=
    labelOptions(noHide=T,textsize="14px",offset=c(-5,-10),textOnly=T)) %>%#,
                # style=~list("color"=~col, "font-size"="14px")))  %>% 
 setView(-150, 0, zoom=3)  ; map

# install.packages("mapview")
library(mapview)
# mapshot(map, file="ElNinoMap.png") # this takes a few minutes
