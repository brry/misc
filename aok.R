# AOK Ausgaben   aok.de/nordost Suchwort Geschäftsbericht 2016 
# Nützliche Graphik zu den Piktogrammen im bleib gesund Heft
# Barchart von Berry Boessenkool, berry-b@gmx.de, Sept 2016, Okt 2017

library(berryFunctions) # for sortDF, round0, seqPal, openFile
# see https://github.com/brry/berryFunctions#installation

aok <- function(jahr)
{
Zahlen <- read.table(as.is=TRUE, header=TRUE, sep=",", text="
Jahr, Versicherte, EinnahmenSumme, AusgabenSumme
2015,     1721953,     7103071930,    6988291108
2016,     1737688,     7317202030,    7250854951
")
rownames(Zahlen) <- Zahlen$Jahr
Versicherte    <- Zahlen[as.character(jahr), "Versicherte"]
EinnahmenSumme <- Zahlen[as.character(jahr), "EinnahmenSumme"]
AusgabenSumme  <- Zahlen[as.character(jahr), "AusgabenSumme"]

# Ausgaben 2015 Jahressumme laut 'Infografik' bleib gesund Heft Sept 2016
aok_2015 <- read.table(as.is=TRUE, header=TRUE, sep=",", text=
"anteil, absolut, properson, posten
13.8, 964831273, 560.31, Aerztliche Behandlung
 3.0, 209036712, 121.40, Zahnarzt (ohne Zahnersatz)
 1.6, 113307812,  65.80, Zahnersatz
17.0,1187202169, 689.45, Arzneimittel
 3.8, 262112399, 152.22, Hilfsmittel (zB Rollator)
 2.5, 172430804, 100.14, Heilmittel
37.6,2624292385,1524.02, Krankenhausbehandlung
 4.7, 325626396, 189.10, Verwaltungsausgaben
 3.6, 254689190, 147.91, Fahrkosten
 1.1,  78635886,  45.67, Vorsorge + Rehabilitation
 0.5,  37408483,  21.72, Schutzimpfungen
 0.7,  47468617,  27.57, Frueherkennungsmassnahmen
 0.2,  17421266,  10.12, Schwanger- + Mutterschaft _ ohne stationaere Entbindung
 4.9, 340297797, 197.62, Haeusliche Krankenpflege _ inkl. Behandlungspflege
 1.3,  92433809,  53.68, Dialyse
 1.2,  82843147,  48.11, Sonstige Leistungsausgaben
 2.0, 138031421,  80.16, Krankengeld
 0.6,  40221542,  23.36, Sonstige Ausgaben
")
# Ausgaben 2016 Jahressumme laut 'Infografik' bleib gesund Heft Sept 2017
aok_2016 <- read.table(as.is=TRUE, header=TRUE, sep=",", text=
"anteil, absolut, properson, posten
14.2,1026898375, 590.96, Aerztliche Behandlung
 3.0, 216781617, 124.75, Zahnarzt (ohne Zahnersatz)
 1.5, 107802368,  62.04, Zahnersatz
16.6,1204904901, 693.40, Arzneimittel
 3.7, 268852923, 154.72, Hilfsmittel (zB Rollator)
 2.6, 187586561, 107.95, Heilmittel
37.0,2685418902,1545.40, Krankenhausbehandlung
 4.8, 345437648, 198.79, Verwaltungsausgaben
 3.6, 263194398, 151.46, Fahrkosten
 1.2,  83711954,  48.17, Vorsorge + Rehabilitation
 0.5,  36586319,  21.05, Schutzimpfungen
 0.7,  49514180,  28.49, Frueherkennungsmassnahmen
 0.2,  17927111,  10.32, Schwanger- + Mutterschaft _ ohne stationaere Entbindung
 4.9, 353023341, 203.16, Haeusliche Krankenpflege _ inkl. Behandlungspflege
 1.3,  91425118,  52.61, Dialyse
 1.3,  95294099,  54.84, Sonstige Leistungsausgaben
 2.1, 150352569,  86.52, Krankengeld
 0.9,  66142567,  38.06, Sonstige Ausgaben
")
if(jahr==2015) aok <- aok_2015 else
if(jahr==2016) aok <- aok_2016 else
stop("Keine Daten vorhanden für Jahr ", jahr)
 
#aok <- sortDF(aok, "absolut", decreasing=FALSE)
aok <- aok[order(aok_2016$absolut), ]

# Daten pruefen
message("Datenprüfung ", jahr, "   Ziel: 100, 0, 0, 0.   Ist: ",
toString(c( sum(aok$anteil) ,
sum(aok$absolut) - AusgabenSumme ,
sum(round(aok$absolut/Versicherte,2) - aok$properson) ,
sum(round(aok$absolut/AusgabenSumme*100,1) - aok$anteil))) )

# Barplot
par(mar=c(3.5,13,5,1), mgp=c(2,0.6,0), las=1, cex=0.9)
bp <- barplot(aok$absolut/1e9, names.arg=sapply(strsplit(aok$posten," _ "), "[",1),
        xlab=paste0("Ausgabensumme  [Mrd Euro, ",jahr,"]"), xaxt="n", 
        col=seqPal(nrow(aok)), xlim=c(0,2.8), ylim=c(0.7, 21.1), horiz=TRUE)
axis(1, lwd=0, lwd.ticks=1)
mtext(paste("Grafik: Boessenkool (github.com/brry/misc)\n",
            "Quelle: AOK nordost bleib gesund Heft Sept", jahr+1),
      side=1, adj=0.1, line=-1.3, outer=TRUE, cex=0.7, col="grey60")
title(main=paste0("AOK Ausgaben ",jahr," (",round(Versicherte/1e6,2),
                  " Mio Versicherte)"), line=-1.5, adj=0.05, outer=TRUE)
promonat <- pretty2(c(0,aok$absolut/Versicherte/12))
axis(3, promonat*12*Versicherte/1e9, promonat, lwd=0, lwd.ticks=1)
mtext("Ausgaben pro Versicherten  [Euro / Monat]", line=1.8)

AusVers <- AusgabenSumme/Versicherte
Gewinn <- EinnahmenSumme-AusgabenSumme
GewVers <- Gewinn/Versicherte
text(1.5, 10, paste0("Ausgaben Summe:\n", 
  round0(AusVers/12,        2, big.mark="'"), " Euro/Versicherte/Monat\n",
  round0(AusVers,           2, big.mark="'"), " Euro/Versicherte/Jahr\n",
  round0(AusgabenSumme/1e6, 1, big.mark="'"), " Mio Euro insgesamt\n\n",
  "Gewinn AOK:\n",
  round0(GewVers/12, pre=0, 2, big.mark="'"), " Euro/Versicherte/Monat\n",
  round0(GewVers,    pre=0, 2, big.mark="'"), " Euro/Versicherte/Jahr\n",
  round0(Gewinn/1e6,        1, big.mark="'"), " Mio Euro insgesamt\n\n"), 
  col="red3")
text(0.05, bp[11:18], paste0(round(aok$anteil[11:18]),"%"), adj=0, cex=0.9, col="white")
#text(0.35, 6, "}", cex=12, col="gray97")
textField(0.37, 6, paste0("Rest:\n",round(sum(aok$anteil[1:11])),"%"), cex=0.9, 
          fill="gray80", mar=0.7, col="white")
text(aok$absolut/1e9 + 0.05, bp[,1]-0.1, sapply(strsplit(aok$posten," _ "), "[",2), adj=0, cex=0.8)

# Ausgabe
invisible(list(AusgabenSumme=AusgabenSumme, Versicherte=Versicherte, 
               EinnahmenSumme=EinnahmenSumme, aok=aok))
}


pdf("AOK.pdf", height=5) ; aok(2015) ; aok(2016) ; dev.off()
png("AOK_2015.png", height=5, width=7, units="in", res=300) ; aok(2015); dev.off()
png("AOK_2016.png", height=5, width=7, units="in", res=300) ; aok(2016); dev.off()

openFile("aok.pdf")

