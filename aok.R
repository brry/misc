# AOK Ausgaben  2015  aok.de/nordost bleib gesund Heft
# Barchart von Berry Boessenkool, berry-b@gmx.de, Sept 2016

AusgabenSumme <- 6988291108 # Euro
Versicherte <- 1721953 # Anzahl
EinnahmenSumme <- 7103071930 # Euro

# Ausgaben 2015 Jahressumme laut 'Infografik'
aok <- read.table(as.is=TRUE, header=TRUE, sep=",", text=
"anteil, absolut, properson, posten
13.8, 964831273, 560.31, Ärztliche Behandlung
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
 0.7,  47468617,  27.57, Früherkennungsmaßnahmen
 0.2,  17421266,  10.12, Schwanger- + Mutterschaft _ ohne stationäre Entbindung
 4.9, 340297797, 197.62, Häusliche Krankenpflege _ inkl. Behandlungspflege
 1.3,  92433809,  53.68, Dialyse
 1.2,  82843147,  48.11, Sonstige Leistungsausgaben
 2.0, 138031421,  80.16, Krankengeld
 0.6,  40221542,  23.36, Sonstige Ausgaben
")

# Daten prüfen

sum(aok$anteil) # 100.1 (gut)
sum(aok$absolut) - AusgabenSumme # 0 (sehr gut)
round(aok$absolut/Versicherte,2) - aok$properson # alle 0 (sehr gut)
round(aok$absolut/AusgabenSumme*100,1) - aok$anteil # alle 0 (sehr gut)

aok <- sortDF(aok, "absolut", decreasing=FALSE)

AusgabenSumme/Versicherte/12 # 338.20 Euro pro Person pro Monat
# Und da wundert man sich noch, dass das deutsche Gesundheitssystem zu teuer ist...

(EinnahmenSumme-AusgabenSumme)/Versicherte/c(1,12) # 66.657349  5.554779 # Gewinn
(EinnahmenSumme-AusgabenSumme)/1e6


expr <- function(){
par(mar=c(3.5,12,5,1), mgp=c(2,0.6,0), las=1, cex=0.9)
bp <- barplot(aok$absolut/1e9, names.arg=sapply(strsplit(aok$posten," _ "), "[",1),
        xlab="Ausgabensumme  [Mrd Euro]", xaxt="n", col=seqPal(nrow(aok)), 
        ylim=c(0.7, 21.1), horiz=TRUE)
axis(1, lwd=0, lwd.ticks=1)
mtext("Grafik: Boessenkool (github.com/brry)\n  Quelle: AOK nordost bleib gesund Heft Sept 2016",
      side=1, adj=0.1, line=-1.3, outer=TRUE, cex=0.7, col="grey60")
title(main="AOK Ausgaben 2015 (1.7 Mio Versicherte)", line=-1.5, adj=0.05, outer=TRUE)
promonat <- pretty2(c(0,aok$absolut/Versicherte/12))
axis(3, promonat*12*Versicherte/1e9, promonat, lwd=0, lwd.ticks=1)
mtext("Ausgaben pro Versicherten  [Euro/Monat]", line=1.8)
text(1.5, 10, paste0("Ausgaben Summe:\n338.20 Euro/Versicherte/Monat\n",
     "4058.35 Euro/Versicherte/Jahr\n~6'988.3 Mio Euro insgesamt\n\n",
     "Gewinn:\n5.55 Euro/Versicherte/Monat\n66.66 Euro/Versicherte/Jahr\n~114.8 Mio Euro insgesamt"), col="red3")
text(0.05, bp[11:18], paste0(round(aok$anteil[11:18]),"%"), adj=0, cex=0.9, col="white")
#text(0.35, 6, "}", cex=12, col="gray97")
textField(0.37, 6, paste0("Rest:\n",round(sum(aok$anteil[1:11])),"%"), cex=0.9, 
          fill="gray80", mar=0.7, col="white")
text(aok$absolut/1e9 + 0.05, bp[,1]-0.1, sapply(strsplit(aok$posten," _ "), "[",2), adj=0, cex=0.8)
}

pdf("AOK.pdf", height=5)
expr()
dev.off()

png("AOK.png", height=5, width=7, units="in", res=300)
expr()
dev.off()

#system2("open", "aok.pdf")


