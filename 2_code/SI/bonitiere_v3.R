# Stark ver?nderte und kommentierte R-?bertragung der SILVA-Funktion
# zur Ertragstafelbonitierung

# Peter Biber, 23.  3. 2009
# ?berabeitet   6. 11. 2009
# ?berabeitet  26.  3. 2012
# ?berarbeitet 22.  4. 2015 Jetzt kann optional auch die Fichte nicht nur nach h100, 
#                           sondern auch nach hg bonitiert werden (auch Assmann Franz)
# ?berarbeitet 14. 10. 2015 Erle kann bonitiert werden (Mitscherlich, hg), L?rche auch (Schober EuL?, hg), Tanne auch (Hausser, hg)

#-------------------------------------------------------------------------------

# Lade vorhandene Bonit?tsf?cher

# Alle Baumarten werden nach hg bonitiert.


# Nicht gut, wenn dieses Skript als source aus einem anderen Verzeichnis aufgerufen wird.
# => TODO allgemeine L?sung finden.

load("2_code/SI/boni.faecher.fi.RData")     # Fichte Assmann-Franz h100
load("2_code/SI/boni.faecher.fi.hg.RData")  # Fichte Assmann-Franz hg
load("2_code/SI/boni.faecher.ki.RData")     # Kiefer Wiedemann, hg
load("2_code/SI/boni.faecher.bu.RData")     # Buche Schober, hg
load("2_code/SI/boni.faecher.ei.RData")     # Eiche J?ttner, hg
load("2_code/SI/boni.faecher.dg.RData")     # Douglasie Bergel, hg
load("2_code/SI/boni.faecher.er.RData")     # Erle Mitscherlich hg
load("2_code/SI/boni.faecher.la.RData")     # Europ?ische L?rche Schober, hg
load("2_code/SI/boni.faecher.ta.RData")     # Tanne Hausser, hg

#-------------------------------------------------------------------------------
# function chapman

# Berechnet den Wert der Chapman-Richards-Funktion
# y = a * (1 - exp(-b * t)) ^ c
# an der Stelle t

chapman <- function(t, a, b, c) {
           y  <- a * exp(c * log(1-exp(-b * t)))
           return(y)
} # function chapman

#-------------------------------------------------------------------------------
# function bonitaet 

# Berechnet die absolute Bonitaet eines Waldbestandes.
# tafel: Ertragstafel-Bonit?tsf?cher, muss ein data.frame sein, der 
# mindestens die drei Spalten a, b und c enh?lt. Jede Zeile ist eine
# Ertragsklasse, deren H?henverlauf als
# h = a * (1 - exp(-b * alter)) ^ c
# definiert ist.
# Der Dataframe braucht nicht sortiert zu sein.
# alter, hoehe: Entsprechende Kennwerte des Bestandes.
# bezug.alter ist das Alter, f?r das man die Bonit?t haben will.

bonitaet  <- function(tafel, alter, hoehe, bezug.alter) {

             # 1. Schritt 
             # Einf?gen einer Spalte mit den Ertragstafelh?hen zum 
             # ?bergebenen Alter und Bezugsalter.
             tafel$et_h  <- mapply(chapman, a = tafel$a, b = tafel$b, c = tafel$c, t = alter)
             tafel$et_hb <- mapply(chapman, a = tafel$a, b = tafel$b, c = tafel$c, t = bezug.alter)
             # Sortieren
             
             
             # browser()
             
             
             index      <- order(tafel[, "et_h"])
             tafel      <- tafel[index,]
             
             # 2. Schritt
             # repeat-Schleife, durchl?uft Ertragstafelfl?cher von unten,
             # bis die Ertragstafelh?he bei alter gr??er ist, als die
             # Bestandesh?he, bzw. bis zum Ende von tafel.
             i <- 0
             repeat {
                    i <- i + 1
                    if ((tafel[i,]$et_h >= hoehe) | (i == nrow(tafel))) break
             } # repeat
            
             # 3. Schritt
             # Ermitteln der Bonit?t durch Inter- bzw. Extrapolation aus tafel. 
             if ((i == nrow(tafel)) & (tafel[i,]$et_h < hoehe)) {
                # Bonit?t ist besser als eine 1. Ekl.
                # Es wird aus der h?chsten und zweith?chsten Bonit?t
                # extrapoliert.
                differenz  <- tafel[i,]$et_h - tafel[i-1,]$et_h 
			          bruchteil  <- (hoehe - tafel[i,]$et_h) / differenz
                diff.bez   <- tafel[i,]$et_hb - tafel[i-1,]$et_hb 
                erg        <- tafel[i,]$et_hb + bruchteil * diff.bez
		         } # Bonit?t ist besser als eine 1. Ekl.
		         else {
		              if (i == 1) { 
                     # Bonit?t ist schlechter als die niedrigste Ekl.
                     # Es wird aus der niedrigsten und zweitniedrigsten
                     # Bonit?t extrapoliert.
                     differenz  <- tafel[i+1,]$et_h - tafel[i,]$et_h 
			               bruchteil  <- (tafel[i,]$et_h - hoehe) / differenz
                     diff.bez   <- tafel[i+1,]$et_hb - tafel[i,]$et_hb 
                     erg        <- tafel[i,]$et_hb - bruchteil * diff.bez
		              } # Bonit?t ist schlechter als die niedrigste Ekl.
		              else {
		                 # Bonit?t liegt innerhalb des H?henf?chers.
		                 # Lineare Interpolation.
                     differenz  <- tafel[i,]$et_h - tafel[i-1,]$et_h     
			               bruchteil  <- (hoehe - tafel$et_h[i-1]) / differenz
                     diff.bez   <- tafel[i,]$et_hb - tafel[i-1,]$et_hb                      			
                     erg        <- tafel[i-1,]$et_hb + bruchteil * diff.bez 			
		              } # Bonit?t liegt innerhalb des H?henf?chers
		         } # ?bergeordnetes else
		        
             return(erg) 

} # function bonitiere

#-------------------------------------------------------------------------------

# Jetzt eine sch?ne Funktion, die auf data.frames mit verschiedenen Arten mit 
# mapply angewandt werden kann.
# Sie kapselt "bonitaet" und sucht noch vorher den richtigen F?cher aus.
# 22. 4. 2015 neue Eingabevariable: Fichte.hdef, kann die Werte "h100" (Voreinstellung) oder "hg" annehmen.
#             Wenn also nichts ver?ndert wird, gilt h100, wenn man hg will, muss man die Variable ?ndern
bonitiere.art <- function(art, alter, hoehe, bezug.alter, Fichte.hdef = "h100") {
         tafel.text <- switch(as.character(art), "10" = "boni.faecher.fi", "20" = "boni.faecher.ta", "30" = "boni.faecher.ki", "40" = "boni.faecher.la", "50" = "boni.faecher.bu", "60" = "boni.faecher.ei", "70" = "boni.faecher.dg", "91" = "boni.faecher.er")
         if(Fichte.hdef == "hg" & art == 10) { tafel.text <- paste(tafel.text, "hg", sep = ".") }
         call.text  <- paste("bonitaet(", tafel.text, ", ", alter, ", ", hoehe, ", ", bezug.alter, ")", sep = "")
         erg        <- eval(parse(text = call.text))
         return(erg)
} # function boni.art

# Anwendungsbeispiel

# Wenn die Fichtenh?hen sich als h100 verstehen, muss man gar nichts tun
# set3.boni$Bon40  <- mapply(bonitiere.art, art = set3.boni$Artcode, alter = set3.boni$A1, hoehe = set3.boni$H1.m., MoreArgs = list(bezug.alter = 40))
# set3.boni$Bon100 <- mapply(bonitiere.art, art = set3.boni$Artcode, alter = set3.boni$A1, hoehe = set3.boni$H1.m., MoreArgs = list(bezug.alter = 100))

# Wenn die Fichtenh?hen sich als hg verstehen, dann
# set3.boni$Bon40  <- mapply(bonitiere.art, art = set3.boni$Artcode, alter = set3.boni$A1, hoehe = set3.boni$H1.m., MoreArgs = list(bezug.alter = 40, Fichte.hdef = "hg"))
# set3.boni$Bon100 <- mapply(bonitiere.art, art = set3.boni$Artcode, alter = set3.boni$A1, hoehe = set3.boni$H1.m., MoreArgs = list(bezug.alter = 100, Fichte.hdef = "hg"))

#-------------------------------------------------------------------------------