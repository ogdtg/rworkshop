##############################################################################
##                                                                          ##
##                    BEISPIELE: EIGENE FUNKTIONEN IN R                     ##
##                                                                          ##
##  Dieses Script zeigt anhand von praktischen Beispielen, wie man          ##
##  eigene Funktionen in R schreibt und einsetzt.                            ##
##                                                                          ##
##  Jedes Kapitel kann unabhängig ausgeführt werden.                        ##
##  Empfehlung: Code Zeile für Zeile ausführen (Ctrl+Enter)                 ##
##                                                                          ##
##############################################################################

library(dplyr)


# ============================================================================
# KAPITEL 1: Die allererste Funktion
# ============================================================================
#
# Ziel: Aus einer Zeichenkette eine Begrüssung machen.
# "Felix" → "Hallo, Felix!"
#
# Schritt 1: Den Code OHNE Funktion schreiben
name <- "Felix"
paste0("Hallo, ", name, "!")

# Schritt 2: In eine Funktion verpacken
begruessen <- function(name) {
  return(paste0("Hallo, ", name, "!"))
}

# Funktion aufrufen
begruessen(name = "Olaf")
begruessen("Anna")
begruessen("Welt")

# ============================================================================
# KAPITEL 2: Argumente – was die Funktion "hereinnimmt"
# ============================================================================

# --- 2a) Ein Argument ---
quadrat <- function(x) {
  return(x^2)
}

quadrat(5)
quadrat(12)
quadrat(c(1, 2, 3, 4, 5))   # funktioniert auch mit einem Vektor!

# --- 2b) Mehrere Argumente ---
potenz <- function(basis, exponent) {
  return(basis^exponent)
}

potenz(2, 8)    # 2 hoch 8
potenz(10, 3)   # 10 hoch 3

# Argumente können auch mit Namen übergeben werden (Reihenfolge egal!)
potenz(exponent = 3, basis = 5)

# --- 2c) Default-Werte ---
# Wenn ein Argument einen Standardwert hat, muss es nicht angegeben werden.
potenz_mit_default <- function(basis, exponent = 2) {
  return(basis^exponent)
}

potenz_mit_default(4)      # exponent = 2 (Default)
potenz_mit_default(4, 3)   # exponent = 3 (überschrieben)
potenz_mit_default(4, 0.5) # Wurzel ziehen!

# --- 2d) Funktion ohne Argumente ---
aktuelles_jahr <- function() {
  return(lubridate::year(Sys.Date()))
}

aktuelles_jahr()


# ============================================================================
# KAPITEL 3: Praxisbeispiele – nützliche kleine Funktionen
# ============================================================================

# --- 3a) Bevölkerungsdichte berechnen ---
bev_dichte <- function(einwohner, flaeche_km2) {
  # Einwohner pro Quadratkilometer
  dichte <- einwohner / flaeche_km2
  return(round(dichte, 1))
}

bev_dichte(285000, 991)    # Kanton Thurgau
bev_dichte(1590000, 1729)  # Kanton Zürich
bev_dichte(1050000, 5959)  # Kanton Bern

# --- 3b) Prozentwert formatieren ---
# Aus 0.753 wird "75.3 %"
als_prozent <- function(wert, nachkommastellen = 1) {
  paste0(round(wert * 100, nachkommastellen), " %")
}

als_prozent(0.753)
als_prozent(0.5)
als_prozent(0.123456, nachkommastellen = 2)

# --- 3c) Schweizer Zahlenformat ---
# 1234567.8 → "1'234'567.8"
ch_format <- function(x, nachkommastellen = 0) {
  formatC(x,
          format  = "f",
          digits  = nachkommastellen,
          big.mark = "'")
}

ch_format(285000)
ch_format(1234567.89, nachkommastellen = 1)

# --- 3d) Altersklasse bestimmen ---
altersklasse <- function(alter) {
  dplyr::case_when(
    alter < 20          ~ "unter 20",
    alter < 40          ~ "20 bis 39",
    alter < 65          ~ "40 bis 64",
    .default            =  "65 und älter"
  )
}

altersklasse(15)
altersklasse(34)
altersklasse(c(5, 25, 50, 70))   # mit einem Vektor


# ============================================================================
# KAPITEL 4: Body – was in der Funktion passiert
# ============================================================================
#
# Der Body kann beliebig komplex sein: mehrere Schritte, Schleifen, etc.

bmi_berechnen <- function(gewicht_kg, groesse_m) {
  # Schritt 1: BMI-Formel anwenden
  bmi <- gewicht_kg / groesse_m^2

  # Schritt 2: WHO-Kategorisierung
  kategorie <- dplyr::case_when(
    bmi < 18.5 ~ "Untergewicht",
    bmi < 25   ~ "Normalgewicht",
    bmi < 30   ~ "Übergewicht",
    .default   =  "Adipositas"
  )

  # Schritt 3: lesbaren Output erstellen
  ergebnis <- paste0("BMI: ", round(bmi, 1), " → ", kategorie)
  return(ergebnis)
}

bmi_berechnen(70, 1.75)
bmi_berechnen(95, 1.70)
bmi_berechnen(55, 1.80)


# ============================================================================
# KAPITEL 5: Return – was die Funktion zurückgibt
# ============================================================================

# --- 5a) Explizites vs. implizites Return ---
# Explizit (empfohlen):
addiere_explizit <- function(a, b) {
  ergebnis <- a + b
  return(ergebnis)
}



# Implizit (letzter Ausdruck wird automatisch zurückgegeben):
addiere_implizit <- function(a, b) {
  a + b
}



addiere_explizit(3, 7)
addiere_implizit(3, 7)

# --- 5b) Mehrere Werte zurückgeben → als Liste ---
statistiken <- function(x, na_entfernen = TRUE) {
  return(list(
    n          = sum(!is.na(x)),
    mittelwert = round(mean(x, na.rm = na_entfernen), 2),
    median     = round(median(x, na.rm = na_entfernen), 2),
    sd         = round(sd(x, na.rm = na_entfernen), 2),
    min        = min(x, na.rm = na_entfernen),
    max        = max(x, na.rm = na_entfernen)
  ))
}


ergebnis <- statistiken(c(4, 8, 15, 16, 23, 42))
ergebnis$mittelwert
ergebnis$sd

# Alle Werte auf einmal ansehen:
ergebnis

# --- 5c) Frühzeitiger Ausstieg mit return() ---
# return() beendet die Funktion sofort – der Rest wird übersprungen
sicheres_teilen <- function(zaehler, nenner) {
  if (nenner == 0) {
    return(NA)   # sofortiger Abbruch
  }
  return(zaehler / nenner)
}

sicheres_teilen(10, 2)
sicheres_teilen(10, 0)   # gibt NA zurück, kein Fehler


# ============================================================================
# KAPITEL 6: Scoping – wo sucht R nach Variablen?
# ============================================================================

# --- Regel 1: Innen vor Aussen ---
# Eine Variable innerhalb der Funktion "maskiert" gleichnamige Variable aussen
x <- 100   # globale Variable

f_masking <- function() {
  x <- 1   # lokale Variable
  return(x)
}

f_masking()   # gibt 1 zurück (nicht 100)
x             # globale Variable unverändert

# --- Regel 2: Wenn innen nicht gefunden → suche aussen ---
multiplikator <- 5

skalieren <- function(wert) {
  return(wert * multiplikator)   # nutzt die globale Variable
}
skalieren(10)   # ergibt 50

# Besser: als Argument übergeben (macht die Funktion unabhängig)
skalieren_besser <- function(wert, multiplikator) {
  return(wert * multiplikator)
}
skalieren_besser(10, 5)

# --- Regel 3: Fresh Start ---
# Jeder Funktionsaufruf startet frisch – kein "Gedächtnis"
zaehler <- function() {
  n <- 0
  n <- n + 1
  return(n)
}

zaehler()   # 1
zaehler()   # immer noch 1 (nicht 2!)


# ============================================================================
# KAPITEL 7: Eingaben prüfen (Fehler, Warnungen, Infos)
# ============================================================================

# --- 7a) stop(): Fehler auslösen und abbrechen ---
bev_dichte_sicher <- function(einwohner, flaeche_km2) {
  if (!is.numeric(einwohner)) {
    stop("'einwohner' muss numerisch sein. Erhalten: ", class(einwohner))
  }
  if (!is.numeric(flaeche_km2)) {
    stop("'flaeche_km2' muss numerisch sein.")
  }
  if (flaeche_km2 <= 0) {
    stop("'flaeche_km2' muss grösser als 0 sein.")
  }
  return(round(einwohner / flaeche_km2, 1))
}

bev_dichte_sicher(285000, 991)           # funktioniert
# bev_dichte_sicher("viele", 991)        # Fehler (auskommentiert)
# bev_dichte_sicher(285000, 0)           # Fehler (auskommentiert)

# --- 7b) warning(): Warnung ausgeben, aber weitermachen ---
als_prozent_mit_warnung <- function(wert) {
  if (any(wert > 1 | wert < 0)) {
    warning("Einige Werte liegen ausserhalb [0, 1] – bitte prüfen.")
  }
  paste0(round(wert * 100, 1), " %")
}

als_prozent_mit_warnung(0.75)
als_prozent_mit_warnung(c(0.5, 1.2, -0.1))   # Warnung, aber Ergebnis wird noch ausgegeben

# --- 7c) message(): Information ausgeben ---
lade_daten <- function(pfad) {
  message("Lese Datei ein: ", pfad)
  message("Zeitstempel: ", Sys.time())
  # in der Praxis: readRDS(pfad)
}

lade_daten("Daten/beispiel.rds")


# ============================================================================
# KAPITEL 8: Funktionen in dplyr-Pipelines
# ============================================================================

# Eigene Funktionen lassen sich perfekt in mutate(), filter() etc. einsetzen.

# Beispieldaten erstellen
bevoelkerung <- tibble::tibble(
  kanton     = c("TG", "ZH", "BE", "BS", "AI"),
  einwohner  = c(285000, 1590000, 1050000, 178000, 16000),
  flaeche    = c(991, 1729, 5959, 37, 173)
)

bevoelkerung

# Eigene Funktion in mutate():
bevoelkerung %>%
  mutate(
    dichte         = bev_dichte(einwohner, flaeche),
    dichte_format  = ch_format(dichte),
    groesse        = case_when(
      einwohner < 100000  ~ "klein",
      einwohner < 500000  ~ "mittel",
      .default            = "gross"
    )
  )

# Funktion auch in filter() verwendbar:
bevoelkerung %>%
  filter(bev_dichte(einwohner, flaeche) > 500)


# ============================================================================
# KAPITEL 9: Funktion auf Liste/Vektor anwenden (mit purrr)
# ============================================================================

library(purrr)

kantone <- c("TG", "ZH", "BE", "SG", "GR")

# Gleiche Funktion auf jedes Element anwenden:
map_chr(kantone, ~ paste0("Kanton: ", .x))

# Mit eigener Funktion:
kanton_info <- function(kuerzel) {
  case_when(
    kuerzel == "TG" ~ "Thurgau, Hauptort: Frauenfeld",
    kuerzel == "ZH" ~ "Zürich, Hauptort: Zürich",
    kuerzel == "BE" ~ "Bern, Hauptort: Bern",
    kuerzel == "SG" ~ "St. Gallen, Hauptort: St. Gallen",
    .default = paste0(kuerzel, ": Information nicht verfügbar")
  )
}

map_chr(kantone, kanton_info)


# ============================================================================
# ÜBUNGEN: Versuche es selbst!
# ============================================================================

# --- Übung 1: Temperaturumrechnung ---
# Schreibe eine Funktion `celsius_zu_fahrenheit(celsius)`.
# Formel: F = C * 9/5 + 32
# Teste: celsius_zu_fahrenheit(0)   → 32
#        celsius_zu_fahrenheit(100)  → 212
#        celsius_zu_fahrenheit(c(-10, 0, 20, 37, 100))



# --- Übung 2: Varianz berechnen ---
# Schreibe eine Funktion `varianz(x)` ohne base R's var() zu verwenden.
# Formel: Var(x) = sum((x - mean(x))^2) / (n - 1)
# Prüfe mit: varianz(c(2,4,4,4,5,5,7,9))
#            var(c(2,4,4,4,5,5,7,9))       ← sollte gleich sein



# --- Übung 3: Mit Fehlerprüfung ---
# Erweitere deine varianz()-Funktion:
# a) stop(), wenn x nicht numerisch ist
# b) stop(), wenn x weniger als 2 Elemente hat
# c) warning(), wenn x NA-Werte enthält, dann NAs entfernen



# --- Übung 4: Funktion in Pipeline ---
# Erstelle einen Datensatz mit 5 Kantonen (Name, Einwohner, Fläche).
# Schreibe eine Funktion groesse_kategorie(einwohner), die zurückgibt:
#   < 50'000     → "sehr klein"
#   < 200'000    → "klein"
#   < 500'000    → "mittel"
#   >= 500'000   → "gross"
# Füge die Kategorie mit mutate() zum Datensatz hinzu.



# --- Übung 5 (Bonus): Mehrere Rückgabewerte ---
# Schreibe eine Funktion `analyse(x)`, die einen numerischen Vektor nimmt
# und eine Liste mit folgenden Elementen zurückgibt:
#   - n: Anzahl Werte (ohne NA)
#   - mittelwert: gerundet auf 2 Stellen
#   - sd: gerundet auf 2 Stellen
#   - ausreisser: Vektor aller Werte ausserhalb (mittelwert ± 2*sd)
#
# Teste mit: analyse(c(10, 12, 11, 13, 9, 45, 10, 11))
# → 45 sollte als Ausreisser erscheinen



bev_dichte <- function(einwohner, flaeche) {
  if (!is.numeric(einwohner) || !is.numeric(flaeche)) {
    stop("'einwohner' und 'flaeche' müssen numerisch sein.")
  }
  if (flaeche <= 0) {
    stop("'flaeche' muss grösser als 0 sein.")
  }
  return(einwohner / flaeche)
}
bev_dichte("viele", 100) 
