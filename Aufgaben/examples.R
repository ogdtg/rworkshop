# =============================================================================
# R Workshop – Beispiele: Meldungen, Warnungen & Fehler
# =============================================================================
#
# Dieses Skript enthält alle Beispiele aus den Folien sowie weiterführende
# Beispiele zum selbst Ausprobieren.
#
# Führe die Codeblöcke einzeln aus (Cursor in die Zeile → Ctrl+Enter)
# und beobachte die Ausgabe in der Konsole.
#
# Fehler-Codezeilen sind mit einem führenden # auskommentiert,
# damit das Skript vollständig durchlaufen kann.
# Kommentiere sie einzeln aus, um den Fehler zu sehen.
# =============================================================================


# =============================================================================
# 1. MESSAGES – Reine Informationsmeldungen
# =============================================================================

# 1a) Message beim Laden von Packages
#     dplyr gibt eine Message aus, wenn es Funktionen aus anderen Packages
#     "maskiert" (also überschreibt). Das ist normal und kein Problem.
library(dplyr)

# 1b) Eigene Message mit message() erzeugen
message("Das ist eine Meldung – der Code danach läuft trotzdem weiter.")
cat("Dieser Text erscheint nach der Message.\n")

# 1c) Messages unterdrücken (wenn man sicher weiss, dass sie harmlos sind)
suppressMessages(library(tidyr))

# Fazit: Messages = Informationen. Weitermachen.


# =============================================================================
# 2. WARNINGS – Warnungen (Code lief, aber prüfe das Ergebnis!)
# =============================================================================

# 2a) NAs durch Typumwandlung (coercion)
#     "zwei" kann nicht in eine Zahl umgewandelt werden → wird zu NA
x <- as.numeric(c("1", "zwei", "3"))
x                       # [1]  1 NA  3
is.na(x)                # zeigt, an welcher Position NA entstanden ist

# 2b) Fehlende Werte in Berechnungen
mean(c(1, 2, NA))                    # Ergebnis: NA  – NA "steckt" im Ergebnis
mean(c(1, 2, NA), na.rm = TRUE)      # Ergebnis: 1.5 – mit na.rm = TRUE ignorieren

sum(c(10, NA, 30))                   # Ergebnis: NA
sum(c(10, NA, 30), na.rm = TRUE)     # Ergebnis: 40

# 2c) log(-1) – mathematisch undefiniert → NaN (Not a Number)
log(-1)                              # Warning: NaNs produced, Ergebnis: NaN
sqrt(-4)                             # Warning: NaNs produced

# 2d) Warnungen bei veralteten Funktionen
#     Wenn eine Funktion veraltet ist, gibt R eine Warnung aus.
#     Der Code funktioniert meistens noch, aber man sollte auf die neuere
#     Funktion umsteigen (hier als Kommentar, da das Beispiel tidyr braucht):
# library(tidyr)
# df |> separate(col, c("a","b"))
# → Warning: `separate()` was deprecated in tidyr 1.3.0.

# 2e) Warnungen gezielt unterdrücken
#     Nur machen, wenn man den Grund kennt und das Ergebnis trotzdem korrekt ist!
suppressWarnings(as.numeric(c("1", "zwei", "3")))

# Fazit: Warnungen = Ausführung ok, aber Ergebnis immer auf Plausibilität prüfen!


# =============================================================================
# 3. ERRORS – Fehler (Code wurde NICHT ausgeführt)
# =============================================================================

# 3a) Funktion nicht gefunden – Tippfehler oder Package nicht geladen
# Read_csv("datei.csv")          # Error: could not find function "Read_csv"
# read_csv("datei.csv")          # würde funktionieren (nach library(readr))

# 3b) Objekt nicht gefunden
mein_wert <- 42
# mein_wert2                     # Error: object 'mein_wert2' not found
mein_wert                        # korrekt

# 3c) Syntaxfehler – Klammer nicht geschlossen
# mean(c(1, 2, 3)                # Error: unexpected end of input
mean(c(1, 2, 3))                 # korrekt: alle Klammern geschlossen

# 3d) Falscher Datentyp
# sqrt("zehn")                   # Error: non-numeric argument to mathematical function
sqrt(10)                         # korrekt

# 3e) Package nicht geladen
# read_excel("datei.xlsx")       # Error: could not find function "read_excel"
library(readxl)
# read_excel("datei.xlsx")       # würde jetzt gefunden werden

# Fazit: Bei einem Error wurde kein Ergebnis erzeugt. Erst beheben, dann weiter.


# =============================================================================
# 4. HÄUFIGE ANFÄNGERFEHLER – BEISPIELE UND KORREKTUREN
# =============================================================================

# --- Fehler 1: Gross-/Kleinschreibung ---
meine_zahlen <- c(10, 20, 30)
# Mean(meine_zahlen)             # Error: could not find function "Mean"
mean(meine_zahlen)               # korrekt

# --- Fehler 2: Package nicht geladen ---
# read_excel("datei.xlsx")       # Error: could not find function "read_excel"
library(readxl)                  # Package erst laden!

# --- Fehler 3: Objekt existiert noch nicht ---
# summary(noch_nicht_da)         # Error: object 'noch_nicht_da' not found
noch_nicht_da <- data.frame(x = 1:5, y = letters[1:5])
summary(noch_nicht_da)           # jetzt funktioniert es

# --- Fehler 4: = statt == im Vergleich ---
df <- data.frame(kanton = c("TG", "ZH", "BE"), wert = c(100, 200, 150))
# df |> filter(kanton = "TG")    # Error: unexpected '=' in "filter(kanton ="
df |> filter(kanton == "TG")     # korrekt: doppeltes ==

# --- Fehler 5: Klammeranzahl stimmt nicht ---
# round(sqrt(sum(c(1,2,3)))      # Error: unexpected end of input  (3 auf, 2 zu)
round(sqrt(sum(c(1, 2, 3))))     # korrekt: 3 öffnend = 3 schliessend

# --- Fehler 6: Tippfehler im Variablennamen ---
mein_vektor <- c(5, 10, 15)
# sum(mein_vekter)               # Error: object 'mein_vekter' not found
sum(mein_vektor)                 # korrekt

# --- Fehler 7: Anführungszeichen nicht geschlossen ---
# mein_text <- "Hallo, Welt      # Error: unexpected end of input
mein_text <- "Hallo, Welt"       # korrekt: beide " gesetzt

# --- Fehler 8: Package nicht installiert ---
# library(nichtInstalliert)      # Error: there is no package called '...'
# Lösung: erst installieren (nur einmalig nötig!)
# install.packages("nichtInstalliert")


# =============================================================================
# 5. HÄUFIGE WARNUNGEN – BEDEUTUNG UND UMGANG
# =============================================================================

# 5a) NAs durch Typumwandlung – Werte prüfen!
werte_gemischt <- c("12.5", "8.3", "keine Angabe", "15.1", "n/a")
zahlen         <- as.numeric(werte_gemischt)   # Warning: NAs introduced by coercion
zahlen                                          # NA an Positionen 3 und 5

# Korrekte Summe trotz NAs:
sum(zahlen, na.rm = TRUE)   # 35.9

# 5b) Fehlende Werte bei Berechnungen
vektor_mit_na <- c(1, 2, NA, 4, 5)
max(vektor_mit_na)                     # Ergebnis: NA
max(vektor_mit_na, na.rm = TRUE)       # Ergebnis: 5

# 5c) Vergleich mit NA liefert immer NA (nicht TRUE/FALSE!)
NA == 5        # NA  – nicht FALSE!
is.na(NA)      # TRUE – korrekte Prüfung auf NA

# 5d) Integer-Überlauf (selten, aber möglich)
.Machine$integer.max             # grösster möglicher Integer-Wert
# .Machine$integer.max + 1L      # Warning: NAs produced by integer overflow
.Machine$integer.max + 1         # funktioniert als numeric (kein Integer)


# =============================================================================
# 6. HILFE FINDEN IN R
# =============================================================================

# 6a) Hilfeseite für eine Funktion öffnen
?mean
?sum
?paste0
?data.frame

# 6b) Hilfe suchen, wenn man den genauen Funktionsnamen nicht weiss
??pivot        # sucht in allen Hilfeseiten nach "pivot"
??regression   # sucht nach "regression"

# 6c) Beispiele direkt aus der Hilfeseite ausführen
example(mean)
example(paste)

# 6d) Welche Argumente hat eine Funktion?
args(mean)
args(round)
formals(round)   # detailliertere Ansicht

# 6e) Vignetten – ausführliche Tutorials der Packages
# vignette("dplyr")          # öffnet das dplyr-Tutorial im Help-Pane
# vignette("readr")          # öffnet das readr-Tutorial
# vignette(package = "dplyr") # zeigt alle verfügbaren Vignetten eines Packages


# =============================================================================
# 7. ARBEITSVERZEICHNIS (WORKING DIRECTORY)
# =============================================================================

# 7a) Aktuelles Verzeichnis anzeigen
getwd()

# 7b) Dateien im Verzeichnis auflisten
list.files()                            # alle Dateien im aktuellen Verzeichnis
list.files("rmd/Daten")                 # Dateien in einem Unterordner
list.files(pattern = "\\.csv$")         # nur Dateien mit der Endung .csv
list.files(pattern = "\\.rds$")         # nur .rds-Dateien

# 7c) Unterordner anlegen (falls nötig)
# dir.create("output")   # erstellt den Ordner "output" im Arbeitsverzeichnis

# 7d) Pfade in R (immer / statt \, auch unter Windows!)
# FALSCH (Windows-Backslash): "C:\Users\mein\ordner\datei.csv"
# RICHTIG:                    "C:/Users/mein/ordner/datei.csv"
# ODER:                       r"(C:\Users\mein\ordner\datei.csv)"  # raw string

# 7e) Relative vs. absolute Pfade
# Relativ (empfohlen mit RStudio-Projekten):
#   read_csv("rmd/Daten/bevölkerung.csv")    # sucht ab Projektordner
# Absolut (unflexibel, nicht weitergabefähig):
#   read_csv("C:/Users/user/rworkshop/rmd/Daten/bevölkerung.csv")

# Empfehlung: Immer über das .Rproj-File öffnen → getwd() zeigt den Projektordner


# =============================================================================
# 8. DIE R-UMGEBUNG (ENVIRONMENT)
# =============================================================================

# 8a) Alle Objekte in der aktuellen Session anzeigen
ls()

# 8b) Objekte erstellen und prüfen
a <- 10
b <- "Hallo"
c <- c(TRUE, FALSE, TRUE)
ls()   # a, b, c sind jetzt vorhanden

# 8c) Typ und Struktur eines Objekts prüfen
class(a)       # "numeric"
class(b)       # "character"
class(c)       # "logical"
str(a)
str(c)

# 8d) Einzelne Objekte löschen
rm(a)
ls()   # a ist weg

# 8e) Alle Objekte löschen – Vorsicht!
# rm(list = ls())   # auskommentiert – löscht ALLES in der Session!

# Tipp: In RStudio unter Tools → Global Options → General:
#   - "Restore .RData into workspace at startup" deaktivieren
#   - "Save workspace to .RData on exit" → Never
# → jede Session startet sauber und reproduzierbar


# =============================================================================
# 9. REPRODUZIERBARKEIT
# =============================================================================

# 9a) Session-Informationen anzeigen (Packages, R-Version)
sessionInfo()

# 9b) Zufallszahlen reproduzierbar machen mit set.seed()
set.seed(42)
sample(1:100, 5)   # immer: 91 21 51 50 72 (oder ähnlich)

set.seed(42)
sample(1:100, 5)   # identisches Ergebnis!

# Ohne set.seed: jedes Mal andere Zahlen
sample(1:100, 5)
sample(1:100, 5)

# 9c) Skript von oben nach unten testen
# Shortcut: Ctrl+Shift+F10  → R-Session neu starten (Environment geleert)
# Shortcut: Ctrl+Shift+Enter → gesamtes Skript von oben ausführen


# =============================================================================
# 10. DEBUGGING – STRATEGISCH VORGEHEN
# =============================================================================

# 10a) Pipe schrittweise ausführen – jeden Schritt prüfen
library(dplyr)

df_debug <- data.frame(
  name   = c("Anna", "Ben", "Cara", "Dana"),
  punkte = c(85, 62, 91, 55),
  gruppe = c("A", "B", "A", "B")
)

# Schritt 1 alleine testen:
df_debug |> filter(gruppe == "A")

# Schritt 2 hinzufügen:
df_debug |>
  filter(gruppe == "A") |>
  mutate(bestanden = punkte >= 70)

# Schritt 3 hinzufügen:
df_debug |>
  filter(gruppe == "A") |>
  mutate(bestanden = punkte >= 70) |>
  select(name, bestanden)

# 10b) Zwischenergebnis in einer Pipe ausgeben (ohne Pipeline zu unterbrechen)
df_debug |>
  filter(gruppe == "A") |>
  (\(x) { cat("Nach filter():", nrow(x), "Zeilen\n"); x })() |>
  mutate(bestanden = punkte >= 70)

# 10c) Mit kleinem Datensatz testen, dann skalieren
df_gross <- data.frame(
  x = rnorm(100000),
  y = sample(c("A","B"), 100000, replace = TRUE)
)

# Erst mit head() testen:
df_gross |> head(5) |> filter(y == "A")
# Dann auf den ganzen Datensatz anwenden:
# df_gross |> filter(y == "A")


# =============================================================================
# 11. KOMMENTARE IM CODE
# =============================================================================

# 11a) Einzeiliger Kommentar
x <- 5   # Anzahl der Versuche

# 11b) Mehrzeilige Erklärung
# Ziel: Durchschnitt aller positiven Werte berechnen.
# Vorgehen:
#   1. NA-Werte und negative Zahlen entfernen
#   2. Mittelwert über die verbleibenden Werte bilden
messung <- c(-3, NA, 5, 8, -1, 12, NA, 7)
positiv <- messung[!is.na(messung) & messung > 0]
mean(positiv)   # 8

# 11c) Abschnittstrennlinie (erscheint im Document Outline: Ctrl+Shift+O)
# ---------- Daten einlesen ------------------------------------------------

# ---------- Daten bereinigen ----------------------------------------------

# ---------- Auswertung ----------------------------------------------------

# 11d) TODO-Kommentare für offene Punkte
# TODO: Encoding prüfen, falls Umlaute fehlen
# TODO: Datumsformat anpassen (aktuell: YYYY-MM-DD, Ziel: DD.MM.YYYY)


# =============================================================================
# 12. WEITERFÜHRENDE BEISPIELE ZUM AUSPROBIEREN
# =============================================================================

# 12a) Typ-Überraschungen – was gibt class() zurück?
class(1L)          # "integer"  – ganzzahliges L-Literal
class(1.0)         # "numeric"
class(TRUE)        # "logical"
class("abc")       # "character"
class(NULL)        # "NULL"
class(NA)          # "logical"  – überraschend!
class(NA_real_)    # "numeric"  – typisiertes NA

# 12b) Vektorisierung – R rechnet komponentenweise
a <- c(1, 2, 3)
b <- c(10, 20, 30)
a + b              # [1] 11 22 33 – kein Loop nötig!
a * 2              # [1]  2  4  6

# 12c) Recycling – kürzere Vektoren werden wiederholt (Vorsicht!)
c(1, 2, 3, 4) + c(10, 20)   # [1] 11 22 13 24 – 10, 20 wird recycelt

# 12d) %in% – übersichtlicher als viele |-Vergleiche
kantone <- c("TG", "ZH", "BE", "BS", "GE")
kantone %in% c("TG", "ZH", "BS")   # [1]  TRUE  TRUE FALSE  TRUE FALSE
# äquivalent zu, aber kürzer als:
kantone == "TG" | kantone == "ZH" | kantone == "BS"

# 12e) Gross-/Kleinschreibung beachten – häufige Fehlerquelle!
mein_objekt <- 100
# mein_Objekt   # Error: object 'mein_Objekt' not found
# MeinObjekt    # Error: object 'MeinObjekt' not found
mein_objekt     # korrekt

# 12f) <- vs = vs ==
a   <- 5         # Zuweisung: a bekommt den Wert 5
b   <- a == 5    # Vergleich: b ist TRUE (5 ist gleich 5)
b                # TRUE
# Innerhalb von Funktionsargumenten: = für Parameter-Zuweisung
round(3.14159, digits = 2)   # digits = 2 (nicht <- 2)

# 12g) NA-Prüfung – funktioniert nur mit is.na()
x <- NA
x == NA      # NA  (nicht TRUE – Vergleich mit NA ergibt immer NA)
is.na(x)     # TRUE  (korrekte Methode)

# 12h) Pipe-Operator: |> vs %>%
#     Beide leiten das Ergebnis links als erstes Argument rechts weiter.
#     |> ist eingebaut (ab R 4.1), %>% kommt aus dem magrittr-Package.
library(magrittr)
c(3, 1, 4, 1, 5) |>  sort() |>  rev()     # nativer Pipe
c(3, 1, 4, 1, 5) %>% sort() %>% rev()     # magrittr Pipe – identisches Ergebnis
