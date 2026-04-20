# ════════════════════════════════════════════════════════════════
# PRAXISBEISPIEL: Einwohnerstatistik Kanton Thurgau
# Quelle: data.tg.ch (angelehnt an Datensatz sk-stat-58)
# R-Workshop – Amt für Daten und Statistik, Kanton Thurgau
# ════════════════════════════════════════════════════════════════

# Ausgangslage: Jede der 80 Thurgauer Gemeinden liefert jährlich
# ihre Bevölkerungsdaten als separate CSV-Datei. Deine Aufgabe:
# Daten einlesen, bereinigen, kantonal auswerten und pro Gemeinde
# einen automatisierten Bericht erstellen.

# Schritt 0: Testdaten erstellen ----
# Führe dieses Script zuerst aus, um die Beispieldaten zu generieren:
source("Praxisbeispiel_TG/erstelle_daten.R")

library(tidyverse)
library(ggplot2)
library(openxlsx)


# ════════════════════════════════════════════════════════════════
# Aufgabe 1: Eine Datei kennenlernen ----
# ════════════════════════════════════════════════════════════════
# Lies die Datei für Arbon (BFS 4421) ein und erkunde die Struktur.

datei_arbon <- read_csv("___")   # Pfad zur CSV-Datei ergänzen (z.B. "Praxisbeispiel_TG/daten/Einwohner_4421.csv")
___(datei_arbon)                 # Struktur anzeigen: glimpse() oder str()

# Wie viele Zeilen und Spalten hat die Datei?
dim(datei_arbon)

# Erste Zeilen anzeigen
head(datei_arbon)

# Tipp: Schau dir die Spalte «gemeinde» genau an – siehst du etwas Ungewöhnliches?
# Achte auch darauf, ob alle Werte in «anzahl» vorhanden sind (NA-Werte?).


# ════════════════════════════════════════════════════════════════
# Aufgabe 2: Alle Dateien zusammenführen ----
# ════════════════════════════════════════════════════════════════
# Lies alle 80 CSV-Dateien auf einmal ein und füge sie zu einem
# einzigen Datensatz zusammen.

alle_dateien <- list.files(
  path      = "___",       # Ordner, in dem die CSV-Dateien liegen
  pattern   = "___",       # Muster zum Filtern, z.B. "\\.csv$" oder "Einwohner_.*\\.csv"
  full.names = TRUE
)

# Wie viele Dateien wurden gefunden?
length(alle_dateien)

alle_daten <- map_df(alle_dateien, ___)   # Welche Funktion liest eine CSV-Datei ein?

# Wie viele Zeilen hat der kombinierte Datensatz?
nrow(alle_daten)

# Tipp: map_df() ist praktischer als eine for-Schleife, weil es die Ergebnisse
# aller Aufrufe automatisch per bind_rows() zusammenfügt – kein Aufbau eines
# leeren Objekts nötig und der Code bleibt kurz und lesbar.


# ════════════════════════════════════════════════════════════════
# Aufgabe 3: Daten bereinigen ----
# ════════════════════════════════════════════════════════════════

# 3a) Whitespace in «gemeinde» entfernen
# Einige Gemeinden haben führende oder nachgestellte Leerzeichen im Namen.

alle_daten_clean <- alle_daten |>
  mutate(gemeinde = ___(gemeinde))   # Welche stringr-Funktion entfernt Whitespace?

# Prüfe: Gibt es noch führende Leerzeichen?
alle_daten_clean |>
  filter(str_detect(gemeinde, "^ ")) |>
  nrow()
# Erwartetes Ergebnis: 0

# Tipp: str_trim() entfernt Whitespace am Anfang und Ende, str_squish() zusätzlich
# auch mehrfache Leerzeichen im Inneren des Strings.


# 3b) Fehlende Werte in «anzahl» untersuchen und behandeln
# Bevor wir NAs entfernen: Wo treten sie auf? Gibt es ein Muster?

alle_daten_clean |>
  filter(is.na(anzahl)) |>
  count(___, ___)   # Nach welchen Variablen möchtest du die NAs aufschlüsseln?
                    # z.B. gemeinde, jahr oder bezirk

# Jetzt die Zeilen mit NA entfernen
alle_daten_clean <- alle_daten_clean |>
  filter(___)   # Bedingung: anzahl darf nicht NA sein

# Tipp: filter(!is.na(anzahl)) oder alternativ drop_na(anzahl) – beide Varianten
# sind korrekt. Überlege kurz, ob das Entfernen hier inhaltlich sinnvoll ist
# oder ob Imputation besser wäre.


# 3c) Altersklasse als geordneten Faktor definieren
# Damit spätere Grafiken und Auswertungen die richtige Reihenfolge haben.

altersklassen_reihenfolge <- c(
  "0-4",  "5-9",  "10-14", "15-19", "20-24", "25-29",
  "30-34","35-39","40-44", "45-49", "50-54", "55-59",
  "60-64","65-69","70-74", "75-79", "80-84", "85-89", "90+"
)

alle_daten_clean <- alle_daten_clean |>
  mutate(altersklasse = factor(altersklasse, levels = ___, ordered = TRUE))
#                                                     ^^^ Vektor oben verwenden

# Prüfe: Ist die Reihenfolge korrekt?
levels(alle_daten_clean$altersklasse)

# Tipp: ordered = TRUE ist wichtig für Vergleiche wie altersklasse > "50-54".
# Ohne ordered behandelt R den Faktor als nominal (keine Ranginformation).


# ════════════════════════════════════════════════════════════════
# Aufgabe 4: Kantonsweite Analyse ----
# ════════════════════════════════════════════════════════════════

# 4a) Gesamtbevölkerung pro Jahr (ganzer Kanton)
bev_kanton <- alle_daten_clean |>
  group_by(___) |>                          # Nach welcher Variable gruppieren?
  summarise(einwohner = sum(anzahl))

bev_kanton

# 4b) Bevölkerung nach Bezirk und Jahr
bev_bezirk <- alle_daten_clean |>
  group_by(___, ___) |>                     # Zwei Gruppierungsvariablen
  summarise(einwohner = sum(anzahl), .groups = "drop")

bev_bezirk

# 4c) Top 5 Gemeinden nach Einwohnerzahl im Jahr 2023
top5_gemeinden <- alle_daten_clean |>
  filter(jahr == ___) |>                    # Welches Jahr?
  group_by(gemeinde, bezirk) |>
  summarise(einwohner = ___, .groups = "drop") |>   # Aggregationsfunktion?
  slice_max(___, n = ___)                   # Nach welcher Spalte? Wie viele Zeilen?

top5_gemeinden

# Tipp: slice_max() ist eine tidyverse-Alternative zu arrange() + head().
# Mit .groups = "drop" wird nach summarise() automatisch die Gruppierung aufgehoben,
# damit slice_max() global (über alle Gemeinden) wirkt.


# ════════════════════════════════════════════════════════════════
# Aufgabe 5: Visualisierung ----
# ════════════════════════════════════════════════════════════════

# 5a) Bevölkerungsentwicklung nach Bezirk (Liniendiagramm)
p_entwicklung <- bev_bezirk |>
  ggplot(aes(x = ___, y = ___, color = ___)) +   # x = Jahr, y = Einwohner, color = Bezirk
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title  = "Bevölkerungsentwicklung nach Bezirk",
    x      = "Jahr",
    y      = "Einwohner",
    color  = "Bezirk"
  ) +
  theme_minimal()

p_entwicklung

# Tipp: Ersetze size = 1 in geom_line() durch linewidth = 1 – «size» für Linien
# ist seit ggplot2 3.4 veraltet. Probiere auch theme_light() oder theme_bw() aus.


# 5b) Bevölkerungspyramide für Frauenfeld 2023
# Männlich erscheint links (negative x-Achse), Weiblich rechts (positive x-Achse).

pyramide_daten <- alle_daten_clean |>
  filter(gemeinde == "Frauenfeld", jahr == 2023) |>
  mutate(anzahl_plot = if_else(geschlecht == "Männlich", -anzahl, anzahl))

p_pyramide <- pyramide_daten |>
  ggplot(aes(x = ___, y = altersklasse, fill = ___)) +   # x = anzahl_plot, fill = geschlecht
  geom_col() +
  scale_x_continuous(labels = ___) +   # abs-Funktion für positive Achsenbeschriftung: abs
  scale_fill_manual(values = c("Männlich" = "#4477AA", "Weiblich" = "#EE6677")) +
  labs(
    title = "Bevölkerungspyramide Frauenfeld 2023",
    x     = "Anzahl Personen",
    y     = "Altersklasse",
    fill  = "Geschlecht"
  ) +
  theme_minimal()

p_pyramide

# Tipp: scale_x_continuous(labels = abs) übergibt die Funktion abs() direkt –
# so werden negative Werte auf der Achse als positive Zahlen angezeigt.
# Die Altersklassen erscheinen automatisch in der richtigen Reihenfolge,
# weil wir in Aufgabe 3c einen geordneten Faktor erstellt haben.


# ════════════════════════════════════════════════════════════════
# Aufgabe 6: Funktion + Excel-Export ----
# ════════════════════════════════════════════════════════════════

# Schreibe eine Funktion, die für eine beliebige Gemeinde (nach BFS-Nummer)
# eine Kennzahlen-Tabelle für 2023 zurückgibt.

gemeinde_zusammenfassung <- function(daten, bfs_nummer) {
  daten |>
    filter(bfs_nr == ___, jahr == 2023) |>   # bfs_nummer als Filter verwenden
    summarise(
      einwohner_gesamt      = sum(___),                                          # Gesamtbevölkerung
      anteil_frauen         = sum(anzahl[geschlecht == "Weiblich"]) / sum(___),  # Anteil Frauen (0–1)
      altersklasse_haeufigste = names(which.max(table(___)))                     # Häufigste Altersklasse
    )
}

# Teste die Funktion für Arbon (BFS 4421) und Frauenfeld (BFS 4444)
gemeinde_zusammenfassung(alle_daten_clean, 4421)
gemeinde_zusammenfassung(alle_daten_clean, 4444)

# Tipp: which.max(table(altersklasse)) gibt den Index des häufigsten Eintrags
# zurück; names() holt den zugehörigen Klassennamen. Die Funktion gibt ein
# einzeiliges Tibble zurück – perfekt für map_df() über alle Gemeinden.


# Excel-Arbeitsmappe erstellen und befüllen
wb <- createWorkbook()

# Blatt 1: Kantonsübersicht
addWorksheet(wb, "Kantonsübersicht")
writeData(wb, "Kantonsübersicht", ___)   # Welchen Datensatz möchtest du exportieren?

# Blatt 2: Bezirksübersicht
addWorksheet(wb, "Bezirke")
writeData(wb, "Bezirke", ___)

# Blatt 3: Top-5-Gemeinden
addWorksheet(wb, "Top5 Gemeinden")
writeData(wb, "Top5 Gemeinden", ___)

saveWorkbook(wb, "Praxisbeispiel_TG/einwohnerstatistik_tg.xlsx", overwrite = TRUE)
cat("Excel-Datei gespeichert.\n")

# Tipp: Mit addStyle() und createStyle() kannst du Zellen formatieren
# (Fettschrift, Hintergrundfarbe usw.). openxlsx-Dokumentation: ?createStyle


# ════════════════════════════════════════════════════════════════
# Aufgabe 7: Parametrisierte Berichte mit Quarto (Bonus) ----
# ════════════════════════════════════════════════════════════════
# Für jede Bezirkshauptstadt wird automatisch ein HTML-Bericht gerendert.
# Voraussetzung: Das Template «gemeinde_bericht.qmd» existiert bereits.

# Zuerst: Bereinigten Gesamtdatensatz speichern (wird im Bericht geladen)
write_csv(alle_daten_clean, "Praxisbeispiel_TG/daten/alle_gemeinden.csv")
cat("Bereinigter Datensatz gespeichert.\n")

# Die fünf Bezirkshauptstädte mit ihren BFS-Nummern
bezirkshauptstaedte <- tibble(
  bfs_nr   = c(4421,    4444,         ___,           ___,           ___),
  gemeinde = c("Arbon", "Frauenfeld", "Kreuzlingen", "Münchwilen",  "Weinfelden"),
  bezirk   = c("Arbon", "Frauenfeld", "Kreuzlingen", "Münchwilen",  "Weinfelden")
)
# Tipp: Die fehlenden BFS-Nummern findest du im bereinigten Datensatz:
# alle_daten_clean |> filter(gemeinde %in% c("Kreuzlingen","Münchwilen","Weinfelden")) |>
#   distinct(gemeinde, bfs_nr)

# Bericht für jede Bezirkshauptstadt rendern
walk(seq_len(nrow(bezirkshauptstaedte)), function(i) {
  quarto::quarto_render(
    input       = "Praxisbeispiel_TG/gemeinde_bericht.qmd",
    output_file = paste0("Bericht_", bezirkshauptstaedte$gemeinde[i], ".html"),
    execute_params = list(
      bfs_nr   = ___,   # bfs_nr der i-ten Zeile
      gemeinde = ___,   # Gemeindename der i-ten Zeile
      bezirk   = ___    # Bezirk der i-ten Zeile
    )
  )
  cat("\u2713 Bericht erstellt:", bezirkshauptstaedte$gemeinde[i], "\n")
})

# Tipp: walk() funktioniert wie map(), gibt aber kein Ergebnis zurück –
# ideal für Seiteneffekte wie das Rendern von Dateien oder Schreiben von Logs.
# Möchtest du Berichte für alle 80 Gemeinden? Ersetze bezirkshauptstaedte
# durch alle_daten_clean |> distinct(bfs_nr, gemeinde, bezirk).
