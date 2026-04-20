# =============================================================================
# Musterlösung: Praxisbeispiel Einwohnerstatistik TG
# =============================================================================
# Quelle: data.tg.ch, Datensatz sk-stat-58
# Dieses Skript enthält vollständige Lösungen zu allen Aufgaben in aufgaben.R.
# =============================================================================

source("Praxisbeispiel_TG/erstelle_daten.R")  # Testdaten generieren
library(tidyverse)
library(ggplot2)
library(openxlsx)


# === AUFGABE 1: Eine Datei kennenlernen =======================================

datei_arbon <- read_csv("Praxisbeispiel_TG/daten/Einwohner_4421.csv")
glimpse(datei_arbon)
# Beobachtung: gemeinde hat führende/nachfolgende Leerzeichen, anzahl enthält NAs


# === AUFGABE 2: Alle 80 Dateien einlesen =====================================

alle_dateien <- list.files(
  path      = "Praxisbeispiel_TG/daten",
  pattern   = "Einwohner_.*\\.csv$",
  full.names = TRUE
)
cat("Anzahl Dateien:", length(alle_dateien), "\n")

# map_df() ist effizienter als ein for-loop: kein manuelles Zwischenspeichern
# und rbind() nötig – purrr übernimmt das Zusammenführen automatisch
alle_daten <- map_df(alle_dateien, read_csv, show_col_types = FALSE)
cat("Zeilen gesamt:", nrow(alle_daten), "\n")


# === AUFGABE 3: Daten bereinigen ==============================================

# --- 3a) Leerzeichen in Gemeindenamen entfernen ---
alle_daten_clean <- alle_daten |>
  mutate(gemeinde = str_trim(gemeinde))

# --- 3b) NAs untersuchen ---
# Gibt es ein systematisches Muster (bestimmte Gemeinden/Jahre besonders betroffen)?
alle_daten |>
  filter(is.na(anzahl)) |>
  count(gemeinde, jahr) |>
  print(n = 20)

# NAs entfernen: Die Fehlwerte sind zufällig verteilt, kein systematisches
# Muster erkennbar → listwise deletion (filter) ist vertretbar
alle_daten_clean <- alle_daten_clean |>
  filter(!is.na(anzahl))

# --- 3c) Altersklasse als geordneten Faktor ---
# Wichtig für korrekte Reihenfolge in Plots und Auswertungen
altersklassen_reihenfolge <- c(
  "0-4",  "5-9",  "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80-84", "85-89", "90+"
)

alle_daten_clean <- alle_daten_clean |>
  mutate(altersklasse = factor(altersklasse,
                                levels  = altersklassen_reihenfolge,
                                ordered = TRUE))


# === AUFGABE 4: Kantonsweite Kennzahlen ======================================

# --- 4a) Gesamtbevölkerung pro Jahr ---
bev_kanton <- alle_daten_clean |>
  group_by(jahr) |>
  summarise(einwohner = sum(anzahl), .groups = "drop")

print(bev_kanton)

# --- 4b) Bevölkerung nach Bezirk und Jahr ---
bev_bezirk <- alle_daten_clean |>
  group_by(bezirk, jahr) |>
  summarise(einwohner = sum(anzahl), .groups = "drop")

# --- 4c) Top 5 Gemeinden nach Einwohnerzahl (2023) ---
top5_gemeinden <- alle_daten_clean |>
  filter(jahr == 2023) |>
  group_by(bfs_nr, gemeinde, bezirk) |>
  summarise(einwohner = sum(anzahl), .groups = "drop") |>
  slice_max(einwohner, n = 5)

print(top5_gemeinden)


# === AUFGABE 5: Visualisierung ================================================

# --- 5a) Bevölkerungsentwicklung nach Bezirk ---
p_entwicklung <- bev_bezirk |>
  ggplot(aes(x = jahr, y = einwohner, color = bezirk, group = bezirk)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  scale_x_continuous(breaks = c(2021, 2022, 2023)) +
  labs(
    title    = "Bevölkerungsentwicklung nach Bezirk, Kanton Thurgau",
    subtitle = "Ständige Wohnbevölkerung, 2021–2023",
    x       = NULL,
    y       = "Einwohner",
    color   = "Bezirk",
    caption = "Quelle: data.tg.ch (Datensatz sk-stat-58)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave("Praxisbeispiel_TG/plot_bev_bezirk.png", p_entwicklung,
       width = 9, height = 5)

# --- 5b) Bevölkerungspyramide Frauenfeld 2023 ---
# Männer werden mit negativen Werten dargestellt, damit die Pyramidenform entsteht
pyramide_daten <- alle_daten_clean |>
  filter(gemeinde == "Frauenfeld", jahr == 2023) |>
  mutate(anzahl_plot = if_else(geschlecht == "Männlich", -anzahl, anzahl))

p_pyramide <- pyramide_daten |>
  ggplot(aes(x = anzahl_plot, y = altersklasse, fill = geschlecht)) +
  geom_col() +
  # abs() auf der x-Achse, damit keine negativen Zahlen erscheinen
  scale_x_continuous(labels = \(x) scales::label_number(big.mark = "'")(abs(x))) +
  scale_fill_manual(values = c("Männlich" = "#003366", "Weiblich" = "#CC3300")) +
  labs(
    title = "Bevölkerungspyramide Frauenfeld 2023",
    x     = "Anzahl Personen",
    y     = "Altersklasse",
    fill  = "Geschlecht"
  ) +
  theme_minimal(base_size = 12)

ggsave("Praxisbeispiel_TG/plot_pyramide_frauenfeld.png", p_pyramide,
       width = 7, height = 7)


# === AUFGABE 6: Custom Function + Excel-Export ================================

# --- 6a) Funktion: Kennzahlen für eine Gemeinde ---
# Gibt einen einzeiligen Tibble mit den wichtigsten Kennzahlen zurück
gemeinde_zusammenfassung <- function(daten, bfs_nummer) {
  daten |>
    filter(bfs_nr == bfs_nummer, jahr == 2023) |>
    summarise(
      einwohner_gesamt        = sum(anzahl),
      anteil_frauen_pct       = round(sum(anzahl[geschlecht == "Weiblich"]) /
                                        sum(anzahl) * 100, 1),
      altersklasse_haeufigste = names(which.max(table(altersklasse)))
    )
}

# Testen für Arbon (4421) und Frauenfeld (4444)
gemeinde_zusammenfassung(alle_daten_clean, 4421)
gemeinde_zusammenfassung(alle_daten_clean, 4444)

# --- 6b) Kennzahlen für alle Gemeinden berechnen ---
# map_df() iteriert über alle BFS-Nummern und bindet Ergebnisse zeilenweise zusammen
alle_bfs <- unique(alle_daten_clean$bfs_nr)
kennzahlen_alle <- map_df(alle_bfs, \(bfs) {
  gem_name <- alle_daten_clean |>
    filter(bfs_nr == bfs) |>
    pull(gemeinde) |>
    first()
  bind_cols(
    tibble(bfs_nr = bfs, gemeinde = gem_name),
    gemeinde_zusammenfassung(alle_daten_clean, bfs)
  )
})

# --- 6c) Excel-Export mit Formatierung ---
wb <- createWorkbook()

# Sheet 1: Kantonsübersicht
addWorksheet(wb, "Kantonsübersicht")
ks_tabelle <- tibble(
  Kennzahl = c(
    "Gemeinden gesamt",
    "Einwohner Kanton (2023)",
    "Bevölkerungswachstum 2021-2023"
  ),
  Wert = c(
    length(unique(alle_daten_clean$bfs_nr)),
    sum(alle_daten_clean$anzahl[alle_daten_clean$jahr == 2023]),
    paste0("+", round(
      (sum(alle_daten_clean$anzahl[alle_daten_clean$jahr == 2023]) /
         sum(alle_daten_clean$anzahl[alle_daten_clean$jahr == 2021]) - 1) * 100,
      1
    ), "%")
  )
)
writeData(wb, "Kantonsübersicht", ks_tabelle)

# Header-Stil: weisse Schrift auf dunkelblauem Hintergrund (TG-Farbe)
header_stil <- createStyle(
  fontColour     = "#FFFFFF",
  fgFill         = "#003366",
  textDecoration = "bold",
  halign         = "left"
)
addStyle(wb, "Kantonsübersicht", header_stil, rows = 1, cols = 1:2)

# Sheet 2: Kennzahlen pro Gemeinde
addWorksheet(wb, "Kennzahlen Gemeinden")
writeData(wb, "Kennzahlen Gemeinden", kennzahlen_alle)
addStyle(wb, "Kennzahlen Gemeinden", header_stil,
         rows = 1, cols = 1:ncol(kennzahlen_alle))
setColWidths(wb, "Kennzahlen Gemeinden",
             cols = 1:ncol(kennzahlen_alle), widths = "auto")

saveWorkbook(wb, "Praxisbeispiel_TG/einwohnerstatistik_tg.xlsx", overwrite = TRUE)
cat("✓ Excel gespeichert: einwohnerstatistik_tg.xlsx\n")


# === AUFGABE 7: Parametrisierte Berichte (Bonus) ==============================

# Bereinigten Gesamtdatensatz für den Bericht speichern, damit gemeinde_bericht.qmd
# nicht nochmals alle Einzeldateien einlesen muss
write_csv(alle_daten_clean, "Praxisbeispiel_TG/daten/alle_gemeinden.csv")

# Bezirkshauptstädte des Kantons Thurgau
bezirkshauptstaedte <- tibble(
  bfs_nr   = c(4421,    4444,         4467,          4487,          4580),
  gemeinde = c("Arbon", "Frauenfeld", "Kreuzlingen", "Münchwilen",  "Weinfelden"),
  bezirk   = c("Arbon", "Frauenfeld", "Kreuzlingen", "Münchwilen",  "Weinfelden")
)

# walk() statt map_df(), weil wir einen Seiteneffekt wollen (Dateien schreiben),
# keinen Rückgabewert – walk() gibt nichts zurück und ist semantisch klarer
walk(1:nrow(bezirkshauptstaedte), function(i) {
  quarto::quarto_render(
    input          = "Praxisbeispiel_TG/gemeinde_bericht.qmd",
    output_file    = paste0("Bericht_", bezirkshauptstaedte$gemeinde[i], ".html"),
    execute_params = list(
      bfs_nr   = bezirkshauptstaedte$bfs_nr[i],
      gemeinde = bezirkshauptstaedte$gemeinde[i],
      bezirk   = bezirkshauptstaedte$bezirk[i]
    )
  )
  cat("✓ Bericht erstellt:", bezirkshauptstaedte$gemeinde[i], "\n")
})

cat("\n✓ Alle Aufgaben abgeschlossen!\n")
cat("  – Plots:     Praxisbeispiel_TG/plot_*.png\n")
cat("  – Excel:     Praxisbeispiel_TG/einwohnerstatistik_tg.xlsx\n")
cat("  – Berichte:  Bericht_*.html\n")
