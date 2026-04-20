# =============================================================================
# Musterlösung: Praxisaufgabe Energiewende im Thurgau
# Quelle: data.tg.ch (div-energie-4, -5, -10, -12)
# R-Workshop – Amt für Daten und Statistik, Kanton Thurgau
# =============================================================================
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
# =============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)

url_a <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-10/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_b <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-5/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_c <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-4/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_d <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-12/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"


# === AUFGABE 1: Daten laden und kennenlernen ===

prod_erneuerbar    <- read_csv2(url_a)
verbrauch_gemeinde <- read_csv2(url_b)
verbrauch_kanton   <- read_csv2(url_c)
heizsysteme        <- read_csv2(url_d)

# Überblick über alle Datensätze auf einen Schlag
walk(list(prod_erneuerbar, verbrauch_gemeinde, verbrauch_kanton, heizsysteme), glimpse)

# --- 1a) Zeitraum prüfen ---
prod_erneuerbar    |> count(jahr)
verbrauch_gemeinde |> count(jahr)

# --- 1b) Wie viele Gemeinden sind im Datensatz? ---
verbrauch_gemeinde |> distinct(gemeinde) |> nrow()

# --- 1c) Welche Energieträger kommen vor? ---
prod_erneuerbar    |> distinct(energietraeger)
verbrauch_gemeinde |> distinct(energietraeger)


# === AUFGABE 2: Bereinigung und Kategorisierung ===

# Leerzeichen in Zeichenketten entfernen – verhindert stille Matching-Fehler
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
prod_erneuerbar    <- prod_erneuerbar    |> mutate(across(where(is.character), str_trim))
verbrauch_gemeinde <- verbrauch_gemeinde |> mutate(across(where(is.character), str_trim))
verbrauch_kanton   <- verbrauch_kanton   |> mutate(across(where(is.character), str_trim))
heizsysteme        <- heizsysteme        |> mutate(across(where(is.character), str_trim))

# --- 2a) Kategorie fossil/erneuerbar für Gemeindeebene ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
verbrauch_gemeinde <- verbrauch_gemeinde |>
  mutate(
    kategorie = case_when(
      energietraeger %in% c("Heizöl", "Erdgas")                                   ~ "Fossil",
      energietraeger %in% c("Holz", "Wärmepumpe", "Fernwärme", "Sonne thermisch") ~ "Erneuerbar",
      .default = "Sonstige"
    )
  )

# --- 2b) Kategorie fossil/erneuerbar für Kantonsebene ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
verbrauch_kanton <- verbrauch_kanton |>
  mutate(
    kategorie = case_when(
      energietraeger %in% c("Heizöl", "Erdgas")                                   ~ "Fossil",
      energietraeger %in% c("Holz", "Wärmepumpe", "Fernwärme", "Sonne thermisch") ~ "Erneuerbar",
      .default = "Sonstige"
    )
  )

# Kontrolle: alle Energieträger korrekt zugeordnet?
verbrauch_gemeinde |> count(energietraeger, kategorie)
verbrauch_kanton   |> count(energietraeger, kategorie)


# === AUFGABE 3: Entwicklung erneuerbare Stromproduktion ===

# --- 3a) Aggregation auf Kantonsebene ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
prod_kanton <- prod_erneuerbar |>
  group_by(jahr, energietraeger) |>
  summarise(produktion_mwh = sum(produktion_mwh, na.rm = TRUE), .groups = "drop")

# Konsistente Farbpalette für alle Produktionsgrafiken
farben_traeger <- c(
  "Sonne"           = "#F5A623",
  "Wasser"          = "#4A90D9",
  "Biomasse/Biogas" = "#7ED321",
  "Wind"            = "#9B9B9B"
)

# --- 3b) Liniengrafik: Entwicklung je Energieträger ---
p_linie <- prod_kanton |>
  ggplot(aes(x = jahr, y = produktion_mwh / 1000, color = energietraeger)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = farben_traeger) +
  scale_y_continuous(labels = scales::label_number(suffix = " GWh")) +
  labs(
    title    = "Erneuerbare Stromproduktion Kanton Thurgau",
    subtitle = "Entwicklung nach Energieträger",
    x        = NULL,
    y        = "Produktion (GWh)",
    color    = "Energieträger",
    caption  = "Quelle: data.tg.ch (div-energie-10)"
  ) +
  theme_minimal(base_size = 12)

ggsave("Praxisbeispiel_Energie/plot_produktion_linie.png", p_linie, width = 9, height = 5)

# --- 3c) Gestapeltes Flächendiagramm: Zusammensetzung über Zeit ---
p_flaeche <- prod_kanton |>
  ggplot(aes(x = jahr, y = produktion_mwh / 1000, fill = energietraeger)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = farben_traeger) +
  scale_y_continuous(labels = scales::label_number(suffix = " GWh")) +
  labs(
    title    = "Erneuerbare Stromproduktion Kanton Thurgau",
    subtitle = "Zusammensetzung nach Energieträger",
    x        = NULL,
    y        = "Produktion (GWh)",
    fill     = "Energieträger",
    caption  = "Quelle: data.tg.ch (div-energie-10)"
  ) +
  theme_minimal(base_size = 12)

ggsave("Praxisbeispiel_Energie/plot_produktion_flaeche.png", p_flaeche, width = 9, height = 5)

# --- 3d) Top 5 Gemeinden im neuesten Erhebungsjahr ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
letztes_jahr_prod <- max(prod_erneuerbar$jahr)

top5_gemeinden <- prod_erneuerbar |>
  filter(jahr == letztes_jahr_prod) |>
  group_by(gemeinde_nr, gemeinde) |>
  summarise(produktion_mwh = sum(produktion_mwh, na.rm = TRUE), .groups = "drop") |>
  slice_max(produktion_mwh, n = 5)

print(top5_gemeinden)


# === AUFGABE 4: Wärmeverbrauch – fossil vs. erneuerbar ===

# --- 4a) Verbrauch nach Kategorie und Jahr aggregieren ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
verbrauch_kat <- verbrauch_kanton |>
  group_by(jahr, kategorie) |>
  summarise(verbrauch_mwh = sum(verbrauch_mwh, na.rm = TRUE), .groups = "drop")

# --- 4b) Anteil erneuerbarer Wärme berechnen ---
anteil_erneuerbar <- verbrauch_kat |>
  group_by(jahr) |>
  mutate(anteil_pct = verbrauch_mwh / sum(verbrauch_mwh) * 100) |>
  ungroup()

# Kennzahl für das aktuellste Jahr ausgeben
anteil_erneuerbar |>
  filter(jahr == max(jahr), kategorie == "Erneuerbar") |>
  pull(anteil_pct) |>
  round(1) |>
  cat("Anteil Erneuerbar (aktuellstes Jahr):", ., "%\n")

# Konsistente Farbpalette für Kategorie-Grafiken
farben_kat <- c("Fossil" = "#C0392B", "Erneuerbar" = "#27AE60", "Sonstige" = "#95A5A6")

# --- 4c) Gestapeltes Säulendiagramm: absolute Verbrauchsmengen ---
p_verbrauch <- verbrauch_kat |>
  ggplot(aes(x = jahr, y = verbrauch_mwh / 1000, fill = kategorie)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = farben_kat) +
  scale_y_continuous(labels = scales::label_number(suffix = " GWh")) +
  labs(
    title    = "Endenergieverbrauch Gebäude Kanton Thurgau",
    subtitle = "Nach Kategorie (fossil / erneuerbar / sonstige)",
    x        = NULL,
    y        = "Verbrauch (GWh)",
    fill     = "Kategorie",
    caption  = "Quelle: data.tg.ch (div-energie-4)"
  ) +
  theme_minimal(base_size = 12)

ggsave("Praxisbeispiel_Energie/plot_verbrauch_kat.png", p_verbrauch, width = 9, height = 5)

# --- 4d) Anteilsentwicklung erneuerbar über Zeit ---
p_anteil <- anteil_erneuerbar |>
  filter(kategorie == "Erneuerbar") |>
  ggplot(aes(x = jahr, y = anteil_pct)) +
  geom_line(linewidth = 1.2, color = "#27AE60") +
  geom_point(size = 3, color = "#27AE60") +
  # y-Achse bei 0 verankern, damit Fortschritte nicht übertrieben wirken
  scale_y_continuous(limits = c(0, NA), labels = scales::label_percent(scale = 1)) +
  labs(
    title    = "Anteil erneuerbarer Wärme am Gebäudeverbrauch",
    subtitle = "Kanton Thurgau",
    x        = NULL,
    y        = "Anteil erneuerbar (%)",
    caption  = "Quelle: data.tg.ch (div-energie-4)"
  ) +
  theme_minimal(base_size = 12)

ggsave("Praxisbeispiel_Energie/plot_anteil_erneuerbar.png", p_anteil, width = 9, height = 5)


# === AUFGABE 5: Eigenversorgungsgrad der Gemeinden ===

# Letztes gemeinsames Jahr verwenden, damit beide Datensätze vergleichbar sind
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
letztes_jahr <- min(max(prod_erneuerbar$jahr), max(verbrauch_gemeinde$jahr))

# --- 5a) Erneuerbare Stromproduktion pro Gemeinde ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
prod_gem <- prod_erneuerbar |>
  filter(jahr == letztes_jahr) |>
  group_by(gemeinde_nr, gemeinde) |>
  summarise(produktion_mwh = sum(produktion_mwh, na.rm = TRUE), .groups = "drop")

# --- 5b) Gesamter Gebäudeverbrauch pro Gemeinde ---
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
verbrauch_gem <- verbrauch_gemeinde |>
  filter(jahr == letztes_jahr) |>
  group_by(gemeinde_nr, gemeinde) |>
  summarise(verbrauch_mwh = sum(verbrauch_mwh, na.rm = TRUE), .groups = "drop")

# --- 5c) Join und Eigenversorgungsgrad berechnen ---
# left_join, damit Gemeinden ohne Verbrauchsdaten sichtbar bleiben
eigenversorgung <- prod_gem |>
  left_join(verbrauch_gem, by = c("gemeinde_nr", "gemeinde")) |>
  mutate(eigenversorgungsgrad = produktion_mwh / verbrauch_mwh * 100) |>
  filter(!is.na(eigenversorgungsgrad))

# --- 5d) Top 10 nach Eigenversorgungsgrad ---
eigenversorgung |>
  arrange(desc(eigenversorgungsgrad)) |>
  slice_head(n = 10) |>
  select(gemeinde, produktion_mwh, verbrauch_mwh, eigenversorgungsgrad) |>
  mutate(across(where(is.numeric), round, 1)) |>
  print()

# --- 5e) Streudiagramm mit Beschriftung auffälliger Gemeinden ---
p_scatter <- eigenversorgung |>
  ggplot(aes(
    x     = verbrauch_mwh / 1000,
    y     = produktion_mwh / 1000,
    label = gemeinde
  )) +
  # Diagonale = 100 % Eigenversorgung: Produktion deckt Verbrauch genau
  geom_abline(
    slope     = 1, intercept = 0,
    linetype  = "dashed", color = "grey50", linewidth = 0.8
  ) +
  geom_point(aes(color = eigenversorgungsgrad > 100), size = 2.5, alpha = 0.8) +
  # Nur besonders auffällige Gemeinden beschriften, um Überladung zu vermeiden
  ggrepel::geom_label_repel(
    data        = \(d) filter(
      d,
      eigenversorgungsgrad > 50 | produktion_mwh > quantile(produktion_mwh, 0.85)
    ),
    size         = 3,
    max.overlaps = 15,
    box.padding  = 0.4
  ) +
  scale_color_manual(
    values = c("FALSE" = "#C0392B", "TRUE" = "#27AE60"),
    labels = c("FALSE" = "< 100%",  "TRUE" = "≥ 100%"),
    name   = "Eigenversorgung"
  ) +
  scale_x_continuous(labels = scales::label_number(suffix = " GWh")) +
  scale_y_continuous(labels = scales::label_number(suffix = " GWh")) +
  labs(
    title    = "Eigenversorgungsgrad Thurgauer Gemeinden",
    subtitle = glue::glue(
      "Erneuerbare Stromproduktion vs. Gebäudeverbrauch ({letztes_jahr})"
    ),
    x        = "Gebäudeverbrauch (GWh)",
    y        = "Erneuerbare Stromproduktion (GWh)",
    caption  = "Strichlinie = 100% Eigenversorgung | Quelle: data.tg.ch"
  ) +
  theme_minimal(base_size = 12)

ggsave("Praxisbeispiel_Energie/plot_eigenversorgung.png", p_scatter, width = 10, height = 7)


# === AUFGABE 6 (BONUS): Parametrisierter Quarto-Bericht ===

# Nur Gemeinden einbeziehen, die in beiden Datensätzen vorhanden sind
# Hinweis: Spaltennamen ggf. anpassen je nach effektivem CSV-Export
gemeinde_liste <- prod_erneuerbar |>
  inner_join(
    verbrauch_gemeinde |> distinct(gemeinde_nr, gemeinde),
    by = c("gemeinde_nr", "gemeinde")
  ) |>
  distinct(gemeinde_nr, gemeinde) |>
  arrange(gemeinde)

cat("Berichte werden erstellt für", nrow(gemeinde_liste), "Gemeinden\n")

# Ausgabeordner anlegen (existiert er bereits, keine Fehlermeldung)
dir.create("Praxisbeispiel_Energie/berichte", recursive = TRUE, showWarnings = FALSE)

# walk() statt map(), da wir Seiteneffekte (Dateien schreiben) wollen
# und keinen Rückgabewert benötigen
walk(gemeinde_liste$gemeinde, function(gem) {
  quarto::quarto_render(
    input          = "Praxisbeispiel_Energie/gemeinde_bericht.qmd",
    output_file    = paste0(
      "Praxisbeispiel_Energie/berichte/Energiebericht_",
      str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_"),
      ".html"
    ),
    execute_params = list(gemeinde = gem)
  )
  cat("✓ Bericht erstellt:", gem, "\n")
})
