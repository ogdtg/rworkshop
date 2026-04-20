# =============================================================================
# Musterlösung: Praxisaufgabe Energiewende im Thurgau
# Quelle: data.tg.ch (div-energie-4, -5, -10, -12)
# R-Workshop – Amt für Daten und Statistik, Kanton Thurgau
# =============================================================================

library(tidyverse)
library(ggrepel)
library(glue)

url_prod       <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-10/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_verbr_gem  <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-5/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_verbr_kant <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-4/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_heiz       <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-12/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"


# =============================================================================
# AUFGABE 1: Daten laden und kennenlernen
# =============================================================================

prod_raw       <- read_csv2(url_prod)
verbr_gem_raw  <- read_csv2(url_verbr_gem)
verbr_kant_raw <- read_csv2(url_verbr_kant)
heiz_raw       <- read_csv2(url_heiz)

# Überblick
map(
  list(prod_raw, verbr_gem_raw, verbr_kant_raw, heiz_raw),
  glimpse
)

# Zeitraum und Gemeinden
prod_raw      |> count(jahr)
verbr_gem_raw |> distinct(gemeinde_name) |> nrow()

# Spaltenstruktur anschauen – alle vier Datensätze liegen im Wide-Format vor
# (eine Spalte pro Energieträger), müssen also noch ins Long-Format gebracht werden
names(prod_raw)
names(verbr_gem_raw)
names(verbr_kant_raw)
names(heiz_raw)


# =============================================================================
# AUFGABE 2: Bereinigung und Transformation ins Tidy-Format
# =============================================================================

# --- 2a) Produktion: Wide → Long ---
# Viele Produktionsspalten sind als <chr> eingelesen (z.B. "0.98"), weil
# manche Zellen leer sind. Wir wandeln sie explizit in numerisch um.
prod <- prod_raw |>
  mutate(across(
    c(wasserkraft, biomasse_holz, biogasanlagen_landwirtschaft,
      abfall_reststoffe, biogasanlagen_industrie, biogasanlagen_abwasser,
      photovoltaik, wind),
    as.numeric
  )) |>
  pivot_longer(
    cols      = c(wasserkraft, biomasse_holz, biogasanlagen_landwirtschaft,
                  abfall_reststoffe, biogasanlagen_industrie,
                  biogasanlagen_abwasser, photovoltaik, wind),
    names_to  = "energietraeger",
    values_to = "produktion_gwh"
  ) |>
  filter(!is.na(produktion_gwh)) |>
  # Leserliche Bezeichnungen
  mutate(energietraeger = recode(energietraeger,
                                 "wasserkraft"                   = "Wasser",
                                 "biomasse_holz"                 = "Biomasse/Holz",
                                 "biogasanlagen_landwirtschaft"  = "Biogas Landwirtschaft",
                                 "abfall_reststoffe"             = "Abfall/Reststoffe",
                                 "biogasanlagen_industrie"       = "Biogas Industrie",
                                 "biogasanlagen_abwasser"        = "Biogas Abwasser",
                                 "photovoltaik"                  = "Photovoltaik",
                                 "wind"                          = "Wind"
  )) |>
  select(jahr, bfs_nr_gemeinde, gemeinde_name, einwohner,
         energietraeger, produktion_gwh)

# --- 2b) Verbrauch Gemeinde: Wide → Long ---
verbr_gem <- verbr_gem_raw |>
  mutate(across(
    c(erdoelbrennstoffe, erdgas, elektrizitaet, holzenergie,
      fernwaerme, umweltwaerme, solarwaerme, andere),
    as.numeric
  )) |>
  pivot_longer(
    cols      = c(erdoelbrennstoffe, erdgas, elektrizitaet, holzenergie,
                  fernwaerme, umweltwaerme, solarwaerme, andere),
    names_to  = "energietraeger",
    values_to = "verbrauch_gwh"
  ) |>
  filter(!is.na(verbrauch_gwh)) |>
  mutate(energietraeger = recode(energietraeger,
                                 "erdoelbrennstoffe" = "Heizöl",
                                 "erdgas"            = "Erdgas",
                                 "elektrizitaet"     = "Elektrizität",
                                 "holzenergie"       = "Holz",
                                 "fernwaerme"        = "Fernwärme",
                                 "umweltwaerme"      = "Umweltwärme/WP",
                                 "solarwaerme"       = "Solarwärme",
                                 "andere"            = "Andere"
  )) |>
  select(jahr, bfs_nr_gemeinde, gemeinde_name, einwohner,
         energiebezugsflaeche, energietraeger, verbrauch_gwh)

# --- 2c) Verbrauch Kanton: Wide → Long ---
verbr_kant <- verbr_kant_raw |>
  mutate(across(
    c(erdoelbrennstoffe, erdgas, elektrizitaet, holzenergie,
      fernwaerme, umweltwaerme, solarwaerme, andere),
    as.numeric
  )) |>
  pivot_longer(
    cols      = c(erdoelbrennstoffe, erdgas, elektrizitaet, holzenergie,
                  fernwaerme, umweltwaerme, solarwaerme, andere),
    names_to  = "energietraeger",
    values_to = "verbrauch_mwh"   # Kantonsebene: Einheit ist MWh (nicht GWh)
  ) |>
  filter(!is.na(verbrauch_mwh)) |>
  mutate(energietraeger = recode(energietraeger,
                                 "erdoelbrennstoffe" = "Heizöl",
                                 "erdgas"            = "Erdgas",
                                 "elektrizitaet"     = "Elektrizität",
                                 "holzenergie"       = "Holz",
                                 "fernwaerme"        = "Fernwärme",
                                 "umweltwaerme"      = "Umweltwärme/WP",
                                 "solarwaerme"       = "Solarwärme",
                                 "andere"            = "Andere"
  )) |>
  select(jahr, energietraeger, verbrauch_mwh)

# --- 2d) Heizsysteme: Wide → Long ---
heiz <- heiz_raw |>
  pivot_longer(
    cols      = c(oelfeuerungen, erdgasfeuerungen, elektroheizungen,
                  holzfeuerungen, waermenetzanschluesse, waermepumpen,
                  andere_erneuerbar, andere_nicht_erneuerbar),
    names_to  = "heizsystem",
    values_to = "anzahl"
  ) |>
  filter(!is.na(anzahl)) |>
  mutate(heizsystem = recode(heizsystem,
                             "oelfeuerungen"            = "Ölfeuerung",
                             "erdgasfeuerungen"         = "Erdgas",
                             "elektroheizungen"         = "Elektroheizung",
                             "holzfeuerungen"           = "Holz",
                             "waermenetzanschluesse"    = "Wärmenetz",
                             "waermepumpen"             = "Wärmepumpe",
                             "andere_erneuerbar"        = "Andere erneuerbar",
                             "andere_nicht_erneuerbar"  = "Andere fossil"
  )) |>
  select(jahr, bfs_nr_gemeinde, gemeinde_name, heizsystem, anzahl)

# Kontrolle
verbr_gem  |> count(energietraeger)
verbr_kant |> count(energietraeger)
prod       |> count(energietraeger)
heiz       |> count(heizsystem)


# =============================================================================
# AUFGABE 3: Entwicklung der erneuerbaren Stromproduktion
# =============================================================================

# --- 3a) Aggregation auf Kantonsebene ---
prod_kanton <- prod |>
  group_by(jahr, energietraeger) |>
  summarise(produktion_gwh = sum(produktion_gwh, na.rm = TRUE), .groups = "drop")

# Farbpalette
farben_prod <- c(
  "Photovoltaik"          = "#F5A623",
  "Wasser"                = "#4A90D9",
  "Biomasse/Holz"         = "#7ED321",
  "Biogas Landwirtschaft" = "#8B5E3C",
  "Biogas Abwasser"       = "#A0784A",
  "Biogas Industrie"      = "#C49A6C",
  "Abfall/Reststoffe"     = "#9B9B9B",
  "Wind"                  = "#BDC3C7"
)

# --- 3b) Liniengrafik ---
prod_kanton |>
  ggplot(aes(x = jahr, y = produktion_gwh, color = energietraeger)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = farben_prod) +
  labs(
    title   = "Erneuerbare Stromproduktion Kanton Thurgau",
    x       = NULL, y = "Produktion (GWh)", color = "Energieträger",
    caption = "Quelle: data.tg.ch (div-energie-10)"
  ) +
  theme_minimal(base_size = 12)

# --- 3c) Gestapeltes Flächendiagramm ---
prod_kanton |>
  ggplot(aes(x = jahr, y = produktion_gwh, fill = energietraeger)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = farben_prod) +
  labs(
    title   = "Zusammensetzung der erneuerbaren Stromproduktion",
    x       = NULL, y = "Produktion (GWh)", fill = "Energieträger",
    caption = "Quelle: data.tg.ch (div-energie-10)"
  ) +
  theme_minimal(base_size = 12)

# --- 3d) Top 5 Gemeinden (letztes verfügbares Jahr) ---
letztes_jahr_prod <- max(prod$jahr)

prod |>
  filter(jahr == letztes_jahr_prod) |>
  group_by(bfs_nr_gemeinde, gemeinde_name) |>
  summarise(produktion_gwh = sum(produktion_gwh, na.rm = TRUE), .groups = "drop") |>
  slice_max(produktion_gwh, n = 5)


# =============================================================================
# AUFGABE 4: Wärmeverbrauch – fossil vs. erneuerbar
# =============================================================================

# --- 4a) Kategorie zuweisen ---
verbr_kant <- verbr_kant |>
  mutate(kategorie = case_when(
    energietraeger %in% c("Heizöl", "Erdgas")                              ~ "Fossil",
    energietraeger %in% c("Holz", "Umweltwärme/WP", "Fernwärme",
                          "Solarwärme")                                    ~ "Erneuerbar",
    .default = "Sonstige"
  ))

verbr_gem <- verbr_gem |>
  mutate(kategorie = case_when(
    energietraeger %in% c("Heizöl", "Erdgas")                              ~ "Fossil",
    energietraeger %in% c("Holz", "Umweltwärme/WP", "Fernwärme",
                          "Solarwärme")                                    ~ "Erneuerbar",
    .default = "Sonstige"
  ))

# Kontrolle
verbr_kant |> count(energietraeger, kategorie)

# --- 4b) Anteil erneuerbarer Wärme pro Jahr (Kanton) ---
anteil_erneuerbar <- verbr_kant |>
  group_by(jahr, kategorie) |>
  summarise(verbrauch_mwh = sum(verbrauch_mwh, na.rm = TRUE), .groups = "drop") |>
  group_by(jahr) |>
  mutate(anteil_pct = verbrauch_mwh / sum(verbrauch_mwh) * 100) |>
  ungroup()

# Aktuellster Wert
anteil_erneuerbar |>
  filter(jahr == max(jahr), kategorie == "Erneuerbar") |>
  pull(anteil_pct) |>
  round(1) |>
  (\(x) cat("Anteil Erneuerbar:", x, "%\n"))()

# --- 4c) Grafik: Anteilsentwicklung ---
farben_kat <- c("Fossil" = "#C0392B", "Erneuerbar" = "#27AE60", "Sonstige" = "#95A5A6")

anteil_erneuerbar |>
  filter(kategorie == "Erneuerbar") |>
  ggplot(aes(x = jahr, y = anteil_pct)) +
  geom_line(linewidth = 1.2, color = "#27AE60") +
  geom_point(size = 3, color = "#27AE60") +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title   = "Anteil erneuerbarer Wärme am Gebäudeverbrauch",
    subtitle = "Kanton Thurgau",
    x       = NULL, y = "Anteil erneuerbar (%)",
    caption = "Quelle: data.tg.ch (div-energie-4)"
  ) +
  theme_minimal(base_size = 12)

# --- 4d) Gemeinde mit höchstem Erneuerbar-Anteil (letztes Jahr) ---
letztes_jahr_verbr <- max(verbr_gem$jahr)

verbr_gem |>
  filter(jahr == letztes_jahr_verbr) |>
  group_by(bfs_nr_gemeinde, gemeinde_name, kategorie) |>
  summarise(verbrauch_gwh = sum(verbrauch_gwh, na.rm = TRUE), .groups = "drop") |>
  group_by(bfs_nr_gemeinde, gemeinde_name) |>
  mutate(anteil_pct = verbrauch_gwh / sum(verbrauch_gwh) * 100) |>
  filter(kategorie == "Erneuerbar") |>
  ungroup() |>
  slice_max(anteil_pct, n = 10)


# =============================================================================
# AUFGABE 5: Eigenversorgungsgrad der Gemeinden
# =============================================================================

# Letztes gemeinsames Jahr
letztes_jahr <- min(max(prod$jahr), max(verbr_gem$jahr))

# --- 5a) Erneuerbare Produktion pro Gemeinde ---
prod_gem <- prod |>
  filter(jahr == letztes_jahr) |>
  group_by(bfs_nr_gemeinde, gemeinde_name) |>
  summarise(produktion_gwh = sum(produktion_gwh, na.rm = TRUE), .groups = "drop")

# --- 5b) Gesamtverbrauch pro Gemeinde ---
verbr_gem_total <- verbr_gem |>
  filter(jahr == letztes_jahr) |>
  group_by(bfs_nr_gemeinde, gemeinde_name) |>
  summarise(verbrauch_gwh = sum(verbrauch_gwh, na.rm = TRUE), .groups = "drop")

# --- 5c) Join & Eigenversorgungsgrad ---
# Einheit Verbrauch: GWh (Gemeindeebene), Einheit Produktion: GWh → passt
eigenversorgung <- prod_gem |>
  left_join(verbr_gem_total, by = c("bfs_nr_gemeinde", "gemeinde_name")) |>
  mutate(eigenversorgungsgrad = produktion_gwh / verbrauch_gwh * 100) |>
  filter(!is.na(eigenversorgungsgrad))

# --- 5d) Top 10 Ranking ---
eigenversorgung |>
  arrange(desc(eigenversorgungsgrad)) |>
  slice_head(n = 10) |>
  select(gemeinde_name, produktion_gwh, verbrauch_gwh, eigenversorgungsgrad) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

# --- 5e) Grösse der Gemeinde: Anzahl Heizsysteme total als Proxy ---
groesse <- heiz |>
  filter(jahr == letztes_jahr) |>
  group_by(bfs_nr_gemeinde, gemeinde_name) |>
  summarise(anzahl_heizsysteme = sum(anzahl, na.rm = TRUE), .groups = "drop")

eigenversorgung_plot <- eigenversorgung |>
  left_join(groesse, by = c("bfs_nr_gemeinde", "gemeinde_name"))

# --- 5f) Streudiagramm ---
eigenversorgung_plot |>
  ggplot(aes(
    x     = verbrauch_gwh,
    y     = produktion_gwh,
    label = gemeinde_name
  )) +
  # Diagonale = 100% Eigenversorgung
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_point(aes(color = eigenversorgungsgrad > 100), size = 2.5, alpha = 0.8) +
  ggrepel::geom_label_repel(
    data        = \(d) filter(
      d,
      eigenversorgungsgrad > 50 | produktion_gwh > quantile(produktion_gwh, 0.85)
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
  labs(
    title    = "Eigenversorgungsgrad Thurgauer Gemeinden",
    subtitle = glue("Erneuerbare Stromproduktion vs. Gebäudeverbrauch ({letztes_jahr})"),
    x        = "Gebäudeverbrauch (GWh)",
    y        = "Erneuerbare Stromproduktion (GWh)",
    caption  = "Strichlinie = 100% Eigenversorgung | Quelle: data.tg.ch"
  ) +
  theme_minimal(base_size = 12)


# =============================================================================
# AUFGABE 6 (BONUS): Parametrisierter Quarto-Bericht
# =============================================================================

# Gemeinden, die in beiden Datensätzen vorhanden sind
gemeinde_liste <- prod |>
  inner_join(
    verbr_gem |> distinct(bfs_nr_gemeinde, gemeinde_name),
    by = c("bfs_nr_gemeinde", "gemeinde_name")
  ) |>
  distinct(bfs_nr_gemeinde, gemeinde_name) |>
  arrange(gemeinde_name)

cat("Berichte für", nrow(gemeinde_liste), "Gemeinden\n")

dir.create("Praxisbeispiel_Energie/berichte", recursive = TRUE, showWarnings = FALSE)

# walk() statt map(), da wir nur Seiteneffekte (Dateien schreiben) wollen
walk(gemeinde_liste$gemeinde_name, function(gem) {
  quarto::quarto_render(
    input          = "Praxisbeispiel_Energie/gemeinde_bericht.qmd",
    output_file    = paste0(
      "Praxisbeispiel_Energie/berichte/Energiebericht_",
      str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_"),
      ".html"
    ),
    execute_params = list(gemeinde = gem)
  )
  cat("✓", gem, "\n")
})