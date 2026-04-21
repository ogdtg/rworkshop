# ══════════════════════════════════════════════════════════════════════════════
# Bevölkerungsstatistik Kanton Thurgau – Musterlösung
# Datenquelle: data.tg.ch, Datensatz sk-stat-67
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(lubridate)
library(openxlsx)
library(glue)


# ══════════════════════════════════════════════════════════════════════════════
# Daten laden ----
# ══════════════════════════════════════════════════════════════════════════════

url_alter <- paste0(
  "https://data.tg.ch/api/explore/v2.1/catalog/datasets/",
  "sk-stat-67/exports/csv",
  "?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
)

bev_roh <- read_csv2(url_alter)

bev <- bev_roh |>
  mutate(jahr = year(as.Date(jahr))) |>
  select(
    gemeinde          = gemeinde_name,
    bezirk            = bezirk_name,
    jahr,
    geschlecht        = geschlecht_bezeichnung,
    altersklasse      = alter5klassen_bezeichnung,
    altersklasse_code = alter5klassen_code,
    n                 = anzahl_personen
  )

# Reihenfolge der Altersklassen aus dem numerischen Code ableiten
altersklassen_geordnet <- bev |>
  arrange(altersklasse_code) |>
  distinct(altersklasse) |>
  pull()

bev <- bev |>
  mutate(altersklasse = factor(altersklasse,
                               levels  = altersklassen_geordnet,
                               ordered = TRUE))

# Synthetische Nationalitätsdaten (in der Praxis: zweiter Datensatz von data.tg.ch)
set.seed(2024)
gem_basis <- bev |>
  distinct(gemeinde, bezirk) |>
  mutate(basis = runif(n(), 0.08, 0.32))

erstes_jahr <- min(bev$jahr)
letztes_jahr <- max(bev$jahr)

bev_nat <- bev |>
  group_by(gemeinde, bezirk, jahr) |>
  summarise(total = sum(n), .groups = "drop") |>
  left_join(gem_basis, by = c("gemeinde", "bezirk")) |>
  mutate(
    anteil_ausl = pmin(basis + (jahr - erstes_jahr) * 0.003 +
                         rnorm(n(), 0, 0.006), 0.48),
    auslaender  = round(total * anteil_ausl),
    schweiz     = total - auslaender
  ) |>
  select(gemeinde, bezirk, jahr, auslaender, schweiz) |>
  pivot_longer(cols      = c(schweiz, auslaender),
               names_to  = "nationalitaet",
               values_to = "anzahl") |>
  mutate(nationalitaet = if_else(nationalitaet == "auslaender",
                                 "Ausland", "Schweiz"))


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 1: Überblick verschaffen ----
# ══════════════════════════════════════════════════════════════════════════════

glimpse(bev)
glimpse(bev_nat)

bev |>
  distinct(bezirk, gemeinde) |>
  count(bezirk)

# Welcher Code entspricht 65–69 Jahre?
bev |>
  distinct(altersklasse_code, altersklasse) |>
  arrange(altersklasse_code)
# → Code 14 = «65 – 69 Jahre»

bev |>
  filter(jahr == letztes_jahr) |>
  summarise(total = sum(n))


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 2: Ausländeranteil berechnen ----
# ══════════════════════════════════════════════════════════════════════════════

# --- 2a) Pro Gemeinde --------------------------------------------------------
auslaender_gem <- bev_nat |>
  filter(jahr == letztes_jahr) |>
  group_by(gemeinde, bezirk) |>
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups    = "drop"
  ) |>
  mutate(anteil_pct = round(auslaender / total * 100, 1)) |>
  arrange(desc(anteil_pct))

head(auslaender_gem, 10)

# --- 2b) Pro Bezirk ----------------------------------------------------------
auslaender_bez <- bev_nat |>
  filter(jahr == letztes_jahr) |>
  group_by(bezirk) |>
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups    = "drop"
  ) |>
  mutate(anteil_pct = round(auslaender / total * 100, 1)) |>
  arrange(desc(anteil_pct))

auslaender_bez

# --- 2c) Kanton gesamt -------------------------------------------------------
auslaender_kt <- bev_nat |>
  filter(jahr == letztes_jahr) |>
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl)
  ) |>
  mutate(anteil_pct = round(auslaender / total * 100, 1))

auslaender_kt

# --- 2d) Entwicklung über die Zeit -------------------------------------------
auslaender_zeitreihe <- bev_nat |>
  group_by(jahr) |>
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups    = "drop"
  ) |>
  mutate(anteil_pct = round(auslaender / total * 100, 1))

p_auslaender <- auslaender_zeitreihe |>
  ggplot(aes(x = jahr, y = anteil_pct)) +
  geom_line(linewidth = 1, colour = "#003366") +
  geom_point(size = 2.5, colour = "#003366") +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::label_percent(scale = 1, suffix = " %")) +
  labs(title   = "Ausländeranteil Kanton Thurgau",
       x = NULL, y = "Ausländeranteil",
       caption = "Quelle: data.tg.ch (sk-stat-67)") +
  theme_minimal()

p_auslaender
ggsave("Praxisbeispiel_Bevoelkerung/plot_auslaender_kanton.png",
       p_auslaender, width = 8, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 3: Anteil 65+ ----
# ══════════════════════════════════════════════════════════════════════════════

# --- 3a) Pro Gemeinde (letztes Jahr) -----------------------------------------
anteil_65plus <- bev |>
  filter(jahr == letztes_jahr) |>
  mutate(gruppe = if_else(altersklasse_code >= 14, "65+", "unter 65")) |>
  group_by(gemeinde, bezirk, gruppe) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(gemeinde) |>
  mutate(anteil_pct = round(n / sum(n) * 100, 1)) |>
  filter(gruppe == "65+") |>
  arrange(desc(anteil_pct))

anteil_65plus |> slice_max(anteil_pct, n = 10)

# --- 3b) Entwicklung über die Zeit (Kanton) ----------------------------------
anteil_65plus_kt <- bev |>
  mutate(gruppe = if_else(altersklasse_code >= 14, "65+", "unter 65")) |>
  group_by(jahr, gruppe) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(jahr) |>
  mutate(anteil_pct = round(n / sum(n) * 100, 1)) |>
  filter(gruppe == "65+")

p_65plus <- anteil_65plus_kt |>
  ggplot(aes(x = jahr, y = anteil_pct)) +
  geom_line(linewidth = 1, colour = "#CC3300") +
  geom_point(size = 2.5, colour = "#CC3300") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = " %")) +
  labs(title = "Anteil Bevölkerung 65+ Kanton Thurgau",
       x = NULL, y = "Anteil 65+",
       caption = "Quelle: data.tg.ch (sk-stat-67)") +
  theme_minimal()

p_65plus
ggsave("Praxisbeispiel_Bevoelkerung/plot_65plus_kanton.png",
       p_65plus, width = 8, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 4: Vergleichsdaten vorbereiten ----
# ══════════════════════════════════════════════════════════════════════════════

ausl_gem_alle <- bev_nat |>
  group_by(gemeinde, bezirk, jahr) |>
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups    = "drop"
  ) |>
  mutate(anteil_gem = auslaender / total)

ausl_bez_alle <- bev_nat |>
  group_by(bezirk, jahr) |>
  summarise(
    anteil_bez = sum(anzahl[nationalitaet == "Ausland"]) / sum(anzahl),
    .groups    = "drop"
  )

ausl_kt_alle <- bev_nat |>
  group_by(jahr) |>
  summarise(
    anteil_kt = sum(anzahl[nationalitaet == "Ausland"]) / sum(anzahl),
    .groups   = "drop"
  )

vergleich <- ausl_gem_alle |>
  left_join(ausl_bez_alle, by = c("bezirk", "jahr")) |>
  left_join(ausl_kt_alle,  by = "jahr") |>
  select(gemeinde, bezirk, jahr, anteil_gem, anteil_bez, anteil_kt)

# pivot_longer macht aus drei Anteilsspalten eine Zeile pro Ebene
# → geom_line kann dann color = ebene verwenden
vergleich_long <- vergleich |>
  pivot_longer(
    cols      = c(anteil_gem, anteil_bez, anteil_kt),
    names_to  = "ebene",
    values_to = "anteil"
  ) |>
  mutate(
    ebene = case_when(
      ebene == "anteil_gem" ~ "Gemeinde",
      ebene == "anteil_bez" ~ "Bezirk",
      ebene == "anteil_kt"  ~ "Kanton TG"
    ),
    ebene = factor(ebene, levels = c("Gemeinde", "Bezirk", "Kanton TG"))
  )


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 5: Vergleichs-Plot für jede Gemeinde mit walk() ----
# ══════════════════════════════════════════════════════════════════════════════

dir.create("Praxisbeispiel_Bevoelkerung/plots", showWarnings = FALSE)

erstelle_plot <- function(gem) {

  gem_bezirk <- vergleich |>
    filter(gemeinde == gem) |>
    pull(bezirk) |>
    first()

  vergleich_long |>
    filter(gemeinde == gem) |>
    ggplot(aes(x = jahr, y = anteil,
               colour   = ebene,
               linetype = ebene)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(
      values = c("Gemeinde" = "#003366",
                 "Bezirk"   = "#6699CC",
                 "Kanton TG" = "grey55")
    ) +
    scale_linetype_manual(
      values = c("Gemeinde" = "solid",
                 "Bezirk"   = "dashed",
                 "Kanton TG" = "dotted")
    ) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      title    = glue("Ausländeranteil – {gem}"),
      subtitle = glue("Bezirk: {gem_bezirk}  |  Vergleich mit Bezirk & Kanton TG"),
      x        = NULL,
      y        = "Ausländeranteil",
      colour   = NULL, linetype = NULL,
      caption  = "Quelle: data.tg.ch (sk-stat-67)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# walk() über alle Gemeinden: kein Rückgabewert, nur Plots speichern
walk(unique(vergleich_long$gemeinde), function(gem) {
  p <- erstelle_plot(gem)
  dateiname <- str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_")
  ggsave(
    glue("Praxisbeispiel_Bevoelkerung/plots/{dateiname}.png"),
    p, width = 8, height = 5
  )
  cat("✓ Plot:", gem, "\n")
})


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 6: Excel-Export pro Gemeinde ----
# ══════════════════════════════════════════════════════════════════════════════

dir.create("Praxisbeispiel_Bevoelkerung/excel", showWarnings = FALSE)

# Zahlenformat: 1 Dezimalstelle, kein % (Prozent steht im Spaltennamen)
zahl_stil <- createStyle(numFmt = "0.0")
head_stil <- createStyle(fontColour     = "#FFFFFF",
                         fgFill         = "#003366",
                         textDecoration = "bold",
                         halign         = "left")

erstelle_excel <- function(gem) {

  kennzahlen <- tibble(
    Kennzahl = c("Ausländeranteil (%)", "Anteil 65+ (%)"),
    Wert = c(
      auslaender_gem |> filter(gemeinde == gem) |> pull(anteil_pct),
      anteil_65plus  |> filter(gemeinde == gem) |> pull(anteil_pct)
    )
  )

  # Zeitreihe im Breitformat: eine Spalte pro Jahr
  zeitreihe <- vergleich |>
    filter(gemeinde == gem) |>
    select(jahr, anteil_gem, anteil_bez, anteil_kt) |>
    mutate(across(starts_with("anteil"), \(x) round(x * 100, 1))) |>
    pivot_wider(names_from  = jahr,
                values_from = c(anteil_gem, anteil_bez, anteil_kt))

  wb <- createWorkbook()
  addWorksheet(wb, "Kennzahlen")
  addWorksheet(wb, "Zeitreihe")

  writeData(wb, "Kennzahlen", kennzahlen)
  writeData(wb, "Zeitreihe",  zeitreihe)

  addStyle(wb, "Kennzahlen", head_stil, rows = 1, cols = 1:2)
  addStyle(wb, "Zeitreihe",  head_stil, rows = 1, cols = 1:ncol(zeitreihe))

  # Zahlenzellen formatieren (nur Wert-Spalte in Kennzahlen)
  addStyle(wb, "Kennzahlen", zahl_stil,
           rows = 2:(nrow(kennzahlen) + 1), cols = 2)

  setColWidths(wb, "Kennzahlen", cols = 1:2,           widths = "auto")
  setColWidths(wb, "Zeitreihe",  cols = 1:ncol(zeitreihe), widths = "auto")

  dateiname <- str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_")
  saveWorkbook(wb,
    glue("Praxisbeispiel_Bevoelkerung/excel/{dateiname}.xlsx"),
    overwrite = TRUE)
}

walk(unique(auslaender_gem$gemeinde), function(gem) {
  erstelle_excel(gem)
  cat("✓ Excel:", gem, "\n")
})

cat("\n✓ Fertig!\n")
cat("  Plots:  Praxisbeispiel_Bevoelkerung/plots/  (", 
    n_distinct(bev$gemeinde), "Dateien)\n")
cat("  Excel:  Praxisbeispiel_Bevoelkerung/excel/  (",
    n_distinct(bev$gemeinde), "Dateien)\n")
