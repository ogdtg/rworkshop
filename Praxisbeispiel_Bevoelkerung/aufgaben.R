# ══════════════════════════════════════════════════════════════════════════════
# Bevölkerungsstatistik Kanton Thurgau – Aufgaben
# Datenquelle: data.tg.ch, Datensatz sk-stat-67
# Packages: tidyverse, lubridate, openxlsx, glue
# ══════════════════════════════════════════════════════════════════════════════
#
# Ausgangslage:
# Der Kanton Thurgau publiziert Bevölkerungsdaten für alle Gemeinden frei auf
# data.tg.ch. Wir laden den Datensatz direkt in R, analysieren Ausländeranteil
# und Altersstruktur – und erstellen für jede Gemeinde automatisch einen Plot
# und eine Excel-Datei.
#
# Einmalig installieren (falls nötig):
# install.packages(c("tidyverse", "lubridate", "openxlsx", "glue"))
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(lubridate)
library(openxlsx)
library(glue)


# ══════════════════════════════════════════════════════════════════════════════
# VORBEREITUNG: Daten laden (bereits ausgefüllt – einfach ausführen) ----
# ══════════════════════════════════════════════════════════════════════════════

# --- Datensatz 1: Altersstruktur nach Gemeinde (sk-stat-67) ------------------
url_alter <- paste0(
  "https://data.tg.ch/api/explore/v2.1/catalog/datasets/",
  "sk-stat-67/exports/csv",
  "?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
)

bev_roh <- read_csv2(url_alter)

# Jahr als Zahl extrahieren («2023-12-31» → 2023)
bev <- bev_roh |>
  mutate(jahr = year(as.Date(jahr))) |>
  select(
    gemeinde  = gemeinde_name,
    bezirk    = bezirk_name,
    jahr,
    geschlecht = geschlecht_bezeichnung,
    altersklasse = alter5klassen_bezeichnung,
    altersklasse_code = alter5klassen_code,
    n = anzahl_personen
  )

# Altersklassen in die richtige Reihenfolge bringen
altersklassen_geordnet <- bev |>
  arrange(altersklasse_code) |>
  distinct(altersklasse) |>
  pull()

bev <- bev |>
  mutate(altersklasse = factor(altersklasse,
                               levels  = altersklassen_geordnet,
                               ordered = TRUE))

# --- Datensatz 2: Ausländeranteil nach Gemeinde (synthetisch) ----------------
# Hinweis: In der Praxis würde man hier einen zweiten Datensatz von data.tg.ch
# laden, der Nationalität (Schweiz / Ausland) pro Gemeinde enthält.
# Für diese Übung erzeugen wir realistische Beispieldaten.

set.seed(2024)
gem_basis <- bev |>
  distinct(gemeinde, bezirk) |>
  mutate(basis = runif(n(), 0.08, 0.32))

erstes_jahr <- min(bev$jahr)

bev_nat <- bev |>
  group_by(gemeinde, bezirk, jahr) |>
  summarise(total = sum(n), .groups = "drop") |>
  left_join(gem_basis, by = c("gemeinde", "bezirk")) |>
  mutate(
    anteil_ausl = pmin(basis + (jahr - erstes_jahr) * 0.003 +
                         rnorm(n(), 0, 0.006), 0.48),
    auslaender = round(total * anteil_ausl),
    schweiz    = total - auslaender
  ) |>
  select(gemeinde, bezirk, jahr, auslaender, schweiz) |>
  pivot_longer(cols = c(schweiz, auslaender),
               names_to  = "nationalitaet",
               values_to = "anzahl") |>
  mutate(nationalitaet = if_else(nationalitaet == "auslaender",
                                 "Ausland", "Schweiz"))

letztes_jahr <- max(bev$jahr)
cat("Zeitraum:", erstes_jahr, "–", letztes_jahr, "\n")
cat("Gemeinden:", n_distinct(bev$gemeinde), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 1: Überblick verschaffen ----
# ══════════════════════════════════════════════════════════════════════════════
# Verschaffe dir einen ersten Überblick über beide Datensätze.

# Wie sind die Datensätze aufgebaut?
glimpse(___)
glimpse(___)

# Welche Bezirke gibt es? Wie viele Gemeinden pro Bezirk?
bev |>
  ___(bezirk, gemeinde) |>    # wie viele Gemeinden pro Bezirk zählen?
  count(___)

# Welche Altersklassen-Codes gibt es? Welcher Code entspricht 65–69 Jahre?
bev |>
  distinct(___, ___) |>       # alle Code/Bezeichnung-Kombinationen anzeigen
  arrange(altersklasse_code)

# Wie gross ist die Gesamtbevölkerung im letzten verfügbaren Jahr?
bev |>
  filter(jahr == ___) |>
  summarise(total = sum(___))

# Tipp: distinct() gibt einzigartige Werte, count() zählt Vorkommen.


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 2: Ausländeranteil berechnen ----
# ══════════════════════════════════════════════════════════════════════════════

# --- 2a) Ausländeranteil pro Gemeinde (letztes Jahr) -------------------------
auslaender_gem <- bev_nat |>
  filter(jahr == ___) |>
  group_by(___, ___) |>          # Gemeinde und Bezirk
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups = "drop"
  ) |>
  mutate(anteil_pct = round(___ / ___ * 100, 1)) |>
  arrange(desc(anteil_pct))

head(auslaender_gem, 10)         # Top-10 nach Ausländeranteil

# --- 2b) Ausländeranteil pro Bezirk (letztes Jahr) ---------------------------
auslaender_bez <- bev_nat |>
  filter(jahr == ___) |>
  group_by(___) |>               # nur nach Bezirk
  summarise(
    auslaender = sum(anzahl[___ == "Ausland"]),
    total      = sum(___),
    .groups = "drop"
  ) |>
  mutate(anteil_pct = round(auslaender / total * 100, 1)) |>
  arrange(desc(___))

auslaender_bez

# --- 2c) Ausländeranteil Kanton gesamt (letztes Jahr) ------------------------
auslaender_kt <- bev_nat |>
  filter(___) |>
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl)
  ) |>
  mutate(anteil_pct = round(___ / ___ * 100, 1))

auslaender_kt

# --- 2d) Entwicklung des Ausländeranteils über die Zeit (Kanton) -------------
auslaender_zeitreihe <- bev_nat |>
  group_by(___) |>               # nach Jahr aggregieren
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups = "drop"
  ) |>
  mutate(anteil_pct = round(auslaender / total * 100, 1))

# Liniendiagramm: Entwicklung über die Zeit
auslaender_zeitreihe |>
  ggplot(aes(x = ___, y = anteil_pct)) +
  geom_line(linewidth = 1, colour = "#003366") +
  geom_point(size = 2.5, colour = "#003366") +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::label_percent(scale = 1, suffix = " %")) +
  labs(
    title   = "Ausländeranteil Kanton Thurgau",
    x       = NULL,
    y       = "Ausländeranteil",
    caption = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 3: Anteil 65+ ----
# ══════════════════════════════════════════════════════════════════════════════
# Altersklassen-Codes: Code 14 = 65–69 Jahre, alle Codes ≥ 14 sind 65+.
# (Überprüfen mit: bev |> distinct(altersklasse_code, altersklasse) |> arrange(altersklasse_code))

# --- 3a) Anteil 65+ pro Gemeinde (letztes Jahr) ------------------------------
anteil_65plus <- bev |>
  filter(jahr == ___) |>
  mutate(gruppe = if_else(___ >= 14, "65+", "unter 65")) |>
  group_by(___, ___, gruppe) |>     # Gemeinde, Bezirk, Gruppe
  summarise(n = sum(n), .groups = "drop") |>
  group_by(gemeinde) |>
  mutate(anteil_pct = round(n / sum(n) * 100, 1)) |>
  filter(gruppe == "65+") |>
  arrange(desc(___))

# Top-10: Wo ist der Anteil am höchsten?
anteil_65plus |> slice_max(___, n = 10)

# --- 3b) Entwicklung des 65+-Anteils über die Zeit (Kanton) -----------------
anteil_65plus_kt <- bev |>
  mutate(gruppe = if_else(altersklasse_code >= 14, "65+", "unter 65")) |>
  group_by(___, gruppe) |>           # nach Jahr UND Gruppe
  summarise(n = sum(n), .groups = "drop") |>
  group_by(jahr) |>
  mutate(anteil_pct = round(n / sum(n) * 100, 1)) |>
  filter(gruppe == "65+")

anteil_65plus_kt |>
  ggplot(aes(x = ___, y = ___)) +
  geom_line(linewidth = 1, colour = "#CC3300") +
  geom_point(size = 2.5, colour = "#CC3300") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = " %")) +
  labs(title = "Anteil Bevölkerung 65+ Kanton Thurgau",
       x = NULL, y = "Anteil 65+",
       caption = "Quelle: data.tg.ch (sk-stat-67)") +
  theme_minimal()


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 4: Vergleichsdaten vorbereiten ----
# ══════════════════════════════════════════════════════════════════════════════
# Für den Vergleichs-Plot (Aufgabe 5) brauchen wir den Ausländeranteil auf
# drei Ebenen: Gemeinde, Bezirk, Kanton – alle in einem Datensatz.

# Ausländeranteil pro Gemeinde und Jahr (alle Jahre)
ausl_gem_alle <- bev_nat |>
  group_by(___, ___, ___) |>    # Gemeinde, Bezirk, Jahr
  summarise(
    auslaender = sum(anzahl[nationalitaet == "Ausland"]),
    total      = sum(anzahl),
    .groups    = "drop"
  ) |>
  mutate(anteil_gem = auslaender / total)

# Ausländeranteil pro Bezirk und Jahr
ausl_bez_alle <- bev_nat |>
  group_by(___, ___) |>         # Bezirk, Jahr
  summarise(
    anteil_bez = sum(anzahl[nationalitaet == "Ausland"]) / sum(anzahl),
    .groups    = "drop"
  )

# Ausländeranteil Kanton und Jahr
ausl_kt_alle <- bev_nat |>
  group_by(___) |>              # nur Jahr
  summarise(
    anteil_kt = sum(anzahl[nationalitaet == "Ausland"]) / sum(anzahl),
    .groups   = "drop"
  )

# Alle drei Ebenen zusammenführen (zwei Joins)
vergleich <- ausl_gem_alle |>
  left_join(___, by = c("bezirk", "jahr")) |>    # Bezirksdaten dazuhängen
  left_join(___, by = "jahr") |>                 # Kantonsdaten dazuhängen
  select(gemeinde, bezirk, jahr,
         anteil_gem, anteil_bez, anteil_kt)

# Ins Langformat bringen (geom_line braucht eine Zeile pro Punkt)
vergleich_long <- vergleich |>
  pivot_longer(
    cols      = c(anteil_gem, anteil_bez, anteil_kt),
    names_to  = "___",
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

# Tipp: pivot_longer() macht aus Spalten (anteil_gem, anteil_bez, anteil_kt)
# eine einzige Wertspalte – ideal für ggplot.


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 5: Vergleichs-Plot für jede Gemeinde mit walk() ----
# ══════════════════════════════════════════════════════════════════════════════
# Ziel: Für jede Gemeinde ein Liniendiagramm speichern, das zeigt wie sich
# der Ausländeranteil der Gemeinde im Vergleich zu Bezirk und Kanton entwickelt.

dir.create("Praxisbeispiel_Bevoelkerung/plots", showWarnings = FALSE)

alle_gemeinden <- unique(vergleich_long$gemeinde)

# Zuerst: Funktion für einen einzelnen Plot schreiben
erstelle_plot <- function(gem) {

  gem_bezirk <- vergleich |>
    filter(gemeinde == gem) |>
    pull(bezirk) |>
    first()

  vergleich_long |>
    filter(___ == gem) |>       # nach Gemeinde filtern
    ggplot(aes(x = ___, y = anteil,
               colour = ___, linetype = ___)) +    # nach Ebene einfärben
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(
      values = c("Gemeinde" = "___",    # dunkles Blau
                 "Bezirk"   = "___",    # mittleres Blau
                 "Kanton TG" = "___")   # Grau
    ) +
    scale_linetype_manual(
      values = c("Gemeinde" = "solid", "Bezirk" = "dashed",
                 "Kanton TG" = "dotted")
    ) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      title    = glue("Ausländeranteil – ___"),    # Gemeindenamen einsetzen
      subtitle = glue("Bezirk: {gem_bezirk}  |  Vergleich: Bezirk & Kanton TG"),
      x        = NULL,
      y        = "Ausländeranteil",
      colour   = NULL, linetype = NULL,
      caption  = "Quelle: data.tg.ch (sk-stat-67)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

# Dann: walk() über alle Gemeinden
# walk() statt map(), weil wir einen Seiteneffekt (Datei schreiben) wollen,
# kein Rückgabeobjekt brauchen.
walk(___, function(gem) {

  p <- erstelle_plot(___)

  # Dateiname: Sonderzeichen ersetzen (z.B. «Berg TG» → «Berg_TG»)
  dateiname <- str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_")

  ggsave(
    filename = glue("Praxisbeispiel_Bevoelkerung/plots/{___}.png"),
    plot     = ___,
    width    = 8, height = 5
  )
  cat("✓", gem, "\n")
})


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 6: Excel-Export pro Gemeinde mit walk() ----
# ══════════════════════════════════════════════════════════════════════════════
# Für jede Gemeinde: ein Excel mit zwei Tabellenblättern.
# Blatt 1 «Kennzahlen»:  Ausländeranteil und 65+-Anteil, letztes Jahr
# Blatt 2 «Zeitreihe»:   Ausländeranteil pro Jahr (alle Jahre, breit)

dir.create("Praxisbeispiel_Bevoelkerung/excel", showWarnings = FALSE)

# Hilfsfunktion: Excel für eine Gemeinde erstellen
erstelle_excel <- function(gem) {

  # Daten für diese Gemeinde zusammenstellen
  kennzahlen <- tibble(
    Kennzahl = c("Ausländeranteil (%)", "Anteil 65+ (%)"),
    Wert = c(
      auslaender_gem |> filter(gemeinde == gem) |> pull(anteil_pct),
      anteil_65plus  |> filter(gemeinde == gem) |> pull(anteil_pct)
    )
  )

  zeitreihe <- vergleich |>
    filter(gemeinde == gem) |>
    select(jahr, anteil_gem, anteil_bez, anteil_kt) |>
    mutate(across(starts_with("anteil"), \(x) round(x * 100, 1))) |>
    rename("Gemeinde (%)" = anteil_gem,
           "Bezirk (%)"   = anteil_bez,
           "Kanton (%)"   = anteil_kt)

  # Workbook aufbauen
  wb <- ___()                      # neues Workbook erstellen
  addWorksheet(wb, "___")          # Blatt 1: "Kennzahlen"
  addWorksheet(wb, "___")          # Blatt 2: "Zeitreihe"

  writeData(wb, "Kennzahlen", ___)
  writeData(wb, "Zeitreihe",  ___)

  # Kopfzeile formatieren
  stil <- createStyle(fontColour = "#FFFFFF", fgFill = "___",
                      textDecoration = "bold")
  addStyle(wb, "Kennzahlen", stil, rows = 1, cols = 1:2)
  addStyle(wb, "Zeitreihe",  stil, rows = 1, cols = 1:ncol(zeitreihe))

  setColWidths(wb, "Kennzahlen", cols = 1:2,           widths = "auto")
  setColWidths(wb, "Zeitreihe",  cols = 1:ncol(zeitreihe), widths = "auto")

  # Speichern
  dateiname <- str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_")
  saveWorkbook(wb,
    glue("Praxisbeispiel_Bevoelkerung/excel/{___}.xlsx"),
    overwrite = TRUE)
}

# walk() über alle Gemeinden
walk(unique(auslaender_gem$gemeinde), function(gem) {
  erstelle_excel(___)
  cat("✓ Excel:", gem, "\n")
})


# ══════════════════════════════════════════════════════════════════════════════
# Demo: Parametrisierter Bericht ----
# ══════════════════════════════════════════════════════════════════════════════
# Öffne die Datei gemeinde_bericht.Rmd und schau dir den Aufbau an.
# Mit dem folgenden Code wird für jede Gemeinde ein HTML-Bericht erstellt:

# dir.create("Praxisbeispiel_Bevoelkerung/berichte", showWarnings = FALSE)
#
# walk(unique(bev$gemeinde), function(gem) {
#   rmarkdown::render(
#     input       = "Praxisbeispiel_Bevoelkerung/gemeinde_bericht.Rmd",
#     output_file = glue("berichte/{str_replace_all(gem, '[^A-Za-z0-9]', '_')}.html"),
#     output_dir  = "Praxisbeispiel_Bevoelkerung",
#     params      = list(gemeinde = gem),
#     quiet       = TRUE
#   )
#   cat("✓ Bericht:", gem, "\n")
# })
