# ══════════════════════════════════════════════════════════════════════════════
# Bevölkerungsstatistik Kanton Thurgau – Musterlösung
# Datenquelle: data.tg.ch, Datensatz sk-stat-67
# ══════════════════════════════════════════════════════════════════════════════
#
# Dieses Skript enthält vollständige Lösungen zu allen Aufgaben.
# Es ist auch als eigenständiges Analyse-Skript nutzbar.
#
# Warum R statt Excel für diese Aufgabe?
# - Die Daten werden direkt von data.tg.ch geladen: kein manueller Download
# - Neue Jahrgänge einlesen = ein Klick auf «Run»
# - Plots und Tabellen sind reproduzierbar und konsistent
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(lubridate)
library(openxlsx)


# ── Daten laden ───────────────────────────────────────────────────────────────

url_bev <- paste0(
  "https://data.tg.ch/api/explore/v2.1/catalog/datasets/",
  "sk-stat-67/exports/csv",
  "?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
)

bev_roh <- read_csv2(url_bev)

glimpse(bev_roh)

# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 1: Daten aufbereiten
# ══════════════════════════════════════════════════════════════════════════════

bev <- bev_roh |>
  mutate(
    # «2023-12-31» → als Datum lesen → nur die Jahreszahl extrahieren
    # year() versteht, dass wir nur die Zahl 2023 wollen
    jahr = year(as.Date(jahr))
  ) |>
  select(
    gemeinde         = gemeinde_name,
    bezirk           = bezirk_name,
    jahr,
    geschlecht       = geschlecht_bezeichnung,
    altersklasse     = alter5klassen_bezeichnung,
    altersklasse_code = alter5klassen_code,
    n                = anzahl_personen
  )

range(bev$jahr)                          # Zeitraum des Datensatzes
bev |> distinct(gemeinde) |> nrow()      # Anzahl Gemeinden
bev |> count(geschlecht)                 # Schreibweise prüfen


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 2: Wie viele Menschen leben im Kanton Thurgau?
# ══════════════════════════════════════════════════════════════════════════════

bev_pro_jahr <- bev |>
  group_by(jahr) |>
  summarise(einwohner = sum(n)) |>
  arrange(jahr)

bev_pro_jahr

# Die Zahlen zeigen: Thurgau wächst. Um wieviel Prozent seit dem ersten Jahr?
erstes_jahr <- min(bev_pro_jahr$jahr)
letztes_jahr <- max(bev_pro_jahr$jahr)

wachstum <- bev_pro_jahr |>
  filter(jahr %in% c(erstes_jahr, letztes_jahr)) |>
  summarise(
    wachstum_pct = round(
      (last(einwohner) / first(einwohner) - 1) * 100, 1
    )
  ) |>
  pull()

cat("Bevölkerungswachstum", erstes_jahr, "–", letztes_jahr, ":", wachstum, "%\n")

# Liniendiagramm
p_entwicklung <- bev_pro_jahr |>
  ggplot(aes(x = jahr, y = einwohner)) +
  geom_line(colour = "#003366", linewidth = 1) +
  geom_point(colour = "#003366", size = 2.5) +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(
    title    = "Bevölkerungsentwicklung Kanton Thurgau",
    subtitle = paste0("Wachstum ", erstes_jahr, "–", letztes_jahr,
                      ": +", wachstum, " %"),
    x        = NULL,
    y        = "Einwohner",
    caption  = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()

p_entwicklung
ggsave("Praxisbeispiel_Bevoelkerung/plot_entwicklung.png",
       p_entwicklung, width = 8, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 3: Welche Gemeinden sind am grössten?
# ══════════════════════════════════════════════════════════════════════════════

top10 <- bev |>
  filter(jahr == letztes_jahr) |>
  group_by(gemeinde) |>
  summarise(einwohner = sum(n), .groups = "drop") |>
  slice_max(einwohner, n = 10)

top10

# Balkendiagramm – horizontal, damit Gemeindenamen gut lesbar sind
p_top10 <- top10 |>
  mutate(gemeinde = reorder(gemeinde, einwohner)) |>
  ggplot(aes(x = einwohner, y = gemeinde)) +
  geom_col(fill = "#003366") +
  geom_text(
    aes(label = scales::label_number(big.mark = "'")(einwohner)),
    hjust = -0.1, size = 3.5, colour = "grey30"
  ) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = "'"),
    expand = expansion(mult = c(0, 0.15))   # Platz für Beschriftung rechts
  ) +
  labs(
    title   = paste("Top 10 Gemeinden nach Einwohnerzahl –", letztes_jahr),
    x       = "Einwohner",
    y       = NULL,
    caption = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()

p_top10
ggsave("Praxisbeispiel_Bevoelkerung/plot_top10.png",
       p_top10, width = 7, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 4: Wie entwickeln sich die Bezirke?
# ══════════════════════════════════════════════════════════════════════════════

bev_bezirk <- bev |>
  group_by(bezirk, jahr) |>
  summarise(einwohner = sum(n), .groups = "drop")

p_bezirk <- bev_bezirk |>
  ggplot(aes(x = jahr, y = einwohner, colour = bezirk, group = bezirk)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(
    title   = "Bevölkerungsentwicklung nach Bezirk",
    x       = NULL,
    y       = "Einwohner",
    colour  = "Bezirk",
    caption = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()

p_bezirk
ggsave("Praxisbeispiel_Bevoelkerung/plot_bezirke.png",
       p_bezirk, width = 9, height = 5)

# Wachstum pro Bezirk in Prozent – pivot_wider macht aus «langen» Daten
# eine Kreuztabelle: jedes Jahr bekommt eine eigene Spalte
bev_bezirk |>
  filter(jahr %in% c(erstes_jahr, letztes_jahr)) |>
  pivot_wider(names_from = jahr, values_from = einwohner) |>
  mutate(
    wachstum_pct = round(
      (.data[[as.character(letztes_jahr)]] /
       .data[[as.character(erstes_jahr)]] - 1) * 100,
      1
    )
  ) |>
  arrange(desc(wachstum_pct))


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 5: Wie alt ist der Kanton Thurgau?
# ══════════════════════════════════════════════════════════════════════════════
# Die Bevölkerungspyramide ist DER Standardplot für Altersstrukturen.
# Männer links (negative Werte), Frauen rechts (positive Werte).

# Reihenfolge der Altersklassen aus dem numerischen Code ableiten
# (der Code 1 = jüngste Klasse, hoher Code = älteste Klasse)
altersklassen_geordnet <- bev |>
  arrange(altersklasse_code) |>
  distinct(altersklasse) |>
  pull()

pyramide <- bev |>
  filter(jahr == letztes_jahr) |>
  group_by(geschlecht, altersklasse) |>
  summarise(n = sum(n), .groups = "drop") |>
  mutate(
    altersklasse = factor(altersklasse,
                          levels  = altersklassen_geordnet,
                          ordered = TRUE),
    # Männer bekommen negative Werte → im Plot erscheinen sie links
    n_plot = if_else(str_detect(geschlecht, "männlich"), -n, n)
  )

p_pyramide <- pyramide |>
  ggplot(aes(x = n_plot, y = altersklasse, fill = geschlecht)) +
  geom_col() +
  # abs() sorgt dafür, dass auf der x-Achse positive Zahlen stehen,
  # obwohl die Männer-Werte intern negativ sind
  scale_x_continuous(
    labels = \(x) scales::label_number(big.mark = "'")(abs(x))
  ) +
  scale_fill_manual(
    values = c("männlich" = "#003366", "weiblich" = "#CC3300")
  ) +
  labs(
    title   = paste("Bevölkerungspyramide Kanton Thurgau", letztes_jahr),
    x       = "Anzahl Personen",
    y       = NULL,
    fill    = NULL,
    caption = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()

p_pyramide
ggsave("Praxisbeispiel_Bevoelkerung/plot_pyramide.png",
       p_pyramide, width = 7, height = 7)


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 6: Eine übersichtliche Tabelle für Excel erstellen
# ══════════════════════════════════════════════════════════════════════════════
# pivot_wider dreht den Datensatz: statt «Bezirk | Jahr | Einwohner» (lang)
# entsteht «Bezirk | 2010 | 2011 | ... | 2023» (breit) – gut für Berichte.

tabelle_bezirk <- bev_bezirk |>
  pivot_wider(names_from  = jahr,
              values_from = einwohner) |>
  arrange(bezirk)

tabelle_bezirk

# Excel-Export mit Formatierung
wb <- createWorkbook()
addWorksheet(wb, "Bevölkerung nach Bezirk")

writeData(wb, "Bevölkerung nach Bezirk", tabelle_bezirk)

# Dunkelblaue Kopfzeile mit weisser Schrift – wirkt professioneller als Rohdaten
header_stil <- createStyle(
  fontColour     = "#FFFFFF",
  fgFill         = "#003366",
  textDecoration = "bold",
  halign         = "left"
)
addStyle(wb, "Bevölkerung nach Bezirk",
         style = header_stil,
         rows  = 1,
         cols  = 1:ncol(tabelle_bezirk))

# Zahlen mit Tausendertrennzeichen formatieren
zahlen_stil <- createStyle(numFmt = "#'##0")
addStyle(wb, "Bevölkerung nach Bezirk",
         style     = zahlen_stil,
         rows      = 2:(nrow(tabelle_bezirk) + 1),
         cols      = 2:ncol(tabelle_bezirk),
         gridExpand = TRUE)

setColWidths(wb, "Bevölkerung nach Bezirk",
             cols   = 1:ncol(tabelle_bezirk),
             widths = "auto")

saveWorkbook(wb, "Praxisbeispiel_Bevoelkerung/bevoelkerung_tg.xlsx",
             overwrite = TRUE)

cat("✓ Excel gespeichert: bevoelkerung_tg.xlsx\n")


# ══════════════════════════════════════════════════════════════════════════════
# Bonus: Anteil Bevölkerung 65+ nach Gemeinde
# ══════════════════════════════════════════════════════════════════════════════
# Code 14 entspricht der Altersklasse «65 – 69 Jahre» – alle Codes ab 14
# sind 65 Jahre und älter. (Prüfen: bev |> distinct(altersklasse_code, altersklasse))

anteil_65plus <- bev |>
  filter(jahr == letztes_jahr) |>
  mutate(gruppe = if_else(altersklasse_code >= 14, "65+", "unter 65")) |>
  group_by(gemeinde, gruppe) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(gemeinde) |>
  mutate(anteil = round(n / sum(n) * 100, 1)) |>
  filter(gruppe == "65+") |>
  arrange(desc(anteil)) |>
  select(gemeinde, anteil_65plus = anteil)

cat("\nTop 10 Gemeinden – höchster Anteil Bevölkerung 65+:\n")
print(head(anteil_65plus, 10))

cat("\nTop 10 Gemeinden – niedrigster Anteil Bevölkerung 65+:\n")
print(tail(anteil_65plus, 10))


# ══════════════════════════════════════════════════════════════════════════════
cat("\n✓ Fertig! Gespeicherte Dateien:\n")
cat("  Praxisbeispiel_Bevoelkerung/plot_entwicklung.png\n")
cat("  Praxisbeispiel_Bevoelkerung/plot_top10.png\n")
cat("  Praxisbeispiel_Bevoelkerung/plot_bezirke.png\n")
cat("  Praxisbeispiel_Bevoelkerung/plot_pyramide.png\n")
cat("  Praxisbeispiel_Bevoelkerung/bevoelkerung_tg.xlsx\n")
