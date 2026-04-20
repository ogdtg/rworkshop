# ══════════════════════════════════════════════════════════════════════════════
# Bevölkerungsstatistik Kanton Thurgau – Aufgaben
# Datenquelle: data.tg.ch, Datensatz sk-stat-67
# ══════════════════════════════════════════════════════════════════════════════
#
# Stell dir vor: Du arbeitest beim Kanton und sollst die Bevölkerungsdaten
# auswerten. Die Daten sind frei verfügbar auf data.tg.ch – du kannst sie
# direkt in R laden, ohne etwas herunterzuladen.
#
# Packages installieren (einmalig, falls nötig):
# install.packages(c("tidyverse", "lubridate", "openxlsx"))
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(lubridate)
library(openxlsx)


# ── Daten laden (dieser Teil ist vorgegeben – einfach ausführen) ──────────────

url_bev <- paste0(
  "https://data.tg.ch/api/explore/v2.1/catalog/datasets/",
  "sk-stat-67/exports/csv",
  "?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
)

bev_roh <- read_csv2(url_bev)

# Kurzer Blick auf die Daten:
glimpse(bev_roh)

# Was haben wir hier?
# - Eine Zeile = eine Altersgruppe, in einer Gemeinde, in einem Jahr, für ein Geschlecht
# - bfs_nr_gemeinde / gemeinde_name: welche Gemeinde?
# - bezirk_name: welcher Bezirk?
# - jahr: das Statistikjahr (steht als Datum, z.B. "2023-12-31")
# - geschlecht_bezeichnung: "männlich" oder "weiblich"
# - alter5klassen_bezeichnung: z.B. "0 - 4 Jahre", "5 - 9 Jahre", ...
# - anzahl_personen: wie viele Menschen in dieser Gruppe

# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 1: Daten aufbereiten ----
# ══════════════════════════════════════════════════════════════════════════════
# Die Spalte «jahr» enthält ein volles Datum (z.B. "2023-12-31"), wir brauchen
# aber nur die Jahreszahl als Zahl. Ausserdem wollen wir nur die Spalten
# behalten, die wir wirklich brauchen.

bev <- bev_roh |>
  mutate(
    jahr = ___(___(___(jahr)))   # Schritt 1: als Datum lesen → year() extrahiert die Jahreszahl
    # Tipp: year() kommt aus lubridate, as.Date() wandelt Text in ein Datum um
  ) |>
  select(
    gemeinde  = ___,   # Gemeindename
    bezirk    = ___,   # Bezirksname
    jahr,
    geschlecht = ___,  # Geschlechtsbezeichnung
    altersklasse = alter5klassen_bezeichnung,
    altersklasse_code = alter5klassen_code,
    n = ___            # Anzahl Personen
  )

# Prüfe: Welche Jahre sind im Datensatz?
___(bev$jahr)

# Prüfe: Wie viele verschiedene Gemeinden?
bev |> ___(gemeinde) |> nrow()

# Tipp: distinct() gibt alle einzigartigen Werte einer Spalte zurück.
#       count() zählt, wie oft jeder Wert vorkommt.


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 2: Wie viele Menschen leben im Kanton Thurgau? ----
# ══════════════════════════════════════════════════════════════════════════════
# Dein Chef fragt: «Wie hat sich die Bevölkerung in den letzten Jahren
# entwickelt?» – Eine Zahl pro Jahr reicht.

bev_pro_jahr <- bev |>
  group_by(___) |>                        # nach welcher Spalte gruppieren?
  summarise(einwohner = sum(___)) |>      # was summieren wir?
  arrange(___)                            # chronologisch sortieren

bev_pro_jahr

# Jetzt als einfaches Liniendiagramm:
bev_pro_jahr |>
  ggplot(aes(x = ___, y = ___)) +
  geom_line(colour = "#003366", linewidth = 1) +
  geom_point(colour = "#003366", size = 2.5) +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(
    title   = "Bevölkerungsentwicklung Kanton Thurgau",
    x       = NULL,
    y       = "Einwohner",
    caption = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()

# Tipp: Vergiss nicht, den Plot zu speichern wenn er dir gefällt:
# ggsave("bev_entwicklung.png", width = 8, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 3: Welche Gemeinden sind am grössten? ----
# ══════════════════════════════════════════════════════════════════════════════
# Eine Kollegin braucht eine Übersicht: die 10 bevölkerungsreichsten Gemeinden
# im aktuellsten verfügbaren Jahr.

letztes_jahr <- max(bev$jahr)   # damit der Code auch nächstes Jahr noch stimmt

top10 <- bev |>
  filter(jahr == ___) |>                             # nur das letzte Jahr
  group_by(___) |>                                   # pro Gemeinde zusammenzählen
  summarise(einwohner = sum(n), .groups = "drop") |>
  slice_max(___, n = ___)                            # die 10 grössten

top10

# Als Balkendiagramm (horizontal, damit die Gemeindenamen lesbar sind):
top10 |>
  mutate(gemeinde = reorder(gemeinde, einwohner)) |>   # absteigend sortieren
  ggplot(aes(x = einwohner, y = gemeinde)) +
  geom_col(fill = "___") +                             # eine Farbe wählen, z.B. "#003366"
  geom_text(
    aes(label = scales::label_number(big.mark = "'")(einwohner)),
    hjust = -0.1, size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = "'"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title   = paste("Top 10 Gemeinden –", letztes_jahr),
    x       = "Einwohner",
    y       = NULL,
    caption = "Quelle: data.tg.ch (sk-stat-67)"
  ) +
  theme_minimal()


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 4: Wie entwickeln sich die Bezirke? ----
# ══════════════════════════════════════════════════════════════════════════════
# Der Kanton hat 5 Bezirke. Wächst überall gleich stark?

bev_bezirk <- bev |>
  group_by(___, ___) |>      # Bezirk UND Jahr
  summarise(einwohner = sum(n), .groups = "drop")

# Liniendiagramm, eine Linie pro Bezirk:
bev_bezirk |>
  ggplot(aes(x = ___, y = ___, colour = ___, group = ___)) +
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

# Bonus: Wieviel Prozent Wachstum hatte jeder Bezirk seit dem ersten Jahr?
erstes_jahr <- min(bev$jahr)

bev_bezirk |>
  filter(jahr %in% c(erstes_jahr, letztes_jahr)) |>
  pivot_wider(names_from = ___, values_from = einwohner) |>   # Jahr als Spalten
  mutate(
    wachstum_pct = round(
      (.data[[as.character(letztes_jahr)]] / .data[[as.character(erstes_jahr)]] - 1) * 100,
      1
    )
  ) |>
  arrange(desc(wachstum_pct))


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 5: Wie alt ist der Kanton Thurgau? ----
# ══════════════════════════════════════════════════════════════════════════════
# Eine Bevölkerungspyramide zeigt die Altersstruktur – Männer links, Frauen
# rechts. Wir bauen sie für das aktuellste Jahr.

# Schritt 1: Daten für die Pyramide vorbereiten
pyramide <- bev |>
  filter(jahr == ___) |>
  group_by(___, ___) |>                # Altersklasse und Geschlecht
  summarise(n = sum(n), .groups = "drop") |>
  mutate(
    # Männer bekommen negative Werte → erscheinen links im Plot
    n_plot = if_else(str_detect(geschlecht, "___"), -n, n)
    #                                  ^^^^ welches Wort erkennt Männer?
  )

# Schritt 2: Altersklassen in die richtige Reihenfolge bringen
# (0-4 unten, 90+ oben) – dazu nutzen wir den numerischen Code aus den Rohdaten
altersklassen_geordnet <- bev |>
  arrange(altersklasse_code) |>
  distinct(altersklasse) |>
  pull()

pyramide <- pyramide |>
  mutate(altersklasse = factor(altersklasse,
                               levels = altersklassen_geordnet,
                               ordered = TRUE))

# Schritt 3: Plot
pyramide |>
  ggplot(aes(x = n_plot, y = altersklasse, fill = ___)) +
  geom_col() +
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


# ══════════════════════════════════════════════════════════════════════════════
# Aufgabe 6: Eine übersichtliche Tabelle für Excel erstellen ----
# ══════════════════════════════════════════════════════════════════════════════
# Du sollst eine Tabelle liefern: Bevölkerung pro Bezirk, jedes Jahr eine
# eigene Spalte – damit man die Entwicklung auf einen Blick sieht.

tabelle_bezirk <- bev_bezirk |>
  pivot_wider(
    names_from  = ___,         # Jahre als Spalten
    values_from = ___          # Einwohnerzahlen als Werte
  ) |>
  arrange(___)                 # alphabetisch nach Bezirk

tabelle_bezirk

# Export nach Excel mit Formatierung
wb <- createWorkbook()
addWorksheet(wb, "Bevölkerung nach Bezirk")

# Daten schreiben
writeData(wb, "Bevölkerung nach Bezirk", tabelle_bezirk)

# Kopfzeile formatieren
header_stil <- createStyle(
  fontColour      = "#FFFFFF",
  fgFill          = "#003366",
  textDecoration  = "bold"
)
addStyle(wb, "Bevölkerung nach Bezirk",
         style = header_stil,
         rows  = 1,
         cols  = 1:ncol(tabelle_bezirk))

# Spaltenbreite anpassen
setColWidths(wb, "Bevölkerung nach Bezirk",
             cols   = 1:ncol(tabelle_bezirk),
             widths = "auto")

# Speichern
saveWorkbook(wb, "Praxisbeispiel_Bevoelkerung/bevoelkerung_tg.xlsx",
             overwrite = TRUE)

cat("✓ Excel gespeichert: bevoelkerung_tg.xlsx\n")


# ══════════════════════════════════════════════════════════════════════════════
# Bonus: Anteil Bevölkerung 65+ nach Gemeinde ----
# ══════════════════════════════════════════════════════════════════════════════
# Welche Gemeinden haben den höchsten Anteil älterer Menschen?
# (Relevant z.B. für Pflegeplanung, Altersheime, ÖV-Angebot)

anteil_65plus <- bev |>
  filter(jahr == letztes_jahr) |>
  mutate(gruppe = if_else(altersklasse_code >= ___, "65+", "unter 65")) |>
  #                                           ^^^  Code für 65-69 Jahre herausfinden
  group_by(gemeinde, gruppe) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(gemeinde) |>
  mutate(anteil = round(n / sum(n) * 100, 1)) |>
  filter(gruppe == "65+") |>
  arrange(desc(anteil)) |>
  select(gemeinde, anteil_65plus = anteil)

# Top 10 Gemeinden mit dem höchsten Anteil älterer Bevölkerung:
head(anteil_65plus, 10)
