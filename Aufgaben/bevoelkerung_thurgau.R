# =============================================================================
# Bevölkerungsentwicklung und Sozialhilfe im Kanton Thurgau
# Ein zusammenhängendes Beispiel: Daten einlesen · tidyr · dplyr
# =============================================================================
#
# KONTEXT
# -------
# Die Fachstelle für Statistik des Kantons Thurgau analysiert regelmässig
# Bevölkerungsdaten auf Gemeindeebene. Dieses Skript zeigt, wie solche Analysen
# mit R umgesetzt werden – von der Datenaufbereitung bis zur Beantwortung
# konkreter Fragestellungen.
#
# FRAGESTELLUNGEN
# ---------------
#   1. Welche Gemeinden verzeichneten zwischen 2015 und 2022 das stärkste
#      Bevölkerungswachstum (absolut und prozentual)?
#   2. Wie entwickelte sich die Bevölkerung in den fünf Bezirken?
#   3. Gibt es einen Zusammenhang zwischen Gemeindegrösse und
#      Sozialhilfequote?
#   4. Welche Gemeinden haben gleichzeitig starkes Wachstum und eine
#      überdurchschnittliche Sozialhilfequote?
#
# DATENQUELLEN (Fachstelle für Statistik, Kanton Thurgau)
# --------------------------------------------------------
#   - bevölkerung.csv: Ständige Wohnbevölkerung nach Gemeinde, 2015–2022
#   - 1_Sozialhilfequote_Bezüger_Dossiers_Gde_2023.csv: Sozialhilfedaten 2023
#
# VERWENDETE PAKETE
# -----------------
#   tidyverse (enthält: readr, tidyr, dplyr, ggplot2, u.a.)
#
# HINWEIS: Führe die Codeblöcke einzeln aus (Cursor in die Zeile → Ctrl+Enter)
# =============================================================================


# ── 0. Pakete laden ──────────────────────────────────────────────────────────

library(tidyverse)  # Lädt readr, tidyr, dplyr, ggplot2 und weitere auf einmal


# =============================================================================
# SCHRITT 1: DATEN EINLESEN
# =============================================================================
# Ziel: Die Rohdaten aus verschiedenen Quellen in R laden.
#       Wir verwenden zwei Dateiformate: CSV (komma-getrennt) und
#       CSV (semikolon-getrennt) – ein im Kanton Thurgau häufiges Vorkommen.


# ── 1a) Bevölkerungsdaten einlesen ───────────────────────────────────────────
# Format: CSV, kommagetrennt, Wide-Format (eine Spalte pro Jahr)
# Die Datei stammt direkt aus dem Statistiksystem – typisches Exportformat

bev_roh <- read_csv(
  "rmd/Daten/bevölkerung.csv",
  locale = locale(encoding = "UTF-8")  # Falls Umlaute falsch erscheinen: "latin1"
)

# Erste Inspektion
glimpse(bev_roh)
# → 87 Zeilen, 10 Spalten
# → Die Datei enthält KANTON, BEZIRKE und GEMEINDEN gemischt
# → Spalten: "BFS-Nr.", "Gemeinde", "2015", "2016", ..., "2022"

# Die ersten Zeilen zeigen die Struktur:
head(bev_roh, 5)


# ── 1b) Sozialhilfedaten einlesen ────────────────────────────────────────────
# Format: CSV, SEMIKOLON-getrennt (häufig bei Exporten aus STATISTIK/SAP)
# → Wichtig: delim = ";" muss explizit angegeben werden!

sozial_roh <- read_delim(
  "rmd/Daten/1_Sozialhilfequote_Bezüger_Dossiers_Gde_2023.csv",
  delim   = ";",
  locale  = locale(encoding = "UTF-8")  # Sicherstellen: Umlaute in Gemeindenamen
)

glimpse(sozial_roh)
# → 80 Zeilen, 7 Spalten
# → Eine Zeile pro Gemeinde mit Sozialhilfequote, Anzahl Dossiers, Personen


# ── 1c) Gemeinde-Bezirk-Zuordnung erstellen ──────────────────────────────────
# Die Bevölkerungsdaten haben keine Bezirksspalte – wir brauchen eine
# Referenztabelle (könnte auch aus einer Datenbank stammen).
# Kanton Thurgau hat seit der Bezirksreform 2011 genau 5 Bezirke.

gemeinde_bezirk <- tibble(
  bfs_nr = c(
    # ── Bezirk Arbon (12 Gemeinden) ──────────────────────────────────────────
    4461, 4401, 4406, 4411, 4416, 4421, 4426, 4431, 4436, 4441, 4446, 4451,
    # ── Bezirk Frauenfeld (23 Gemeinden) ─────────────────────────────────────
    4536, 4801, 4545, 4806, 4561, 4566, 4571, 4811, 4816, 4590, 4821, 4826,
    4591, 4831, 4601, 4841, 4546, 4864, 4606, 4611, 4616, 4871, 4621,
    # ── Bezirk Kreuzlingen (14 Gemeinden) ────────────────────────────────────
    4641, 4643, 4646, 4651, 4656, 4666, 4671, 4681, 4683, 4691, 4846, 4851,
    4696, 4701,
    # ── Bezirk Münchwilen (13 Gemeinden) ─────────────────────────────────────
    4551, 4716, 4721, 4723, 4724, 4726, 4741, 4746, 4751, 4761, 4776, 4781,
    4786,
    # ── Bezirk Weinfelden (18 Gemeinden) ─────────────────────────────────────
    4711, 4881, 4891, 4901, 4471, 4911, 4921, 4476, 4486, 4495, 4501, 4941,
    4756, 4506, 4946, 4951, 4791, 4511
  ),
  bezirk = c(
    rep("Arbon",       12),
    rep("Frauenfeld",  23),
    rep("Kreuzlingen", 14),
    rep("Münchwilen",  13),
    rep("Weinfelden",  18)
  )
)

# Kontrolle: Stimmt die Anzahl pro Bezirk?
count(gemeinde_bezirk, bezirk)
# → Arbon: 12, Frauenfeld: 23, Kreuzlingen: 14, Münchwilen: 13, Weinfelden: 18


# =============================================================================
# SCHRITT 2: DATEN BEREINIGEN UND STRUKTURIEREN (tidyr)
# =============================================================================
# Ziel: Die Rohdaten in ein sauberes, analysefähiges Format bringen.
#       Kernaufgabe: Das Wide-Format der Bevölkerungsdaten in ein Long-Format
#       umwandeln – die Grundlage für Zeitreihenanalysen.


# ── 2a) Spaltennamen vereinfachen ────────────────────────────────────────────
# "BFS-Nr." enthält einen Punkt und einen Bindestrich → umständlich
# Mit rename() geben wir handlichere Namen

bev_roh <- bev_roh |>
  rename(
    bfs_nr   = `BFS-Nr.`,
    gemeinde = Gemeinde
  )


# ── 2b) Aggregatzeilen herausfiltern ─────────────────────────────────────────
# Die Datei enthält Zeilen für:
#   - Kanton Thurgau (BFS-Nr. = 20)
#   - Bezirke         (BFS-Nr. = 2011–2015)
#   - Gemeinden       (BFS-Nr. ≥ 4000)
# Für die Gemeindeanalyse behalten wir nur die Gemeindezeilen.

gemeinden_wide <- bev_roh |>
  filter(bfs_nr >= 4000)

nrow(gemeinden_wide)  # → 80 (eine Zeile pro Gemeinde, 8 Jahresspalten)

# Bezirkszeilen separat behalten (für Vergleiche später)
bezirke_wide <- bev_roh |>
  filter(bfs_nr > 20, bfs_nr < 4000)


# ── 2c) Wide → Long: pivot_longer() ─────────────────────────────────────────
# PROBLEM: Je eine Spalte pro Jahr ("2015", "2016", ..., "2022")
# LÖSUNG:  pivot_longer() klappt alle Jahresspalten zu zwei Spalten:
#          "jahr" (Jahreszahl) und "bevoelkerung" (Bevölkerungswert)
#
# Vorher:  80 Zeilen × 10 Spalten
# Nachher: 640 Zeilen × 4 Spalten  (80 Gemeinden × 8 Jahre)

gemeinden_long <- gemeinden_wide |>
  pivot_longer(
    cols      = `2015`:`2022`,   # Diese Spalten werden "umgeklappt"
    names_to  = "jahr",          # Bisherige Spaltennamen → neue Spalte "jahr"
    values_to = "bevoelkerung"   # Bisherige Werte → neue Spalte "bevoelkerung"
  ) |>
  mutate(jahr = as.integer(jahr))  # Jahr als Zahl speichern (nicht als Text)

# Ergebnis vergleichen
glimpse(gemeinden_wide)   # Wide: 80 Zeilen, 10 Spalten
glimpse(gemeinden_long)   # Long: 640 Zeilen, 4 Spalten


# ── 2d) Sozialhilfedaten bereinigen ──────────────────────────────────────────
# Nur die relevanten Spalten behalten und sinnvoll benennen

sozial <- sozial_roh |>
  select(
    gemeinde    = GdenameTG,
    sh_dossiers = anzahl_doss,
    sh_personen = anzahl_pers,
    sh_quote    = quote,
    sh_kategorie = Quote_Kategorie
  ) |>
  # Kommas in sh_quote durch Punkte ersetzen (CH-Format → R-Format)
  mutate(sh_quote = as.numeric(str_replace(as.character(sh_quote), ",", ".")))

glimpse(sozial)


# =============================================================================
# SCHRITT 3: DATEN ANALYSIEREN (dplyr)
# =============================================================================


# ── FRAGESTELLUNG 1: Welche Gemeinden sind am stärksten gewachsen? ───────────

# Plan:
# a) Nur die Jahre 2015 und 2022 behalten (Anfang und Ende des Beobachtungszeitraums)
# b) pivot_wider(): Aus "lang" wieder "breit" – zwei Spalten bev_2015, bev_2022
# c) Wachstum absolut und prozentual berechnen
# d) Gemeinden in Grössenklassen einteilen

wachstum <- gemeinden_long |>
  # a) Endpunkte herausfiltern
  filter(jahr %in% c(2015, 2022)) |>

  # b) Von Long wieder nach Wide (jetzt eine Spalte pro Jahr)
  pivot_wider(
    names_from  = jahr,
    names_prefix = "bev_",       # → Spalten heissen "bev_2015", "bev_2022"
    values_from = bevoelkerung
  ) |>

  # c) Wachstumskennzahlen berechnen
  mutate(
    zuwachs_abs = bev_2022 - bev_2015,
    zuwachs_pct = round((bev_2022 - bev_2015) / bev_2015 * 100, 1)
  ) |>

  # d) Gemeinden nach Einwohnerzahl 2015 kategorisieren
  mutate(
    groessenklasse = case_when(
      bev_2015 < 1000  ~ "Klein (< 1'000 Einw.)",
      bev_2015 < 5000  ~ "Mittel (1'000–4'999 Einw.)",
      bev_2015 < 15000 ~ "Gross (5'000–14'999 Einw.)",
      TRUE             ~ "Zentrum (≥ 15'000 Einw.)"
    )
  )

# Ergebnis: Top 10 Gemeinden mit stärkstem prozentualen Wachstum
cat("\n── Top 10: Stärkstes Bevölkerungswachstum 2015–2022 (%)\n")
wachstum |>
  arrange(desc(zuwachs_pct)) |>
  select(Gemeinde = gemeinde, `Bev. 2015` = bev_2015, `Bev. 2022` = bev_2022,
         `Zuwachs abs.` = zuwachs_abs, `Zuwachs %` = zuwachs_pct) |>
  slice_head(n = 10) |>
  print()

# Und welche Gemeinden haben Bevölkerung verloren?
cat("\n── Gemeinden mit Bevölkerungsrückgang 2015–2022\n")
wachstum |>
  filter(zuwachs_pct < 0) |>
  arrange(zuwachs_pct) |>
  select(Gemeinde = gemeinde, `Bev. 2015` = bev_2015, `Bev. 2022` = bev_2022,
         `Zuwachs %` = zuwachs_pct) |>
  print()


# ── FRAGESTELLUNG 2: Bevölkerungsentwicklung nach Bezirk ─────────────────────

# Plan:
# a) Bezirksinformation zu den Gemeindedaten hinzufügen (left_join)
# b) Bevölkerung nach Bezirk und Jahr aufsummieren (group_by + summarise)
# c) Wachstum 2015 → 2022 pro Bezirk berechnen

# a) Bezirk hinzufügen
gemeinden_long_bz <- gemeinden_long |>
  left_join(gemeinde_bezirk, by = "bfs_nr")

# Kontrolle: Haben alle Gemeinden einen Bezirk?
gemeinden_long_bz |>
  filter(is.na(bezirk)) |>
  distinct(gemeinde)
# → Leere Tabelle = Alle Gemeinden haben einen Bezirk ✓

# b) Bevölkerungssumme pro Bezirk und Jahr
bezirk_entwicklung <- gemeinden_long_bz |>
  group_by(bezirk, jahr) |>
  summarise(bevoelkerung = sum(bevoelkerung), .groups = "drop")

bezirk_entwicklung  # 25 Zeilen: 5 Bezirke × 5 Jahre (2015–2022, nicht alle)

# Überblick: Wachstum 2015 → 2022 pro Bezirk
cat("\n── Bevölkerungsentwicklung nach Bezirk 2015–2022\n")
bezirk_entwicklung |>
  filter(jahr %in% c(2015, 2022)) |>
  pivot_wider(names_from = jahr, names_prefix = "bev_", values_from = bevoelkerung) |>
  mutate(
    zuwachs_abs = bev_2022 - bev_2015,
    zuwachs_pct = round((bev_2022 - bev_2015) / bev_2015 * 100, 1)
  ) |>
  arrange(desc(zuwachs_pct)) |>
  print()

# Anzahl Gemeinden mit Wachstum vs. Rückgang – aufgeteilt nach Bezirk
cat("\n── Anzahl wachsende vs. schrumpfende Gemeinden pro Bezirk\n")
wachstum |>
  left_join(gemeinde_bezirk, by = "bfs_nr") |>
  mutate(trend = if_else(zuwachs_pct >= 0, "Wachstum", "Rückgang")) |>
  count(bezirk, trend) |>
  pivot_wider(names_from = trend, values_from = n, values_fill = 0) |>
  arrange(desc(Wachstum)) |>
  print()


# ── FRAGESTELLUNG 3: Grössenklasse und Sozialhilfequote ─────────────────────

# Plan:
# a) Wachstumstabelle mit Sozialhilfedaten verknüpfen (left_join über Gemeindename)
# b) Durchschnittliche SH-Quote nach Grössenklasse berechnen
# c) Gemeinden mit auffälliger Kombination identifizieren

# a) Datentabellen zusammenführen
analyse <- wachstum |>
  left_join(sozial, by = "gemeinde") |>
  # Gemeinden ohne SH-Daten ausschliessen (fehlende Werte)
  filter(!is.na(sh_quote))

cat("\nDatenbasis:", nrow(analyse), "Gemeinden mit vollständigen Daten\n")

# b) Durchschnittliche SH-Quote nach Grössenklasse
cat("\n── Sozialhilfequote 2023 nach Gemeindegrösse\n")
analyse |>
  group_by(groessenklasse) |>
  summarise(
    anzahl          = n(),
    sh_quote_mittel = round(mean(sh_quote, na.rm = TRUE), 2),
    sh_quote_median = round(median(sh_quote, na.rm = TRUE), 2),
    sh_quote_max    = max(sh_quote, na.rm = TRUE),
    bev_mittel_2022 = round(mean(bev_2022))
  ) |>
  arrange(desc(sh_quote_mittel)) |>
  print()

# c) Auffällige Gemeinden: Starkes Wachstum UND überdurchschnittliche SH-Quote
sh_schwelle      <- mean(analyse$sh_quote, na.rm = TRUE)
wachstum_schwelle <- mean(analyse$zuwachs_pct, na.rm = TRUE)

cat(sprintf(
  "\nSchwellenwerte: SH-Quote > %.1f%% UND Wachstum > %.1f%%\n",
  sh_schwelle, wachstum_schwelle
))

cat("\n── Gemeinden mit überdurchschn. Wachstum UND hoher SH-Quote\n")
analyse |>
  filter(sh_quote > sh_schwelle, zuwachs_pct > wachstum_schwelle) |>
  left_join(gemeinde_bezirk, by = "bfs_nr") |>
  select(
    Gemeinde   = gemeinde,
    Bezirk     = bezirk,
    `Bev. 2022` = bev_2022,
    `Wachstum %` = zuwachs_pct,
    `SH-Quote %` = sh_quote,
    `SH-Kategorie` = sh_kategorie
  ) |>
  arrange(desc(sh_quote)) |>
  print()


# ── FRAGESTELLUNG 4: Zeitliche Entwicklung der grössten Gemeinden ────────────

# Welche sind die 5 grössten Gemeinden des Kantons?
top5_gemeinden <- wachstum |>
  slice_max(bev_2022, n = 5) |>
  pull(gemeinde)  # Nur den Gemeindenamen als Vektor extrahieren

cat("\n── Top-5 Gemeinden nach Bevölkerungsgrösse:\n")
cat(paste(top5_gemeinden, collapse = ", "), "\n")

# Bevölkerungsentwicklung 2015–2022 für diese 5 Gemeinden
cat("\n── Bevölkerungsentwicklung der 5 grössten Gemeinden\n")
gemeinden_long |>
  filter(gemeinde %in% top5_gemeinden) |>
  select(Gemeinde = gemeinde, Jahr = jahr, Bevölkerung = bevoelkerung) |>
  pivot_wider(names_from = Jahr, values_from = Bevölkerung) |>
  print()

# Jährliche Veränderungsrate (gegenüber Vorjahr) für Frauenfeld
cat("\n── Frauenfeld: Jährliche Bevölkerungsveränderung\n")
gemeinden_long |>
  filter(gemeinde == "Frauenfeld") |>
  arrange(jahr) |>
  mutate(
    bev_vorjahr    = lag(bevoelkerung),
    veraenderung   = bevoelkerung - bev_vorjahr,
    veraenderung_pct = round((bevoelkerung - bev_vorjahr) / bev_vorjahr * 100, 2)
  ) |>
  filter(!is.na(bev_vorjahr)) |>
  select(Jahr = jahr, Bevölkerung = bevoelkerung,
         `Veränd. abs.` = veraenderung, `Veränd. %` = veraenderung_pct) |>
  print()


# =============================================================================
# SCHRITT 4: VISUALISIERUNG (ggplot2)
# =============================================================================
# Ausblick: Die aufbereiteten Daten können direkt für Grafiken verwendet werden.
# Das Long-Format aus Schritt 2 ist die ideale Basis für ggplot2.

# Bevölkerungsentwicklung der 5 grössten Gemeinden als Liniendiagramm
gemeinden_long |>
  filter(gemeinde %in% top5_gemeinden) |>
  ggplot(aes(x = jahr, y = bevoelkerung, colour = gemeinde)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma_format(big.mark = "'")) +
  labs(
    title   = "Bevölkerungsentwicklung der 5 grössten Thurgauer Gemeinden",
    subtitle = "2015–2022, Ständige Wohnbevölkerung",
    x       = "Jahr",
    y       = "Einwohnerinnen und Einwohner",
    colour  = "Gemeinde",
    caption = "Quelle: Fachstelle für Statistik, Kanton Thurgau"
  ) +
  theme_minimal()

# Wachstum nach Bezirk als Balkendiagramm
wachstum |>
  left_join(gemeinde_bezirk, by = "bfs_nr") |>
  group_by(bezirk) |>
  summarise(zuwachs_pct_mittel = mean(zuwachs_pct)) |>
  ggplot(aes(x = reorder(bezirk, zuwachs_pct_mittel), y = zuwachs_pct_mittel,
             fill = zuwachs_pct_mittel > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#005B96", "FALSE" = "#CC0000"),
                    guide = "none") +
  labs(
    title    = "Durchschnittliches Bevölkerungswachstum nach Bezirk",
    subtitle = "Mittleres prozentuales Gemeindewachstum 2015–2022",
    x        = NULL,
    y        = "Mittleres Wachstum (%)",
    caption  = "Quelle: Fachstelle für Statistik, Kanton Thurgau"
  ) +
  theme_minimal()


# =============================================================================
# ZUSAMMENFASSUNG DER VERWENDETEN FUNKTIONEN
# =============================================================================
#
#  DATEN EINLESEN
#  ──────────────
#  read_csv()          CSV-Datei (kommagetrennt) einlesen
#  read_delim()        CSV-Datei mit beliebigem Trennzeichen (hier: Semikolon)
#  tibble()            Referenztabelle direkt im Code erstellen
#
#  TIDYR – Datenstruktur
#  ─────────────────────
#  pivot_longer()      Wide → Long: mehrere Spalten zu zwei Spalten zusammenführen
#  pivot_wider()       Long → Wide: eine Spalte in mehrere Spalten aufteilen
#
#  DPLYR – Datenanalyse
#  ────────────────────
#  rename()            Spaltennamen ändern
#  filter()            Zeilen nach Bedingung auswählen
#  select()            Spalten auswählen (und umbenennen)
#  mutate()            Neue Spalten berechnen / bestehende verändern
#  case_when()         Mehrere if/else-Bedingungen (für Kategorisierungen)
#  if_else()           Einfache if/else-Bedingung (vektorisiert)
#  group_by()          Gruppen definieren (für nachfolgende Aggregation)
#  summarise()         Gruppen zu Kennzahlen verdichten
#  left_join()         Zwei Tabellen über gemeinsame Spalte(n) verknüpfen
#  arrange()           Zeilen sortieren (desc() für absteigend)
#  slice_head()        Erste n Zeilen einer (Gruppe von) Tabelle(n) behalten
#  slice_max()         Zeilen mit den grössten Werten einer Spalte behalten
#  count()             Häufigkeiten zählen
#  distinct()          Eindeutige Werte / Zeilen behalten
#  lag()               Vorjahreswert in Zeitreihenspalte abrufen
#  pull()              Einen Spaltenvektor aus einem Data Frame extrahieren
#
# =============================================================================
