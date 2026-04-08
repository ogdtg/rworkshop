##############################################################################
##                                                                          ##
##         PRAXISBEISPIEL: EINWOHNERSTATISTIK REGION MUSTERTAL              ##
##                                                                          ##
##  Szenario: Das Statistikamt erhält jährlich CSV-Dateien aus 4 Gemeinden. ##
##  Ziel: Ein vollständig automatisierter Auswertungsworkflow.              ##
##                                                                          ##
##  Dieser Workshop deckt ab:                                               ##
##    purrr · stringr · lubridate · dplyr · tidyr · case_when              ##
##    eigene Funktionen · ggplot2 · openxlsx                                ##
##                                                                          ##
##  Empfehlung: Zeile für Zeile ausführen mit Ctrl+Enter                    ##
##                                                                          ##
##############################################################################

set.seed(42)   # Reproduzierbarkeit sicherstellen

# =============================================================================
# KAPITEL 0: Packages laden
# =============================================================================

library(tidyverse)   # dplyr, tidyr, stringr, lubridate, purrr, ggplot2, readr
library(openxlsx)
library(scales)      # für percent-Formatierung in ggplot2


# =============================================================================
# KAPITEL 1: Künstliche Rohdaten erstellen und als CSVs speichern
# =============================================================================
#
# Wir simulieren, dass wir von 4 Gemeinden je 3 Jahresdateien erhalten haben.
# In der Praxis kämen diese direkt aus einem Einwohnerregister.

# --- 1a) Referenztabelle: Gemeinden ---

gemeinden_ref <- tibble(
  gemeinde_code = c(2101L, 2102L, 2103L, 2104L),
  gemeinde_name = c("Stadtberg", "Seebach", "Waldheim", "Bergdorf"),
  bezirk        = c("Bezirk Nord", "Bezirk Nord", "Bezirk Süd", "Bezirk Süd"),
  flaeche_km2   = c(12.4, 8.7, 23.1, 15.8)
)

# --- 1b) Hilfsvektoren für die Datengenerierung ---

nachnamen  <- c("Müller","Meier","Schmid","Keller","Fischer",
                "Weber","Huber","Bauer","Zimmermann","Graf")
vornamen_m <- c("Hans","Peter","Thomas","Martin","Stefan","Klaus","Andreas","Beat")
vornamen_f <- c("Anna","Maria","Sandra","Julia","Laura","Monika","Petra","Ursula")
# 70% CH, 12% DE, 9% IT, 6% FR, 3% andere (realistische Verteilung)
nats <- c(rep("CH",70), rep("DE",12), rep("IT",9), rep("FR",6), rep("other",3))

# --- 1c) Funktion: Datensatz für eine Gemeinde + Jahr erstellen ---

erstelle_gemeinde_daten <- function(g_code, jahr, n = 80) {
  # Seed pro Gemeinde+Jahr → gleiche Daten bei wiederholtem Aufruf
  set.seed(g_code + jahr)

  geschlecht <- sample(c("M", "F"), n, replace = TRUE)
  vorname    <- ifelse(
    geschlecht == "M",
    sample(vornamen_m, n, replace = TRUE),
    sample(vornamen_f, n, replace = TRUE)
  )
  # Absichtlich schmutzige Strings (~15% mit Leerzeichen)
  vorname  <- ifelse(runif(n) < .15, paste0("  ", vorname, "  "), vorname)
  nachname <- sample(nachnamen, n, replace = TRUE)
  nachname <- ifelse(runif(n) < .10, paste0(nachname, " "),  nachname)

  geb <- sample(
    seq(as.Date("1940-01-01"), as.Date("2010-12-31"), by = "day"),
    n, replace = TRUE
  )
  zuzug <- sample(
    seq(as.Date(paste0(jahr - 5, "-01-01")), as.Date(paste0(jahr, "-12-31")), by = "day"),
    n, replace = TRUE
  )

  tibble(
    id            = paste0(jahr, "_", g_code, "_", sprintf("%04d", seq_len(n))),
    vorname       = vorname,
    nachname      = nachname,
    geburtsdatum  = format(geb,   "%d.%m.%Y"),   # String DD.MM.YYYY (typisch aus Register)
    geschlecht    = geschlecht,
    nationalitaet = sample(nats, n, replace = TRUE),
    gemeinde_code = g_code,
    zuzug_datum   = format(zuzug, "%Y-%m-%d"),
    stichtag      = paste0(jahr, "-12-31")
  )
}

# --- 1d) Alle CSVs generieren und speichern ---

dir.create("Daten/einwohner", recursive = TRUE, showWarnings = FALSE)

for (jahr in 2022:2024) {
  for (g_code in gemeinden_ref$gemeinde_code) {
    df   <- erstelle_gemeinde_daten(g_code, jahr)
    pfad <- paste0("Daten/einwohner/einwohner_", g_code, "_", jahr, ".csv")
    write_csv(df, pfad)
  }
}

message("✓ 12 CSV-Dateien erstellt in Daten/einwohner/")
list.files("Daten/einwohner/")


# =============================================================================
# KAPITEL 2: Alle CSVs einlesen – purrr::map_df
# =============================================================================
#
# Statt 12 read_csv()-Aufrufe: eine Zeile mit map_df.
# Wenn nächstes Jahr weitere Gemeinden hinzukommen → keine Codeänderung.

# --- 2a) Alle Dateipfade ermitteln ---

csv_files <- list.files("Daten/einwohner", pattern = "\\.csv$", full.names = TRUE)
length(csv_files)   # sollte 12 sein

# --- 2b) Einlesefunktion mit Fortschrittsanzeige ---

lese_einwohner_csv <- function(pfad) {
  message("Lese: ", basename(pfad))
  read_csv(pfad, col_types = cols(.default = "c")) |>    # alles als character (→ wir parsen selbst)
    mutate(quelldatei = basename(pfad))
}

# --- 2c) Alle CSVs auf einmal laden ---

roh_alle <- map_df(csv_files, lese_einwohner_csv)

nrow(roh_alle)                    # Gesamtanzahl Datensätze
n_distinct(roh_alle$stichtag)    # 3 verschiedene Stichtage

# --- Alternativer for-loop (auskommentiert) ---
# roh_alle2 <- list()
# for (pfad in csv_files) {
#   roh_alle2[[pfad]] <- lese_einwohner_csv(pfad)
# }
# roh_alle2 <- bind_rows(roh_alle2)


# =============================================================================
# KAPITEL 3: Daten bereinigen – stringr & lubridate
# =============================================================================

# --- 3a) Strings bereinigen und Daten parsen ---

sauber <- roh_alle |>
  mutate(
    # Leerzeichen an Anfang/Ende entfernen (str_trim)
    vorname      = str_trim(vorname),
    nachname     = str_trim(nachname),
    vollname     = str_c(vorname, nachname, sep = " "),

    # Datumsstrings → echte Datumsobjekte (lubridate)
    geburtsdatum = dmy(geburtsdatum),      # "15.03.1985" → Date
    stichtag     = ymd(stichtag),          # "2024-12-31" → Date
    zuzug_datum  = ymd(zuzug_datum),

    # gemeinde_code zurück zu integer (hatten alles als character geladen)
    gemeinde_code = as.integer(gemeinde_code),

    # Berechnungen auf Basis der geparsten Daten
    alter        = as.integer((stichtag - geburtsdatum) / 365.25),
    jahr         = year(stichtag),
    geburtsmonat = month(geburtsdatum, label = TRUE)
  )

# --- 3b) Qualitätskontrolle ---

# Wie viele Geburtsdaten konnten nicht geparst werden?
sauber |> filter(is.na(geburtsdatum)) |> nrow()

# Enthält ein Nachname Ziffern? (wäre verdächtig)
sauber |>
  mutate(name_verdaechtig = str_detect(nachname, "\\d")) |>
  filter(name_verdaechtig)

glimpse(sauber)


# =============================================================================
# KAPITEL 4: Daten anreichern – left_join & case_when
# =============================================================================

# --- 4a) Gemeindeinformationen anfügen ---

aufbereitet <- sauber |>
  left_join(gemeinden_ref, by = "gemeinde_code") |>
  mutate(
    # Altersklassen mit case_when (besser als verschachteltes ifelse)
    altersklasse = case_when(
      alter <  6  ~ "0–5",
      alter < 18  ~ "6–17",
      alter < 40  ~ "18–39",
      alter < 65  ~ "40–64",
      .default    =  "65+"
    ),
    # Als geordneter Faktor → richtige Sortierung in Grafiken
    altersklasse = factor(altersklasse,
                          levels = c("0–5","6–17","18–39","40–64","65+")),

    # Nationalitätsgruppen
    nationalitaet_gr = case_when(
      nationalitaet == "CH"                ~ "Schweiz",
      nationalitaet %in% c("DE", "AT")    ~ "DACH (ohne CH)",
      .default                              =  "Übrige"
    )
  )

# --- 4b) Qualitätskontrolle: unbekannte Gemeindecodes? ---
# anti_join zeigt Zeilen aus sauber, die keinen Match in gemeinden_ref haben
anti_join(sauber, gemeinden_ref, by = "gemeinde_code")


# =============================================================================
# KAPITEL 5: Eigene Hilfsfunktionen schreiben
# =============================================================================
#
# Wiederkehrende Operationen einmal korrekt schreiben und dann überall nutzen.

# --- 5a) Prozentwert formatieren ---

als_prozent <- function(anteil, stellen = 1) {
  if (!is.numeric(anteil)) stop("'anteil' muss numerisch sein.")
  paste0(round(anteil * 100, stellen), " %")
}

als_prozent(0.374)
als_prozent(c(0.1, 0.5, 0.999), stellen = 0)

# --- 5b) Bevölkerungsdichte berechnen ---

bev_dichte <- function(einwohner, flaeche_km2) {
  if (any(flaeche_km2 <= 0)) stop("'flaeche_km2' muss > 0 sein.")
  round(einwohner / flaeche_km2, 1)
}

bev_dichte(285000, 991)

# --- 5c) Zusammenfassung für eine Gemeinde ---

zusammenfassung_gemeinde <- function(df, gmd, j) {
  df |>
    filter(gemeinde_name == gmd, jahr == j) |>
    summarise(
      einwohner    = n(),
      anteil_ch    = als_prozent(mean(nationalitaet == "CH")),
      anteil_ausl  = als_prozent(mean(nationalitaet != "CH")),
      median_alter = median(alter, na.rm = TRUE),
      anteil_65p   = als_prozent(mean(altersklasse == "65+"))
    )
}

zusammenfassung_gemeinde(aufbereitet, "Stadtberg", 2024)

# --- 5d) Lesbaren Berichtstext erstellen ---

erstelle_bericht_text <- function(df, gmd, j) {
  z <- zusammenfassung_gemeinde(df, gmd, j)
  paste0(
    "=== ", gmd, " (", j, ") ===\n",
    "  Einwohner:    ", z$einwohner, "\n",
    "  Anteil CH:    ", z$anteil_ch, "\n",
    "  Anteil Ausl.: ", z$anteil_ausl, "\n",
    "  Medianalter:  ", z$median_alter, " Jahre\n",
    "  Anteil 65+:   ", z$anteil_65p, "\n"
  )
}

cat(erstelle_bericht_text(aufbereitet, "Stadtberg", 2024))

# Berichte für alle 4 Gemeinden generieren (lapply gibt eine Liste zurück)
alle_berichte <- lapply(gemeinden_ref$gemeinde_name, erstelle_bericht_text,
                        df = aufbereitet, j = 2024)
cat(unlist(alle_berichte), sep = "\n")


# =============================================================================
# KAPITEL 6: Analyse – group_by, summarise, across, pivot
# =============================================================================

# --- 6a) Einwohner pro Jahr und Gemeinde ---

pro_jahr_gemeinde <- aufbereitet |>
  group_by(jahr, gemeinde_name, bezirk) |>
  summarise(
    einwohner   = n(),
    anteil_ch   = mean(nationalitaet == "CH"),
    median_alter = median(alter, na.rm = TRUE),
    .groups     = "drop"
  )

pro_jahr_gemeinde

# --- 6b) Altersklassenverteilung 2024 ---

altersklassen_2024 <- aufbereitet |>
  filter(jahr == 2024) |>
  group_by(altersklasse) |>
  summarise(n = n()) |>
  mutate(anteil = als_prozent(n / sum(n)))

altersklassen_2024

# --- 6c) Jahresvergleich mit pivot_wider ---

jahresvergleich <- pro_jahr_gemeinde |>
  select(gemeinde_name, bezirk, jahr, einwohner) |>
  pivot_wider(names_from = jahr, values_from = einwohner) |>
  mutate(
    wachstum_abs = `2024` - `2022`,
    wachstum_pct = round((`2024` - `2022`) / `2022` * 100, 1)
  ) |>
  arrange(bezirk, gemeinde_name)

jahresvergleich

# --- 6d) across(): mehrere Spalten auf einmal bearbeiten ---
# Alle numerischen Spalten auf 1 Nachkommastelle runden

pro_jahr_gemeinde |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# --- 6e) slice_max: bevölkerungsreichste Gemeinde pro Jahr ---

pro_jahr_gemeinde |>
  group_by(jahr) |>
  slice_max(einwohner, n = 1) |>
  select(jahr, gemeinde_name, einwohner)


# =============================================================================
# KAPITEL 7: Visualisierung – ggplot2
# =============================================================================

dir.create("Output", showWarnings = FALSE)

# --- 7a) Altersverteilung 2024 nach Bezirk ---

p_alter <- aufbereitet |>
  filter(jahr == 2024) |>
  count(altersklasse, bezirk) |>
  ggplot(aes(x = altersklasse, y = n, fill = bezirk)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Altersverteilung 2024 nach Bezirk",
    subtitle = "Region Mustertal – Einwohnerregister",
    x        = "Altersklasse",
    y        = "Anzahl Einwohner",
    fill     = "Bezirk"
  ) +
  theme_minimal(base_size = 13)

p_alter
ggsave("Output/altersverteilung_2024.png", p_alter, width = 8, height = 5, dpi = 150)

# --- 7b) Einwohnerentwicklung 2022–2024 ---

p_verlauf <- pro_jahr_gemeinde |>
  ggplot(aes(x = jahr, y = einwohner,
             colour = gemeinde_name, group = gemeinde_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  geom_label(aes(label = einwohner), nudge_y = 2, size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = 2022:2024) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title   = "Einwohnerentwicklung 2022–2024",
    x       = NULL,
    y       = "Einwohner",
    colour  = "Gemeinde"
  ) +
  theme_minimal(base_size = 13)

p_verlauf
ggsave("Output/einwohnerentwicklung.png", p_verlauf, width = 8, height = 5, dpi = 150)

# --- 7c) Nationalitätsanteile nach Jahr (facettiert) ---

p_nat <- aufbereitet |>
  count(jahr, nationalitaet_gr) |>
  group_by(jahr) |>
  mutate(anteil = n / sum(n)) |>
  ggplot(aes(x = as.factor(jahr), y = anteil, fill = nationalitaet_gr)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Nationalitätsanteile nach Jahr",
    x     = "Jahr",
    y     = "Anteil",
    fill  = "Gruppe"
  ) +
  theme_minimal(base_size = 13)

p_nat
ggsave("Output/nationalitaeten.png", p_nat, width = 7, height = 5, dpi = 150)

message("✓ 3 Grafiken gespeichert in Output/")


# =============================================================================
# KAPITEL 8: Excel-Export – openxlsx
# =============================================================================

# --- 8a) Styles definieren ---

style_titel <- createStyle(
  fontSize        = 13,
  textDecoration  = "bold",
  fgFill          = "#003366",
  fontColour      = "white",
  halign          = "center",
  valign          = "center"
)
style_subtitel <- createStyle(
  italic  = TRUE,
  fgFill  = "#F2F2F2",
  halign  = "left"
)
style_header <- createStyle(
  textDecoration = "bold",
  border         = "Bottom",
  fgFill         = "#D9E1F2",
  halign         = "center"
)
style_zahlen <- createStyle(numFmt = "#,##0", halign = "right")
style_pct    <- createStyle(numFmt = "0.0\"%\"", halign = "right")

# --- 8b) Workbook erstellen ---

wb <- createWorkbook()

# ── Sheet 1: Jahresvergleich ──────────────────────────────────────────────
addWorksheet(wb, "Jahresvergleich")

# Titelzeile schreiben und formatieren
writeData(wb, "Jahresvergleich",
          "Einwohnerstatistik Region Mustertal – Stand 2024", startRow = 1)
mergeCells(wb, "Jahresvergleich", rows = 1, cols = 1:ncol(jahresvergleich))
addStyle(wb, "Jahresvergleich", style_titel,
         rows = 1, cols = 1:ncol(jahresvergleich), gridExpand = TRUE)

# Untertitelzeile
writeData(wb, "Jahresvergleich",
          "Quelle: Einwohnerregister der Gemeinden", startRow = 2)
mergeCells(wb, "Jahresvergleich", rows = 2, cols = 1:ncol(jahresvergleich))
addStyle(wb, "Jahresvergleich", style_subtitel,
         rows = 2, cols = 1:ncol(jahresvergleich), gridExpand = TRUE)

# Daten ab Zeile 4 eintragen
writeData(wb, "Jahresvergleich", jahresvergleich, startRow = 4)

# Header-Zeile formatieren
addStyle(wb, "Jahresvergleich", style_header,
         rows = 4, cols = 1:ncol(jahresvergleich), gridExpand = TRUE)

# Zahlenspalten (Einwohnerzahlen und Wachstum) formatieren
addStyle(wb, "Jahresvergleich", style_zahlen,
         rows = 5:(4 + nrow(jahresvergleich)), cols = 3:5, gridExpand = TRUE)
addStyle(wb, "Jahresvergleich", style_pct,
         rows = 5:(4 + nrow(jahresvergleich)), cols = 6, gridExpand = TRUE)

setColWidths(wb, "Jahresvergleich", cols = 1:ncol(jahresvergleich), widths = "auto")
setRowHeights(wb, "Jahresvergleich", rows = 1, heights = 25)

# ── Sheet 2: Altersklassen ────────────────────────────────────────────────
addWorksheet(wb, "Altersklassen")

writeData(wb, "Altersklassen",
          "Altersklassenverteilung 2024 – Region Mustertal", startRow = 1)
mergeCells(wb, "Altersklassen", rows = 1, cols = 1:ncol(altersklassen_2024))
addStyle(wb, "Altersklassen", style_titel,
         rows = 1, cols = 1:ncol(altersklassen_2024), gridExpand = TRUE)

writeData(wb, "Altersklassen", altersklassen_2024, startRow = 3)
addStyle(wb, "Altersklassen", style_header,
         rows = 3, cols = 1:ncol(altersklassen_2024), gridExpand = TRUE)

# Zeilen farblich codieren – for-loop über Altersklassen
altersfarben <- c("0–5"   = "#EBF5FB",
                  "6–17"  = "#D5EAD0",
                  "18–39" = "#FEF9E7",
                  "40–64" = "#FDF2E9",
                  "65+"   = "#F9EBEA")

for (i in seq_len(nrow(altersklassen_2024))) {
  klasse <- as.character(altersklassen_2024$altersklasse[i])
  farbe  <- altersfarben[[klasse]]
  addStyle(wb, "Altersklassen",
           createStyle(fgFill = farbe),
           rows = 3 + i, cols = 1:ncol(altersklassen_2024),
           gridExpand = TRUE, stack = TRUE)
}

# Altersverteilungs-Grafik ins Sheet einfügen
insertImage(wb, "Altersklassen", "Output/altersverteilung_2024.png",
            startRow = 10, startCol = 1, width = 14, height = 9, units = "cm")

setColWidths(wb, "Altersklassen", cols = 1:ncol(altersklassen_2024), widths = "auto")

# --- 8c) Speichern ---
saveWorkbook(wb, "Output/Einwohnerstatistik_2024.xlsx", overwrite = TRUE)
message("✓ Excel-Report gespeichert: Output/Einwohnerstatistik_2024.xlsx")


# =============================================================================
# KAPITEL 9: Bonus – Loops & purrr für gemeindespezifische Grafiken
# =============================================================================

# --- 9a) for-loop: Zeitreihengrafik je Gemeinde speichern ---

for (gmd in gemeinden_ref$gemeinde_name) {
  p <- aufbereitet |>
    filter(gemeinde_name == gmd) |>
    count(jahr, altersklasse) |>
    ggplot(aes(x = jahr, y = n, fill = altersklasse)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = 2022:2024) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1) +
    labs(title   = paste("Altersstruktur:", gmd),
         x = NULL, y = "Anteil", fill = "Altersklasse") +
    theme_minimal(base_size = 12)

  ggsave(paste0("Output/", gmd, "_altersstruktur.png"), p,
         width = 7, height = 4.5, dpi = 120)
}

message("✓ 4 gemeindespezifische Grafiken gespeichert.")

# --- 9b) Dasselbe mit purrr::walk (kürzer, kein Loop nötig) ---

gemeinden_ref$gemeinde_name |>
  walk(function(gmd) {
    p <- aufbereitet |>
      filter(gemeinde_name == gmd) |>
      count(jahr, geschlecht) |>
      ggplot(aes(x = jahr, y = n, colour = geschlecht, group = geschlecht)) +
      geom_line(linewidth = 1.1) + geom_point(size = 3) +
      scale_x_continuous(breaks = 2022:2024) +
      labs(title = paste("Geschlechterverteilung:", gmd),
           x = NULL, y = "Einwohner", colour = "Geschlecht") +
      theme_minimal(base_size = 12)

    ggsave(paste0("Output/", gmd, "_geschlecht.png"), p,
           width = 6, height = 4, dpi = 120)
  })

message("✓ 4 Geschlechter-Grafiken gespeichert.")


# =============================================================================
# ABSCHLUSS: Erstellte Dateien
# =============================================================================

cat("\n========================================\n")
cat("  Erstellte Outputs:\n")
cat("========================================\n")
cat("  CSV-Rohdaten:  ", length(list.files("Daten/einwohner")), "Dateien\n")
cat("  Grafiken:      ", length(list.files("Output", pattern="\\.png$")), "PNG-Dateien\n")
cat("  Excel-Report:  Output/Einwohnerstatistik_2024.xlsx\n")
cat("========================================\n\n")
cat("Verwendete Techniken:\n")
cat("  purrr::map_df   → alle CSVs auf einmal einlesen\n")
cat("  stringr::str_trim → Leerzeichen bereinigen\n")
cat("  lubridate::dmy  → Strings zu Datumsobjekten\n")
cat("  dplyr::left_join → Referenzdaten anfügen\n")
cat("  case_when       → Alters- und Nationalitätsklassen\n")
cat("  eigene Funktionen → als_prozent, zusammenfassung_gemeinde\n")
cat("  tidyr::pivot_wider → Jahresvergleichstabelle\n")
cat("  dplyr::across   → mehrere Spalten gleichzeitig bearbeiten\n")
cat("  ggplot2         → Balken-, Linien- und Flächendiagramme\n")
cat("  openxlsx        → formatierter Excel-Report mit Grafik\n")
cat("  for-loop + walk → gemeindespezifische Grafiken\n")
