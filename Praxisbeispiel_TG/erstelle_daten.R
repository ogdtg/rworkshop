# =============================================================================
# Synthetische Bevölkerungsdaten für den Kanton Thurgau
# Zweck: Übungsdaten für R-Workshop generieren, angelehnt an das Datenformat
#        des Datensatzes sk-stat-58 auf data.tg.ch
#        ("Ständige Wohnbevölkerung Kanton Thurgau nach Gemeinden,
#        Geschlecht und Fünfjahresaltersklassen")
# =============================================================================

library(tidyverse)

set.seed(42)  # Reproduzierbarkeit sicherstellen

# === ABSCHNITT 1: Gemeinden-Referenz erstellen ===

# BFS-Nummern beginnen bei 4421 und werden fortlaufend vergeben
# Die 80 Gemeinden sind auf 5 Bezirke verteilt

gemeinden <- tribble(
  ~gemeinde,                   ~bezirk,
  # Bezirk Arbon (16 Gemeinden)
  "Arbon",                     "Arbon",
  "Amriswil",                  "Arbon",
  "Berg TG",                   "Arbon",
  "Dozwil",                    "Arbon",
  "Egnach",                    "Arbon",
  "Erlen",                     "Arbon",
  "Hauptwil-Gottshaus",        "Arbon",
  "Horn",                      "Arbon",
  "Kesswil",                   "Arbon",
  "Lanterswil",                "Arbon",
  "Neukirch-Egnach",           "Arbon",
  "Roggwil TG",                "Arbon",
  "Romanshorn",                "Arbon",
  "Salmsach",                  "Arbon",
  "Sommeri",                   "Arbon",
  "Uttwil",                    "Arbon",
  # Bezirk Frauenfeld (18 Gemeinden)
  "Frauenfeld",                "Frauenfeld",
  "Amlikon-Bissegg",           "Frauenfeld",
  "Birwinken",                 "Frauenfeld",
  "Dussnang",                  "Frauenfeld",
  "Herdern",                   "Frauenfeld",
  "Homburg",                   "Frauenfeld",
  "Hüttlingen",                "Frauenfeld",
  "Lommis",                    "Frauenfeld",
  "Matzingen",                 "Frauenfeld",
  "Mettlen",                   "Frauenfeld",
  "Neunforn",                  "Frauenfeld",
  "Pfyn",                      "Frauenfeld",
  "Schlattingen",              "Frauenfeld",
  "Stettfurt",                 "Frauenfeld",
  "Thundorf",                  "Frauenfeld",
  "Warth-Weiningen",           "Frauenfeld",
  "Wigoltingen",               "Frauenfeld",
  "Felben-Wellhausen",         "Frauenfeld",
  # Bezirk Kreuzlingen (12 Gemeinden)
  "Kreuzlingen",               "Kreuzlingen",
  "Altnau",                    "Kreuzlingen",
  "Berlingen",                 "Kreuzlingen",
  "Bottighofen",               "Kreuzlingen",
  "Ermatingen",                "Kreuzlingen",
  "Gottlieben",                "Kreuzlingen",
  "Güttingen",                 "Kreuzlingen",
  "Kemmental",                 "Kreuzlingen",
  "Lengwil",                   "Kreuzlingen",
  "Lipperswil",                "Kreuzlingen",
  "Münsterlingen",             "Kreuzlingen",
  "Tägerwilen",                "Kreuzlingen",
  # Bezirk Münchwilen (17 Gemeinden)
  "Münchwilen",                "Münchwilen",
  "Aadorf",                    "Münchwilen",
  "Bettwiesen",                "Münchwilen",
  "Bichelsee-Balterswil",      "Münchwilen",
  "Braunau",                   "Münchwilen",
  "Bussnang",                  "Münchwilen",
  "Eschlikon",                 "Münchwilen",
  "Fischingen",                "Münchwilen",
  "Leutmerken",                "Münchwilen",
  "Rickenbach TG",             "Münchwilen",
  "Schönholzerswilen",         "Münchwilen",
  "Sirnach",                   "Münchwilen",
  "Tobel-Tägerschen",          "Münchwilen",
  "Wängi",                     "Münchwilen",
  "Wilen bei Wil",             "Münchwilen",
  "Wuppenau",                  "Münchwilen",
  "Wiezikon",                  "Münchwilen",
  # Bezirk Weinfelden (17 Gemeinden)
  "Weinfelden",                "Weinfelden",
  "Alterswilen",               "Weinfelden",
  "Bürglen TG",                "Weinfelden",
  "Donzhausen",                "Weinfelden",
  "Kradolf-Schönenberg",       "Weinfelden",
  "Märstetten",                "Weinfelden",
  "Müllheim",                  "Weinfelden",
  "Sulgen",                    "Weinfelden",
  "Wagenhausen",               "Weinfelden",
  "Raperswilen",               "Weinfelden",
  "Triboltingen",              "Weinfelden",
  "Wäldi",                     "Weinfelden",
  "Hohentannen",               "Weinfelden",
  "Sitterdorf",                "Weinfelden",
  "Birwinken-Tal",             "Weinfelden",
  "Weerswilen",                "Weinfelden",
  "Schönenberg an der Thur",   "Weinfelden"
)

# BFS-Nummern fortlaufend ab 4421 vergeben
gemeinden <- gemeinden |>
  mutate(bfs_nr = 4420 + row_number())

# Städte und grössere Zentren erhalten höhere Basiseinwohnerzahl;
# Zentrumsgemeinden werden explizit mit höheren Ausgangswerten versehen
gemeinden <- gemeinden |>
  mutate(
    basis_einwohner = case_when(
      gemeinde == "Frauenfeld"   ~ sample(20000:45000, 1),
      gemeinde == "Kreuzlingen"  ~ sample(18000:35000, 1),
      gemeinde == "Arbon"        ~ sample(12000:20000, 1),
      gemeinde == "Romanshorn"   ~ sample(10000:18000, 1),
      gemeinde == "Amriswil"     ~ sample(10000:16000, 1),
      gemeinde == "Weinfelden"   ~ sample(10000:15000, 1),
      gemeinde == "Aadorf"       ~ sample(8000:12000,  1),
      gemeinde == "Sirnach"      ~ sample(6000:10000,  1),
      gemeinde == "Münchwilen"   ~ sample(5000:9000,   1),
      gemeinde == "Tägerwilen"   ~ sample(5000:8000,   1),
      TRUE                       ~ sample(1000:6000,   1)
    )
  )


# === ABSCHNITT 2: Altersklassen definieren ===

# Standardisierte Fünfjahresaltersklassen gemäss BFS-Nomenklatur
altersklassen <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80-84", "85-89", "90+"
)

# Realistische Altersverteilung: junge Erwachsene (20-39) am stärksten vertreten,
# Kinder moderat, Ältere abnehmend aber vorhanden
# Diese Gewichte spiegeln die demografische Struktur einer Schweizer Gemeinde wider
alters_gewichte <- c(
  0.055,  # 0-4
  0.055,  # 5-9
  0.055,  # 10-14
  0.060,  # 15-19
  0.075,  # 20-24
  0.085,  # 25-29
  0.085,  # 30-34
  0.080,  # 35-39
  0.070,  # 40-44
  0.065,  # 45-49
  0.065,  # 50-54
  0.060,  # 55-59
  0.055,  # 60-64
  0.045,  # 65-69
  0.035,  # 70-74
  0.025,  # 75-79
  0.015,  # 80-84
  0.010,  # 85-89
  0.005   # 90+
)


# === ABSCHNITT 3: Funktion zum Erstellen der Gemeindedaten ===

erstelle_gemeinde_daten <- function(bfs, name, bezirk_name, basis_einwohner) {

  # Jährliche Wachstumsraten leicht variieren, um realistische Dynamik abzubilden
  wachstum_2022 <- 1 + runif(1, 0.005, 0.015)
  wachstum_2023 <- 1 + runif(1, 0.005, 0.015)

  # Alle Kombinationen aus Jahr, Geschlecht und Altersklasse aufspannen
  expand.grid(
    jahr        = c(2021L, 2022L, 2023L),
    geschlecht  = c("Männlich", "Weiblich"),
    altersklasse = altersklassen,
    stringsAsFactors = FALSE
  ) |>
    as_tibble() |>
    mutate(
      bfs_nr  = bfs,
      gemeinde = name,
      bezirk  = bezirk_name,

      # Jahresfaktor: Basisjahr 2021, danach kumuliertes Wachstum
      jahres_faktor = case_when(
        jahr == 2021L ~ 1.0,
        jahr == 2022L ~ wachstum_2022,
        jahr == 2023L ~ wachstum_2022 * wachstum_2023
      ),

      # Altersgewichte werden per Index zugeordnet
      alters_idx = match(altersklasse, altersklassen),
      gewicht    = alters_gewichte[alters_idx],

      # Gesamtbevölkerung gleichmässig auf beide Geschlechter verteilen
      # (leichte Zufallsvariation simuliert reale Ungleichverteilung)
      geschlechts_faktor = ifelse(
        geschlecht == "Männlich",
        runif(n(), 0.48, 0.52),
        1 - runif(n(), 0.48, 0.52)
      ),

      # Anzahl als gerundete Integer-Zahl berechnen
      anzahl = as.integer(round(
        basis_einwohner * jahres_faktor * gewicht * geschlechts_faktor *
          # Kleines zufälliges Rauschen für mehr Realismus
          runif(n(), 0.85, 1.15)
      ))
    ) |>
    select(bfs_nr, gemeinde, bezirk, jahr, geschlecht, altersklasse, anzahl)
}


# === ABSCHNITT 4: Datenqualitätsprobleme einbauen ===

fuege_qualitaetsprobleme_ein <- function(df) {

  n <- nrow(df)

  # ~10% der Gemeindenamen erhalten führende/nachfolgende Leerzeichen,
  # um typische Import- und Tippfehler zu simulieren
  whitespace_idx <- which(runif(n) < 0.10)
  df$gemeinde[whitespace_idx] <- paste0("  ", df$gemeinde[whitespace_idx], "  ")

  # ~2% der Anzahl-Werte werden auf NA gesetzt,
  # um fehlende Meldungen in Verwaltungsdaten zu simulieren
  na_idx <- which(runif(n) < 0.02)
  df$anzahl[na_idx] <- NA_integer_

  df
}


# === ABSCHNITT 5: Ausgabeverzeichnis vorbereiten und Daten generieren ===

# Verzeichnis anlegen, falls noch nicht vorhanden
dir.create("Praxisbeispiel_TG/daten", recursive = TRUE, showWarnings = FALSE)

# Für jede Gemeinde Daten generieren und als eigene CSV-Datei speichern
# Separate Dateien pro Gemeinde entsprechen dem Originalformat von data.tg.ch
walk(seq_len(nrow(gemeinden)), function(i) {

  daten <- erstelle_gemeinde_daten(
    bfs            = gemeinden$bfs_nr[i],
    name           = gemeinden$gemeinde[i],
    bezirk_name    = gemeinden$bezirk[i],
    basis_einwohner = gemeinden$basis_einwohner[i]
  )

  # Datenqualitätsprobleme einbauen (nach der eigentlichen Datengenerierung,
  # damit die Probleme unabhängig von der Gemeindengrösse verteilt sind)
  daten <- fuege_qualitaetsprobleme_ein(daten)

  dateiname <- paste0(
    "Praxisbeispiel_TG/daten/Einwohner_",
    gemeinden$bfs_nr[i],
    ".csv"
  )

  write_csv(daten, dateiname)
})

cat("\u2713 Daten erstellt:", nrow(gemeinden), "CSV-Dateien in Praxisbeispiel_TG/daten/\n")
