# =============================================================================
# Auswertung_Feedback.R
# =============================================================================
# Zweck:    Auswertung der Feedbackumfragen zum R-Workshop (Kanton Thurgau)
# Umfragen: Umfrage 1 = direkt nach Kursabschluss (Sofort)
#           Umfrage 2 = ca. 4 Monate nach Kursabschluss (Nachbefragung)
# Hinweis:  Das Skript enthält Beispieldaten für 7 Kursteilnehmende.
#           Bei realer Auswertung die Abschnitte "Beispieldaten" ersetzen.
# Kurs:     6 Sitzungen à 3.5 Stunden, 7 Teilnehmende
# =============================================================================


# === KAPITEL 0: BIBLIOTHEKEN ===
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(openxlsx)


# === KAPITEL 1: BEISPIELDATEN ===
# Künstliche Daten für 7 Teilnehmende – bei realer Auswertung ersetzen

# --- 1a) Umfrage 1: Direkt nach Kursabschluss ---
umfrage1 <- tibble(
  id                = 1:7,
  zufriedenheit     = c(5, 4, 5, 4, 5, 3, 4),   # F1: 1-5
  erwartungen       = c(4, 4, 5, 3, 4, 3, 5),    # F2: 1-5
  empfehlung        = c("Ja, auf jeden Fall", "Ja, auf jeden Fall",
                        "Ja, auf jeden Fall", "Ja, mit Einschränkungen",
                        "Ja, auf jeden Fall", "Bin unsicher",
                        "Ja, auf jeden Fall"),    # F3
  relevanz          = c(4, 5, 4, 3, 5, 4, 4),    # F4: 1-5
  level_vorher      = c(0, 1, 0, 2, 0, 1, 0),    # F8: 0-5
  level_nachher     = c(3, 3, 2, 3, 3, 2, 3),    # F9: 0-5
  r_vorstellen      = c(4, 5, 3, 4, 5, 3, 4),    # F10: 1-5
  tempo             = c("Genau richtig", "Zu schnell", "Genau richtig",
                        "Zu schnell", "Genau richtig", "Genau richtig",
                        "Zu schnell"),            # F5
  aufwand_ok        = c("Ja, gut investierte Zeit", "Ja, gut investierte Zeit",
                        "Eher ja", "Schwer zu sagen",
                        "Ja, gut investierte Zeit", "Eher ja",
                        "Ja, gut investierte Zeit"),
  zeitaufwand_ok_num = c(5, 5, 4, 3, 5, 4, 5)   # numerische Entsprechung
)

# Welche Themen wurden als besonders wertvoll eingeschätzt?
themen_names <- c("Daten einlesen", "Tidy Data", "dplyr", "tidyr",
                  "stringr", "lubridate", "ggplot2", "Funktionen",
                  "purrr", "openxlsx", "Praxisbeispiel")

# Realistische Verteilung: dplyr und Daten einlesen beliebt, purrr weniger
themen_wertvoll <- expand_grid(id = 1:7, thema = themen_names) |>
  mutate(
    wertvoll = case_when(
      thema == "Daten einlesen" ~ id %in% c(1,2,3,4,5,6),
      thema == "dplyr"          ~ id %in% c(1,2,3,5,6,7),
      thema == "ggplot2"        ~ id %in% c(1,2,4,5,7),
      thema == "Praxisbeispiel" ~ id %in% c(1,3,5,7),
      thema == "tidyr"          ~ id %in% c(2,3,5,7),
      thema == "lubridate"      ~ id %in% c(1,3,5),
      thema == "stringr"        ~ id %in% c(2,4,6),
      thema == "openxlsx"       ~ id %in% c(1,5,7),
      thema == "Tidy Data"      ~ id %in% c(2,6),
      thema == "Funktionen"     ~ id %in% c(3,7),
      thema == "purrr"          ~ id %in% c(5),
      TRUE ~ FALSE
    )
  )

# Sitzungsbewertungen (Verständlichkeit + Relevanz pro Session)
set.seed(42)
sitzungen <- expand_grid(id = 1:7, sitzung = paste0("S", 1:6)) |>
  mutate(
    sitzung       = factor(sitzung, levels = paste0("S", 1:6)),
    verstaendlich = c(4,4,3,4,5,4, 5,4,3,5,5,4, 4,3,3,4,4,3,
                      3,3,2,3,4,3, 5,4,4,5,5,5, 4,4,3,4,5,4,
                      4,3,3,4,4,3)[1:(7*6)],
    relevant      = c(4,5,4,5,5,4, 5,4,4,5,4,4, 4,4,3,4,4,3,
                      3,3,2,3,3,2, 5,5,4,5,5,5, 4,4,3,4,4,4,
                      4,4,3,4,4,3)[1:(7*6)]
  )

# --- 1b) Umfrage 2: Nachbefragung nach ca. 4 Monaten ---
umfrage2 <- tibble(
  id                    = 1:7,
  nutzung               = c("regelmaessig", "gelegentlich", "selten",
                             "nein", "gelegentlich", "versucht", "selten"),
  arbeitsveraenderung   = c(4, 3, 2, 1, 3, 1, 2),          # F5: 1-5
  zeitersparnis         = c("3-8 Stunden", "1-3 Stunden",
                             "Weniger als 1 Stunde", "Ich spare keine Zeit",
                             "1-3 Stunden", "Ich spare keine Zeit",
                             "Weniger als 1 Stunde"),         # F6
  aufwand_gerechtfertigt = c(5, 4, 3, 1, 4, 2, 3),          # F7: 1-5
  level_heute           = c(3, 3, 2, 1, 3, 1, 2),           # F8: 0-5
  ki_nutzung            = c("Ja, regelmässig", "Ja, manchmal",
                             "Nein, andere Quellen",
                             "Nein, aus Datenschutzgründen nicht möglich",
                             "Ja, manchmal", "Nein, andere Quellen",
                             "Ja, manchmal"),                 # F11
  ki_hilfreich          = c("Sehr hilfreich", "Eher hilfreich",
                             NA, NA, "Eher hilfreich", NA, "Eher hilfreich"),
  grundwissen_ki        = c("Ja, definitiv", "Eher ja", "Schwer zu sagen",
                             "Nicht zutreffend", "Eher ja", "Nicht zutreffend",
                             "Eher ja"),                      # F12
  kurs_empfehlung       = c("Ja, auf jeden Fall", "Ja, auf jeden Fall",
                             "Ja, aber mit Anpassungen", "Bin unsicher",
                             "Ja, auf jeden Fall", "Ja, aber mit Anpassungen",
                             "Ja, auf jeden Fall")
)

# Welche Techniken werden im Arbeitsalltag eingesetzt?
techniken_names <- c("Daten einlesen (read_csv)", "select / filter",
                     "mutate", "group_by / summarise",
                     "ggplot2", "stringr", "lubridate",
                     "pivot_longer / wider", "Eigene Funktionen", "purrr")

# Nur Personen die R aktiv nutzen (id 1,2,5) oder selten (3,7)
techniken_genutzt <- expand_grid(id = 1:7, technik = techniken_names) |>
  mutate(
    genutzt = case_when(
      id == 4 | id == 6 ~ FALSE,  # nutzen R nicht
      technik == "Daten einlesen (read_csv)" ~ id %in% c(1,2,3,5,7),
      technik == "select / filter"           ~ id %in% c(1,2,3,5,7),
      technik == "mutate"                    ~ id %in% c(1,2,5),
      technik == "group_by / summarise"      ~ id %in% c(1,2,5),
      technik == "ggplot2"                   ~ id %in% c(1,5),
      technik == "stringr"                   ~ id %in% c(1,2),
      technik == "lubridate"                 ~ id %in% c(1,3),
      technik == "pivot_longer / wider"      ~ id %in% c(2,5),
      technik == "Eigene Funktionen"         ~ id %in% c(1),
      technik == "purrr"                     ~ id %in% c(1),
      TRUE ~ FALSE
    )
  )

# Welche Hürden wurden genannt?
huerden_typen <- c("Keine Zeit zum Üben", "Fehlende Anwendungsfälle",
                   "Software-Installation", "Zu komplex",
                   "Kolleginnen nutzen Excel", "Datenschutz / IT-Einschränkungen")

huerden <- expand_grid(id = 1:7, huerden_typ = huerden_typen) |>
  mutate(
    genannt = case_when(
      huerden_typ == "Keine Zeit zum Üben"         ~ id %in% c(3,4,6,7),
      huerden_typ == "Fehlende Anwendungsfälle"    ~ id %in% c(3,4,6),
      huerden_typ == "Software-Installation"       ~ id %in% c(4,6),
      huerden_typ == "Zu komplex"                  ~ id %in% c(4,6,7),
      huerden_typ == "Kolleginnen nutzen Excel"    ~ id %in% c(3,4,7),
      huerden_typ == "Datenschutz / IT-Einschränkungen" ~ id %in% c(4,6),
      TRUE ~ FALSE
    )
  )


# === KAPITEL 2: HILFSFUNKTIONEN ===

# Prozentformatierung für Plots und Ausgaben
als_prozent <- function(x, stellen = 1) {
  paste0(round(x * 100, stellen), "%")
}

# Stichprobengrösse für Achsenbeschriftungen
n_responses <- function(x) paste0("n = ", sum(!is.na(x)))

# Einheitliche Farbpalette (Kanton Thurgau Blautöne)
farben <- c("#003366", "#336699", "#6699CC", "#99BBDD", "#BBDDEE")


# === KAPITEL 3: AUSWERTUNG UMFRAGE 1 (SOFORT) ===

dir.create("Feedback/plots", recursive = TRUE, showWarnings = FALSE)

# --- 3a) Kernkennzahlen ---
kennzahlen_u1 <- tibble(
  Kennzahl = c(
    "Durchschnittliche Zufriedenheit (1-5)",
    "Empfehlen 'Ja, auf jeden Fall'",
    "Durchschnittliche Relevanz (1-5)",
    "Ø R-Level vorher",
    "Ø R-Level nachher",
    "Durchschnittlicher Lernfortschritt"
  ),
  Wert = c(
    round(mean(umfrage1$zufriedenheit), 2),
    als_prozent(mean(umfrage1$empfehlung == "Ja, auf jeden Fall")),
    round(mean(umfrage1$relevanz), 2),
    round(mean(umfrage1$level_vorher), 2),
    round(mean(umfrage1$level_nachher), 2),
    paste0("+", round(mean(umfrage1$level_nachher - umfrage1$level_vorher), 2),
           " Stufen")
  )
)
print("=== Kernkennzahlen Umfrage 1 ===")
print(kennzahlen_u1)

# --- 3b) Plot: Zufriedenheit & Erwartungen ---
p_zufr <- umfrage1 |>
  count(zufriedenheit) |>
  ggplot(aes(x = factor(zufriedenheit), y = n)) +
  geom_col(fill = farben[1]) +
  labs(title = "Zufriedenheit", x = "Bewertung (1-5)",
       y = "Anzahl", subtitle = n_responses(umfrage1$zufriedenheit)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

p_erw <- umfrage1 |>
  count(erwartungen) |>
  ggplot(aes(x = factor(erwartungen), y = n)) +
  geom_col(fill = farben[2]) +
  labs(title = "Erwartungen erfüllt", x = "Bewertung (1-5)",
       y = "Anzahl", subtitle = n_responses(umfrage1$erwartungen)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

p01 <- p_zufr + p_erw +
  plot_annotation(title = "Gesamteindruck",
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave("Feedback/plots/p01_zufriedenheit.png", p01,
       width = 10, height = 5, dpi = 150)

# --- 3c) Plot: Lernfortschritt ---
lernfortschritt <- umfrage1 |>
  select(id, level_vorher, level_nachher) |>
  pivot_longer(cols = c(level_vorher, level_nachher),
               names_to = "zeitpunkt", values_to = "level") |>
  mutate(
    zeitpunkt = factor(zeitpunkt,
                       levels = c("level_vorher", "level_nachher"),
                       labels = c("Vor dem Kurs", "Nach dem Kurs"))
  )

p02 <- lernfortschritt |>
  ggplot(aes(x = zeitpunkt, y = level, group = id, color = factor(id))) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = colorRampPalette(farben)(7),
                     guide = guide_legend(title = "Person")) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(title = "Selbsteingeschätzter R-Level: Vorher vs. Nachher",
       subtitle = "Jede Linie = eine teilnehmende Person",
       x = NULL, y = "R-Level (0 = kein, 5 = Experte)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p02_lernfortschritt.png", p02,
       width = 9, height = 6, dpi = 150)

# --- 3d) Plot: Themen – besonders wertvoll ---
p03 <- themen_wertvoll |>
  filter(wertvoll) |>
  count(thema, sort = TRUE) |>
  mutate(thema = fct_reorder(thema, n)) |>
  ggplot(aes(x = n, y = thema)) +
  geom_col(fill = farben[1]) +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  scale_x_continuous(limits = c(0, 8), breaks = 0:7) +
  labs(title = "Welche Themen waren am wertvollsten?",
       subtitle = "Anzahl Nennungen (Mehrfachauswahl möglich)",
       x = "Anzahl Nennungen", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p03_themen_wertvoll.png", p03,
       width = 9, height = 6, dpi = 150)

# --- 3e) Plot: Sitzungsbewertung als Heatmap ---
sitzungen_mean <- sitzungen |>
  group_by(sitzung) |>
  summarise(
    Verständlichkeit = round(mean(verstaendlich), 1),
    Relevanz         = round(mean(relevant), 1)
  ) |>
  pivot_longer(cols = c(Verständlichkeit, Relevanz),
               names_to = "dimension", values_to = "wert")

p04 <- sitzungen_mean |>
  ggplot(aes(x = dimension, y = sitzung, fill = wert)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = wert), color = "white", fontface = "bold", size = 5) +
  scale_fill_gradient(low = farben[4], high = farben[1],
                      limits = c(1, 5), name = "Ø Bewertung") +
  labs(title = "Sitzungsbewertung",
       subtitle = "Durchschnitt aller Teilnehmenden (Skala 1-5)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 12))

ggsave("Feedback/plots/p04_sitzungen_heatmap.png", p04,
       width = 7, height = 6, dpi = 150)


# === KAPITEL 4: AUSWERTUNG UMFRAGE 2 (NACHBEFRAGUNG) ===

# --- 4a) Kernkennzahl R-Nutzung ---
nutzung_tabelle <- umfrage2 |>
  count(nutzung) |>
  mutate(
    anteil = als_prozent(n / sum(n)),
    nutzung_label = case_when(
      nutzung == "regelmaessig" ~ "Regelmässig",
      nutzung == "gelegentlich" ~ "Gelegentlich",
      nutzung == "selten"       ~ "Selten",
      nutzung == "versucht"     ~ "Versucht, aufgehört",
      nutzung == "nein"         ~ "Nutze R nicht"
    )
  )
print("=== R-Nutzung nach 4 Monaten ===")
print(nutzung_tabelle)

n_nutzen_aktiv <- sum(umfrage2$nutzung %in% c("regelmaessig", "gelegentlich"))
cat("Nutzen R aktiv:", n_nutzen_aktiv, "von 7\n")

# --- 4b) Plot: R-Nutzung ---
nutzung_levels <- c("Regelmässig", "Gelegentlich", "Selten",
                     "Versucht, aufgehört", "Nutze R nicht")

p05 <- umfrage2 |>
  mutate(
    nutzung_label = case_when(
      nutzung == "regelmaessig" ~ "Regelmässig",
      nutzung == "gelegentlich" ~ "Gelegentlich",
      nutzung == "selten"       ~ "Selten",
      nutzung == "versucht"     ~ "Versucht, aufgehört",
      nutzung == "nein"         ~ "Nutze R nicht"
    ),
    nutzung_label = factor(nutzung_label, levels = rev(nutzung_levels))
  ) |>
  count(nutzung_label) |>
  ggplot(aes(x = n, y = nutzung_label)) +
  geom_col(fill = farben[1]) +
  geom_text(aes(label = n), hjust = -0.3, size = 4.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "Nutzt du R noch? (4 Monate nach Kursabschluss)",
       subtitle = paste0(n_nutzen_aktiv, " von 7 Teilnehmenden nutzen R aktiv"),
       x = "Anzahl Personen", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p05_r_nutzung.png", p05,
       width = 9, height = 5, dpi = 150)

# --- 4c) Plot: Zeitersparnis ---
zeit_levels <- c("Ich spare keine Zeit", "Weniger als 1 Stunde",
                  "1-3 Stunden", "3-8 Stunden", "Mehr als 8 Stunden")

p06 <- umfrage2 |>
  mutate(zeitersparnis = factor(zeitersparnis, levels = zeit_levels)) |>
  count(zeitersparnis) |>
  complete(zeitersparnis, fill = list(n = 0)) |>
  ggplot(aes(x = n, y = zeitersparnis)) +
  geom_col(fill = farben[2]) +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "Wieviel Zeit sparst du pro Woche dank R?",
       subtitle = n_responses(umfrage2$zeitersparnis),
       x = "Anzahl Personen", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p06_zeitersparnis.png", p06,
       width = 9, height = 5, dpi = 150)

# --- 4d) Plot: Level-Entwicklung über 3 Zeitpunkte ---
level_verlauf <- umfrage1 |>
  select(id, level_vorher, level_nachher) |>
  left_join(umfrage2 |> select(id, level_heute, nutzung), by = "id") |>
  pivot_longer(cols = c(level_vorher, level_nachher, level_heute),
               names_to = "zeitpunkt", values_to = "level") |>
  mutate(
    zeitpunkt = factor(zeitpunkt,
                       levels = c("level_vorher", "level_nachher", "level_heute"),
                       labels = c("Vor dem Kurs", "Nach dem Kurs", "Heute (4 Mon.)")),
    nutzt_r = case_when(
      nutzung %in% c("regelmaessig", "gelegentlich") ~ "Nutzt R aktiv",
      nutzung == "selten" ~ "Nutzt R selten",
      TRUE ~ "Nutzt R nicht"
    )
  )

p07 <- level_verlauf |>
  ggplot(aes(x = zeitpunkt, y = level, group = id, color = nutzt_r)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Nutzt R aktiv" = farben[1],
                                 "Nutzt R selten" = farben[3],
                                 "Nutzt R nicht"  = farben[5]),
                     name = NULL) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(title = "R-Level-Entwicklung: Vor Kurs → Nach Kurs → Heute",
       subtitle = "Verfall bei Nicht-Nutzenden sichtbar",
       x = NULL, y = "Selbsteingeschätzter R-Level (0-5)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("Feedback/plots/p07_level_verlauf.png", p07,
       width = 10, height = 6, dpi = 150)

# --- 4e) Plot: KI-Nutzung ---
n_ki_nutzen <- sum(grepl("^Ja", umfrage2$ki_nutzung))
n_grundwissen_ja <- sum(umfrage2$grundwissen_ki %in% c("Ja, definitiv", "Eher ja"),
                        na.rm = TRUE)

p08 <- umfrage2 |>
  count(ki_nutzung) |>
  mutate(ki_nutzung = fct_reorder(ki_nutzung, n)) |>
  ggplot(aes(x = n, y = ki_nutzung)) +
  geom_col(fill = farben[1]) +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "Nutzt du KI-Tools für R-Code?",
       subtitle = paste0(n_ki_nutzen, " von 7 nutzen KI; ",
                         n_grundwissen_ja, " sagen: Kursgrundwissen hilft beim KI-Einsatz"),
       x = "Anzahl Personen", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p08_ki_nutzung.png", p08,
       width = 9, height = 5, dpi = 150)

# --- 4f) Plot: War der Aufwand gerechtfertigt? Sofort vs. Nachher ---
aufwand_vergleich <- umfrage1 |>
  select(id, sofort = zeitaufwand_ok_num) |>
  left_join(umfrage2 |> select(id, nachher = aufwand_gerechtfertigt), by = "id") |>
  pivot_longer(cols = c(sofort, nachher),
               names_to = "zeitpunkt", values_to = "bewertung") |>
  mutate(
    zeitpunkt = factor(zeitpunkt,
                       levels = c("sofort", "nachher"),
                       labels = c("Direkt nach Kurs", "Nach 4 Monaten"))
  )

p09 <- aufwand_vergleich |>
  ggplot(aes(x = zeitpunkt, y = bewertung, group = id, color = factor(id))) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3.5) +
  scale_color_manual(values = colorRampPalette(farben)(7),
                     guide = guide_legend(title = "Person")) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5,
                     labels = c("1\nNein", "2", "3\nTeils", "4", "5\nJa")) +
  labs(title = "War der Zeitaufwand gerechtfertigt?",
       subtitle = "Meinungsveränderung über Zeit",
       x = NULL, y = "Bewertung (1 = Nein, 5 = Ja)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p09_aufwand_vergleich.png", p09,
       width = 9, height = 6, dpi = 150)

# --- 4g) Plot: Welche Techniken werden tatsächlich genutzt? ---
p10 <- techniken_genutzt |>
  filter(genutzt) |>
  count(technik, sort = TRUE) |>
  mutate(technik = fct_reorder(technik, n)) |>
  ggplot(aes(x = n, y = technik)) +
  geom_col(fill = farben[1]) +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  scale_x_continuous(limits = c(0, 8)) +
  labs(title = "Welche Techniken werden im Arbeitsalltag eingesetzt?",
       subtitle = "Nur Personen die R nutzen; Mehrfachauswahl möglich",
       x = "Anzahl Personen", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Feedback/plots/p10_techniken_genutzt.png", p10,
       width = 10, height = 6, dpi = 150)


# === KAPITEL 5: EXCEL-EXPORT ===

# Schlüsselkennzahlen berechnen
n_planen_r     <- sum(umfrage1$r_vorstellen >= 4)
n_nutzen_aktiv <- sum(umfrage2$nutzung %in% c("regelmaessig", "gelegentlich"))
n_ki_nutzen    <- sum(grepl("^Ja", umfrage2$ki_nutzung))

zeit_median <- umfrage2 |>
  mutate(zeit_num = case_when(
    zeitersparnis == "Ich spare keine Zeit"  ~ 0,
    zeitersparnis == "Weniger als 1 Stunde"  ~ 0.5,
    zeitersparnis == "1-3 Stunden"           ~ 2,
    zeitersparnis == "3-8 Stunden"           ~ 5.5,
    zeitersparnis == "Mehr als 8 Stunden"    ~ 10,
    TRUE ~ NA_real_
  )) |>
  pull(zeit_num) |>
  median(na.rm = TRUE)

kennzahlen_gesamt <- tibble(
  Kennzahl = c(
    "Teilnehmende gesamt",
    "Planen R zu nutzen (direkt nach Kurs)",
    "Nutzen R aktiv (nach 4 Monaten)",
    "Median Zeitersparnis pro Woche (Std.)",
    "Aufwand gerechtfertigt Ø (direkt, 1-5)",
    "Aufwand gerechtfertigt Ø (nach 4 Mon., 1-5)",
    "KI-Tools für R genutzt"
  ),
  Wert = c(
    "7",
    paste0(n_planen_r, " / 7"),
    paste0(n_nutzen_aktiv, " / 7"),
    as.character(zeit_median),
    as.character(round(mean(umfrage1$zeitaufwand_ok_num), 1)),
    as.character(round(mean(umfrage2$aufwand_gerechtfertigt), 1)),
    paste0(n_ki_nutzen, " / 7")
  )
)

# Workbook erstellen
wb <- createWorkbook()

# Styles
style_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#003366",
  fontName = "Arial", fontSize = 11, textDecoration = "Bold",
  halign = "LEFT", wrapText = FALSE
)
style_bold <- createStyle(textDecoration = "Bold", fontName = "Arial")
style_data  <- createStyle(fontName = "Arial", fontSize = 10)

# Sheet 1: Übersicht
addWorksheet(wb, "Übersicht")
writeData(wb, "Übersicht", kennzahlen_gesamt, startRow = 1)
addStyle(wb, "Übersicht", style_header, rows = 1, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Übersicht", style_data, rows = 2:9, cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Übersicht", cols = 1:2, widths = c(45, 25))

# Plot einfügen wenn vorhanden
if (file.exists("Feedback/plots/p05_r_nutzung.png")) {
  insertImage(wb, "Übersicht", file = "Feedback/plots/p05_r_nutzung.png",
              startRow = 12, startCol = 1, width = 8, height = 4, units = "in")
}

# Sheet 2: Umfrage 1 Rohdaten
addWorksheet(wb, "Umfrage 1")
writeData(wb, "Umfrage 1", umfrage1, startRow = 1)
addStyle(wb, "Umfrage 1", style_header, rows = 1, cols = 1:ncol(umfrage1),
         gridExpand = TRUE)
setColWidths(wb, "Umfrage 1", cols = 1:ncol(umfrage1), widths = "auto")

# Sheet 3: Umfrage 2 Rohdaten
addWorksheet(wb, "Umfrage 2")
writeData(wb, "Umfrage 2", umfrage2, startRow = 1)
addStyle(wb, "Umfrage 2", style_header, rows = 1, cols = 1:ncol(umfrage2),
         gridExpand = TRUE)
setColWidths(wb, "Umfrage 2", cols = 1:ncol(umfrage2), widths = "auto")

saveWorkbook(wb, "Feedback/Ergebnisse_Feedback.xlsx", overwrite = TRUE)
cat("Excel gespeichert: Feedback/Ergebnisse_Feedback.xlsx\n")


# === KAPITEL 6: ZUSAMMENFASSUNG FÜR DIE PRÄSENTATION ===

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║        ZUSAMMENFASSUNG – R-WORKSHOP FEEDBACK                ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("── FORSCHUNGSFRAGE 1: Wird R nach dem Kurs wirklich genutzt? ──\n")
cat(paste0("   → ", n_nutzen_aktiv, " von 7 nutzen R aktiv nach 4 Monaten (",
           als_prozent(n_nutzen_aktiv/7), ")\n"))
cat(paste0("   → ", n_planen_r, " von 7 planen direkt nach Kurs R zu nutzen\n"))
cat("   → Verfall: Level sinkt bei Nicht-Nutzenden (siehe p07)\n\n")

cat("── FORSCHUNGSFRAGE 2: Rechtfertigt R den Lernaufwand? ──\n")
cat(paste0("   → Aufwand gerechtfertigt (Ø direkt):   ",
           round(mean(umfrage1$zeitaufwand_ok_num), 1), " / 5\n"))
cat(paste0("   → Aufwand gerechtfertigt (Ø 4 Monate): ",
           round(mean(umfrage2$aufwand_gerechtfertigt), 1), " / 5\n"))
cat(paste0("   → Mediane Zeitersparnis: ", zeit_median, " Stunden/Woche\n\n"))

cat("── FORSCHUNGSFRAGE 3: Rolle von KI-Tools ──\n")
cat(paste0("   → ", n_ki_nutzen, " von 7 nutzen KI-Tools für R\n"))
cat(paste0("   → ", n_grundwissen_ja, " von 7 sagen: Kursgrundwissen hilft,\n"))
cat("      KI-generierten R-Code zu verstehen und zu prüfen\n\n")

cat("── UNBEQUEME FRAGEN FÜR DIE DISKUSSION ──\n")
cat("   ? Wie viele nutzen R wirklich nach 4 Monaten? Ist das genug?\n")
cat("   ? Rechtfertigt die Zeitersparnis den Kursaufwand (21h)?\n")
cat("   ? Ist KI ein Ersatz oder Ergänzung zum R-Grundwissen?\n")
cat("   ? Was passiert mit dem Wissen ohne regelmässige Anwendung?\n\n")

cat("✓ Auswertung abgeschlossen.\n")
cat("  Alle Plots: Feedback/plots/\n")
cat("  Excel:      Feedback/Ergebnisse_Feedback.xlsx\n")
