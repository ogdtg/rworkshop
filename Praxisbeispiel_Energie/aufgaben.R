# ════════════════════════════════════════════════════════════════
# PRAXISAUFGABE: Energiewende im Thurgau
# Quelle: data.tg.ch
# Benötigte Packages: tidyverse, ggplot2, ggrepel
# R-Workshop – Amt für Daten und Statistik, Kanton Thurgau
# ════════════════════════════════════════════════════════════════
#
# Ausgangslage: Das Amt für Daten und Statistik stellt Energiedaten aller
# Thurgauer Gemeinden als Open Data auf data.tg.ch zur Verfügung. Deine
# Aufgabe: Analysiere die Energiewende im Thurgau – von der Produktion
# erneuerbarer Energie bis zum Eigenversorgungsgrad der Gemeinden.

library(tidyverse)
library(ggplot2)
library(ggrepel)  # für beschriftete Streudiagramme

# URLs für die vier Datensätze
url_a <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-10/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_b <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-5/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_c <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-4/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"
url_d <- "https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-12/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich"


# ════════════════════════════════════════════════════════════════
# Aufgabe 1: Daten laden und kennenlernen (~15 Min.) ----
# ════════════════════════════════════════════════════════════════
# Wir laden alle vier Datensätze direkt von data.tg.ch. Die Daten sind
# bereits vorgeladen – deine Aufgabe ist es, sie mit glimpse(), count()
# und summary() zu erkunden.

prod_erneuerbar    <- read_csv2(url_a)  # div-energie-10: erneuerbare Stromproduktion
verbrauch_gemeinde <- read_csv2(url_b)  # div-energie-5:  Verbrauch nach Gemeinde
verbrauch_kanton   <- read_csv2(url_c)  # div-energie-4:  Verbrauch Kanton gesamt
heizsysteme        <- read_csv2(url_d)  # div-energie-12: Heizsystembestand

# Tipp: read_csv2() erwartet Semikolon als Trennzeichen – das ist der Standard
# auf Schweizer Open-Data-Portalen. read_csv() würde das Komma erwarten und
# die Daten falsch einlesen.

# --- 1a) Erste Orientierung: Struktur der Datensätze ansehen ---
glimpse(___)           # prod_erneuerbar ansehen
glimpse(___)           # verbrauch_gemeinde ansehen
glimpse(___)           # verbrauch_kanton ansehen
glimpse(___)           # heizsysteme ansehen

# Tipp: glimpse() zeigt Spaltentypen und die ersten Werte in kompakter Form.

# --- 1b) Welche Jahre sind im Datensatz enthalten? ---
prod_erneuerbar    |> count(___)   # nach Jahr zählen
verbrauch_gemeinde |> count(___)

# --- 1c) Wie viele Gemeinden enthält der Verbrauchsdatensatz? ---
verbrauch_gemeinde |> distinct(___) |> nrow()

# Tipp: distinct() gibt eindeutige Werte zurück; nrow() zählt die Zeilen.

# --- 1d) Welche Energieträger kommen vor? ---
prod_erneuerbar    |> distinct(___)   # Energieträger in Datensatz A
verbrauch_gemeinde |> distinct(___)   # Energieträger in Datensatz B

# --- 1e) Statistische Kennzahlen für die Produktionsdaten ---
summary(___)

# Hinweis: Spaltennamen können je nach CSV-Export leicht abweichen
# (z.B. Grossschreibung). Bei Abweichungen rename_with(tolower) verwenden.


# ════════════════════════════════════════════════════════════════
# Aufgabe 2: Bereinigung und Kategorisierung (~15 Min.) ----
# ════════════════════════════════════════════════════════════════
# Rohdaten enthalten oft unsichtbare Leerzeichen in Textspalten. Diese
# können später beim Filtern oder Joinen zu stillen Fehlern führen.

# --- 2a) Fehlende Werte prüfen ---
# Wieviele NAs gibt es pro Spalte in jedem Datensatz?
prod_erneuerbar    |> summarise(across(everything(), \(x) sum(is.na(x))))
verbrauch_gemeinde |> summarise(across(everything(), \(x) sum(is.na(x))))
verbrauch_kanton   |> summarise(across(everything(), \(x) sum(is.na(x))))
heizsysteme        |> summarise(across(everything(), \(x) sum(is.na(x))))

# Tipp: across(everything(), ...) wendet eine Funktion auf alle Spalten an.
# \(x) ist eine anonyme Lambda-Funktion (ab R 4.1).

# --- 2b) Leerzeichen in allen Textspalten entfernen ---
# str_trim() schneidet führende und abschliessende Leerzeichen ab.
prod_erneuerbar    <- prod_erneuerbar    |> mutate(across(where(is.character), ___))
verbrauch_gemeinde <- verbrauch_gemeinde |> mutate(across(where(is.character), ___))
verbrauch_kanton   <- verbrauch_kanton   |> mutate(across(where(is.character), ___))
heizsysteme        <- heizsysteme        |> mutate(across(where(is.character), ___))

# Tipp: str_trim() aus dem stringr-Package (Teil von tidyverse).
# where(is.character) wählt nur Textspalten aus.

# --- 2c) Neue Spalte `kategorie` anlegen (fossil / erneuerbar / sonstige) ---
# case_when() weist jeder Zeile eine Kategorie zu – ähnlich wie ein
# geschachteltes if/else, aber lesbarer und vektorisiert.
verbrauch_gemeinde <- verbrauch_gemeinde |>
  mutate(
    kategorie = case_when(
      energietraeger %in% c(___, ___)                               ~ "Fossil",
      energietraeger %in% c(___, ___, ___, ___)                     ~ "Erneuerbar",
      .default = ___
    )
  )

# Dieselbe Kategorisierung für die Kantonsebene
verbrauch_kanton <- verbrauch_kanton |>
  mutate(
    kategorie = case_when(
      energietraeger %in% c(___, ___)                               ~ "Fossil",
      energietraeger %in% c(___, ___, ___, ___)                     ~ "Erneuerbar",
      .default = ___
    )
  )

# Tipp: Die fossil/erneuerbar-Klassifikation findest du im daten_schema.md.
# %in% prüft, ob ein Wert in einem Vektor enthalten ist.

# --- 2d) Kontrolle: Alle Energieträger korrekt zugeordnet? ---
verbrauch_gemeinde |> count(___, ___)   # Energieträger und Kategorie gemeinsam zählen
verbrauch_kanton   |> count(___, ___)

# Hinweis: Wenn ein Energieträger unerwartet in "Sonstige" landet, stimmt
# die Schreibweise möglicherweise nicht überein – str_trim() hilft oft.


# ════════════════════════════════════════════════════════════════
# Aufgabe 3: Entwicklung der erneuerbaren Stromproduktion (~20 Min.) ----
# ════════════════════════════════════════════════════════════════
# Wir aggregieren die Gemeindedaten auf Kantonsebene und visualisieren
# den Ausbau der verschiedenen erneuerbaren Energieträger über die Zeit.

# --- 3a) Produktion auf Kantonsebene aggregieren ---
# Summe über alle Gemeinden pro Jahr und Energieträger
prod_kanton <- prod_erneuerbar |>
  group_by(___, ___) |>                                    # nach Jahr und Energieträger
  summarise(produktion_mwh = sum(___, na.rm = TRUE),
            .groups = "drop")

# Tipp: .groups = "drop" hebt die Gruppierung nach summarise() auf,
# damit folgende Operationen nicht unbeabsichtigt gruppiert laufen.

# --- 3b) Liniengrafik: Entwicklung je Energieträger ---
prod_kanton |>
  ggplot(aes(x = ___, y = produktion_mwh / 1000, color = ___)) +  # x = Jahr, color = Energieträger
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::label_number(suffix = " GWh")) +
  labs(
    title   = "Erneuerbare Stromproduktion Kanton Thurgau",
    subtitle = "Entwicklung nach Energieträger",
    x       = NULL,
    y       = "Produktion (GWh)",
    color   = "Energieträger",
    caption = "Quelle: data.tg.ch (div-energie-10)"
  ) +
  theme_minimal(base_size = 12)

# Tipp: geom_line() verbindet Punkte in der Reihenfolge der x-Achse.
# Divide by 1000 converts MWh to GWh for more readable axis labels.

# --- 3c) Gestapeltes Flächendiagramm: Zusammensetzung über die Zeit ---
# geom_area() zeigt nicht nur den Trend, sondern auch den relativen Anteil
# der einzelnen Energieträger am Gesamtmix.
prod_kanton |>
  ggplot(aes(x = ___, y = produktion_mwh / 1000, fill = ___)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
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

# Tipp: Bei geom_area() fill= statt color= verwenden für die Füllung.

# --- 3d) Top 5 Gemeinden nach Gesamtproduktion (aktuellstes Jahr) ---
# Wir beschränken uns auf das neueste Jahr, damit die Zahlen vergleichbar sind.
letztes_jahr_prod <- max(prod_erneuerbar$___)   # aktuellstes Jahr bestimmen

top5_gemeinden <- prod_erneuerbar |>
  filter(jahr == ___) |>                                   # nur aktuellstes Jahr
  group_by(___, ___) |>                                    # nach Gemeinde-Nr. und Name
  summarise(produktion_mwh = sum(___, na.rm = TRUE),
            .groups = "drop") |>
  slice_max(___, n = 5)                                    # die 5 grössten auswählen

print(top5_gemeinden)

# Tipp: slice_max(spalte, n = 5) wählt die n Zeilen mit den grössten Werten.
# Vorteil gegenüber arrange() + head(): funktioniert auch in gruppierten Daten.

# Hinweis: Gemeinden ohne Produktionsdaten im neuesten Jahr tauchen hier
# nicht auf – das ist korrekt, kein Datenfehler.


# ════════════════════════════════════════════════════════════════
# Aufgabe 4: Wärmeverbrauch – fossil vs. erneuerbar (~20 Min.) ----
# ════════════════════════════════════════════════════════════════
# Wir nutzen den kantonalen Verbrauchsdatensatz (C) mit der in Aufgabe 2
# erstellten Kategorie-Spalte, um den Wandel im Heizenergiemix zu zeigen.

# Voraussetzung: verbrauch_kanton hat bereits die Spalte `kategorie`
# (erstellt in Aufgabe 2c). Falls nicht: Aufgabe 2c zuerst ausführen.

# --- 4a) Verbrauch nach Kategorie und Jahr aggregieren ---
verbrauch_kat <- verbrauch_kanton |>
  group_by(___, ___) |>                                    # nach Jahr und Kategorie
  summarise(verbrauch_mwh = sum(___, na.rm = TRUE),
            .groups = "drop")

# Tipp: Mehrere Gruppierungsvariablen in group_by() mit Komma trennen.

# --- 4b) Prozentualen Anteil pro Jahr berechnen ---
# mutate() innerhalb von group_by() bezieht sich auf die Gruppe, nicht
# die gesamte Tabelle – so ergibt sum(verbrauch_mwh) die Jahressumme.
anteil_erneuerbar <- verbrauch_kat |>
  group_by(___) |>                                         # pro Jahr gruppieren
  mutate(anteil_pct = ___ / sum(___) * 100) |>            # Anteil in Prozent
  ungroup()

# Kontrolle: Was ist der aktuelle Anteil erneuerbarer Wärme?
anteil_erneuerbar |>
  filter(jahr == max(___), kategorie == "Erneuerbar") |>
  pull(anteil_pct) |>
  round(1)

# Tipp: pull() extrahiert eine Spalte als Vektor – praktisch für
# einzelne Kennzahlen, die man z.B. in Text einbauen möchte.

# --- 4c) Gestapeltes Säulendiagramm: absoluter Verbrauch nach Kategorie ---
verbrauch_kat |>
  ggplot(aes(x = ___, y = verbrauch_mwh / 1000, fill = ___)) +
  geom_col(position = "stack") +
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

# Tipp: geom_col() erwartet bereits aggregierte Daten (im Gegensatz zu
# geom_bar(), das selbst zählt). position = "stack" stapelt die Balken.

# --- 4d) Liniengrafik: Anteilsentwicklung erneuerbar über Zeit ---
# Nur die Kategorie "Erneuerbar" filtern und den Trend darstellen.
anteil_erneuerbar |>
  filter(kategorie == ___) |>
  ggplot(aes(x = ___, y = ___)) +
  geom_line(linewidth = 1.2, color = "#27AE60") +
  geom_point(size = 3, color = "#27AE60") +
  # y-Achse bei 0 verankern, damit Fortschritte nicht übertrieben wirken
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title    = "Anteil erneuerbarer Wärme am Gebäudeverbrauch",
    subtitle = "Kanton Thurgau",
    x        = NULL,
    y        = "Anteil erneuerbar (%)",
    caption  = "Quelle: data.tg.ch (div-energie-4)"
  ) +
  theme_minimal(base_size = 12)

# Tipp: label_percent(scale = 1) erwartet Werte in Prozent (z.B. 25),
# nicht als Anteil (z.B. 0.25). Passe scale entsprechend an.

# Hinweis: Falls der Anteil über die Jahre sinkt, könnte dies an einem
# Basisjahr-Effekt liegen – nicht zwingend ein Fehler in den Daten.


# ════════════════════════════════════════════════════════════════
# Aufgabe 5: Eigenversorgungsgrad der Gemeinden (~20 Min.) ----
# ════════════════════════════════════════════════════════════════
# Der Eigenversorgungsgrad zeigt, wie viel Prozent des Gebäudeverbrauchs
# einer Gemeinde durch lokale erneuerbare Stromproduktion gedeckt werden.
# Eigenversorgungsgrad = Produktion / Verbrauch * 100

# Letztes gemeinsames Jahr bestimmen, damit beide Datensätze vergleichbar sind
letztes_jahr <- min(max(prod_erneuerbar$___), max(verbrauch_gemeinde$___))
cat("Aktuellstes gemeinsames Jahr:", letztes_jahr, "\n")

# --- 5a) Erneuerbare Stromproduktion pro Gemeinde aggregieren ---
prod_gem <- prod_erneuerbar |>
  filter(jahr == ___) |>
  group_by(___, ___) |>                                    # nach gemeinde_nr und gemeinde
  summarise(produktion_mwh = sum(___, na.rm = TRUE),
            .groups = "drop")

# --- 5b) Gesamten Gebäudeverbrauch pro Gemeinde aggregieren ---
# Alle Energieträger zusammenfassen – wir wollen den Gesamtverbrauch
# pro Gemeinde, unabhängig vom Träger.
verbrauch_gem <- verbrauch_gemeinde |>
  filter(jahr == ___) |>
  group_by(___, ___) |>                                    # nach gemeinde_nr und gemeinde
  summarise(verbrauch_mwh = sum(___, na.rm = TRUE),
            .groups = "drop")

# --- 5c) Datensätze verknüpfen und Eigenversorgungsgrad berechnen ---
# left_join behält alle Gemeinden aus prod_gem, auch wenn kein Verbrauch vorliegt.
eigenversorgung <- prod_gem |>
  left_join(___, by = c(___, ___)) |>                      # mit verbrauch_gem joinen
  mutate(eigenversorgungsgrad = ___ / ___ * 100) |>        # Grad berechnen
  filter(!is.na(___))                                      # unvollständige Fälle entfernen

# Tipp: left_join(x, y, by = c("schluessel1", "schluessel2")) verknüpft
# über mehrere Schlüsselspalten. gemeinde_nr ist stabiler als gemeinde.

# Hinweis: Gemeinden, für die nur Produktions- aber keine Verbrauchsdaten
# vorliegen (oder umgekehrt), erhalten NA beim Eigenversorgungsgrad.

# --- 5d) Top 10 Gemeinden nach Eigenversorgungsgrad ---
eigenversorgung |>
  arrange(desc(___)) |>
  slice_head(n = 10) |>
  select(gemeinde, produktion_mwh, verbrauch_mwh, eigenversorgungsgrad) |>
  mutate(across(where(is.numeric), round, 1))

# --- 5e) Streudiagramm: Produktion vs. Verbrauch mit Gemeindebeschriftung ---
# Die Diagonale (slope = 1) markiert den 100%-Eigenversorgungspunkt:
# Punkte oberhalb decken mehr als ihren Verbrauch durch Eigenproduktion.
eigenversorgung |>
  ggplot(aes(
    x     = ___ / 1000,                                    # Verbrauch in GWh
    y     = ___ / 1000,                                    # Produktion in GWh
    label = ___                                            # Gemeindename
  )) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50",
              linewidth = 0.8) +                           # 100%-Linie
  geom_point(aes(color = eigenversorgungsgrad > 100),
             size = 2.5, alpha = 0.8) +
  ggrepel::geom_label_repel(
    data        = \(d) filter(d, eigenversorgungsgrad > ___ |
                                produktion_mwh > quantile(produktion_mwh, 0.85)),
    size        = 3,
    max.overlaps = 15,
    box.padding = 0.4
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
    subtitle = paste0("Erneuerbare Stromproduktion vs. Gebäudeverbrauch (", letztes_jahr, ")"),
    x        = "Gebäudeverbrauch (GWh)",
    y        = "Erneuerbare Stromproduktion (GWh)",
    caption  = "Strichlinie = 100% Eigenversorgung | Quelle: data.tg.ch"
  ) +
  theme_minimal(base_size = 12)

# Tipp: ggrepel::geom_label_repel() beschriftet Punkte automatisch ohne
# Überlappungen. data = \(d) filter(d, ...) beschriftet nur eine Teilmenge.
# max.overlaps steuert, wie viele Überschneidungen toleriert werden.


# ════════════════════════════════════════════════════════════════
# Aufgabe 6 (Bonus): Parametrisierter Quarto-Bericht (~20 Min.) ----
# ════════════════════════════════════════════════════════════════
# Ein Quarto-Dokument kann für verschiedene Gemeinden automatisch
# gerendert werden – dank params im YAML-Header. Öffne zuerst
# gemeinde_bericht.qmd und schaue dir den params-Abschnitt an.

# Wie sieht der params-Block in gemeinde_bericht.qmd aus?
# ---
# params:
#   gemeinde: "Frauenfeld"   # Standardwert; wird beim Rendern überschrieben
# ---

# --- 6a) Liste der Gemeinden für die Berichte festlegen ---
# Wir beschränken uns auf fünf exemplarische Gemeinden.
ziel_gemeinden <- c("Frauenfeld", "Kreuzlingen", "Arbon", "Weinfelden", "Amriswil")

# --- 6b) Ausgabeordner anlegen ---
dir.create(___, recursive = TRUE, showWarnings = FALSE)   # "Praxisbeispiel_Energie/berichte/"

# Tipp: showWarnings = FALSE unterdrückt die Warnung, falls der Ordner
# bereits existiert. recursive = TRUE legt auch übergeordnete Ordner an.

# --- 6c) Berichte für alle Zielgemeinden rendern ---
# walk() statt map(), weil wir Seiteneffekte (Dateien schreiben) wollen
# und keinen Rückgabewert benötigen.
purrr::walk(___, function(gem) {              # über ziel_gemeinden iterieren
  quarto::quarto_render(
    input          = ___,                     # Pfad zu gemeinde_bericht.qmd
    output_file    = paste0(
      "Praxisbeispiel_Energie/berichte/Energiebericht_",
      str_replace_all(gem, "[^A-Za-z0-9äöüÄÖÜ]", "_"),
      ".html"
    ),
    execute_params = list(gemeinde = ___)     # Parameter übergeben
  )
  cat("Bericht erstellt:", gem, "\n")
})

# Tipp: quarto::quarto_render() kennt u.a. folgende Argumente:
#   input          – Pfad zur .qmd-Datei
#   output_file    – Dateiname (ohne Verzeichnis; output_dir separat möglich)
#   execute_params – benannte Liste der params-Werte
#
# Hinweis: Das Package quarto muss installiert sein: install.packages("quarto")
# Ausserdem muss Quarto CLI auf dem Rechner verfügbar sein (quarto.org).
