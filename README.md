# R Workshop – Übersicht

## Einführung

Der Workshop wurde im Amt für Daten und Statistik des Kantons Thurgau durchgeführt.
Er umfasste insgesamt **13 Sessions à ca. 3 Stunden** über einen Zeitraum von **2 Jahren**.
Die Themen wuchsen dabei organisch – Wünsche und Vorschläge von Kolleginnen und Kollegen wurden aufgenommen und behandelt.
Ziel war es, **alltägliches Programmieren in R** zu ermöglichen – pragmatisch und praxisnah.
Sicher gibt es Stellen, an denen man Dinge effizienter oder „schöner“ lösen könnte, aber die vermittelten Methoden sind im Arbeitsalltag erprobt und funktionieren zuverlässig.

Es ist ausserdem zu beachten, dass der Ursprung des Workshop in einer Zeit VOR KI liegt. Es sind daher keine Hinweise auf die Verwendung von KI vorhanden, was aber für den Lerneffekt eher förderlich sein sollte.

Einige wenige Aufgaben und RMarkdown FIles (`dataspot`) können nur innerhalb des kantonalen Netzwerks des Kanton Thuragus ausgeführt werden.

---

## Intro

* **Ziele**: Programmier-Mindset entwickeln, Überblick über R, Praxisbeispiele aus der Statistik.
* **Themen**: R & RStudio, R Projects, Objekte & Funktionen, Datenstrukturen, Packages.

## Mindset

* **Kernideen**: Probleme sind selten neu, gezieltes Googeln, Automatisierung spart Zeit, viele Wege führen zum Ziel.
* **Lernansatz**: Programmieren lernt man nur durch praktische Anwendung.

## Tidyverse

* **Definition**: Paketfamilie mit gemeinsamer Grammatik, basierend auf *tidy data*.
* **Zweck**: Einheitliche Tools für Import, Transformation, Visualisierung, Modellierung.
* **Pakete**: `readr`, `readxl`, `haven`, `dplyr`, `tidyr`, `stringr`, `lubridate`, `ggplot2` u.a.

## Daten einlesen

* **Formate**: CSV, Excel, SAS/SPSS/Stata, RDS/RDA, Fixed Width Files.
* **Best Practices**: Passendes Paket wählen, `list.files()` für Batch-Import, spezielle Strategien für große Dateien.

## Tidy Data

* **Prinzip**: Spalte = Variable, Zeile = Beobachtung, Zelle = Wert.
* **Vorteile**: Konsistente Struktur, Wiederverwendbarkeit.
* **Hinweis**: Nur aufräumen, wenn es die Analyse unterstützt.

## tidyr

* **Zweck**: Daten ins *tidy* Format bringen.
* **Funktionen**: `pivot_longer`, `pivot_wider`, `separate`, `unite`.
* **Anwendung**: Formatwechsel, Spalten trennen/verbinden.

## magrittr

* **Zweck**: `%>%` für klare, lesbare Funktionsketten.
* **Vorteile**: Weniger Klammern, einfacher zu lesen.
* **Extras**: Platzhalter `.` für gezieltes Einfügen.

## lubridate

* **Zweck**: Datum/Zeit einfacher einlesen und verarbeiten.
* **Funktionen**: `ymd()`, `dmy()`, `ymd_hms()`, `round_date()`, `year()`, `month()`.

## stringr

* **Zweck**: Einheitliche String-Verarbeitung.
* **Funktionen**: `str_detect`, `str_extract`, `str_replace`, `str_split`.
* **Extras**: Regex-Unterstützung, für mehr Funktionen `stringi`.

## dplyr

* **Zweck**: Datenmanipulation mit klarer Syntax.
* **Funktionen**: `select()`, `filter()`, `mutate()`, `group_by()` + `summarise()`, `join()`.
* **Besonderheit**: Optimal mit `%>%` nutzbar.

## Loops & Apply

* **Loops**: `for`, `while`, mit `break`/`next` steuern.
* **apply-Familie**: Kürzere Iterationen (`lapply`, `sapply` etc.).
* **purrr**: `map`-Funktionen für funktionale Pipelines.

## Conditional Statements

* **Vektorisiert**: `ifelse()` für einfache Bedingungen.
* **Lesbar**: `case_when()` für mehrere Fälle.
* **Klassisch**: `if...else` in Loops und Funktionen.

## Listen

* **Merkmal**: Beliebige, auch gemischte Datentypen.
* **Zugriff**: `[[ ]]`, `[ ]`, `$`.
* **Bearbeitung**: Elemente hinzufügen, kombinieren, in Vektor/Data Frame umwandeln.

## ggplot2

* **Ziel**: Visualisierung nach *Grammar of Graphics*.
* **Aufbau**: `data` + `aes()` + `geom_*()`.
* **Praxis**: Scatterplots, Balken-/Liniendiagramme, Missing-Value-Visualisierung.

## Funktionen

* **Zweck**: Wiederverwendbarer, sauberer Code.
* **Aufbau**: `formals` (Argumente), `body` (Code), `environment` (Kontext).
* **Konzepte**: Name Masking, Lazy Evaluation, Default-Argumente.
* **Best Practices**: Sprechende Namen, konsistente Schreibweise, sinnvolle Kommentare.
* **Conditions**: `stop()`, `warning()`, `message()`.

## Daten in Excel schreiben

* **openxlsx**: XLSX-Dateien ohne Excel erstellen/formatieren.
* **TGexcel**: Standardisierte Internettabellen im Corporate Design.

## CSV: Kodierung & Trennzeichen

* **Probleme**: Unterschiedliche Encodings & Delimiter.
* **Lösungen**: `locale(encoding=)`, `read_delim()`, `readr` für Performance.
* **Best Practice**: Immer RDS für interne Speicherung.

## Arbeiten mit der Datenbank

* **Verbindung**: `DBI` + `odbc()` (intern) oder `RSQLite` (lokal).
* **Erkunden**: `dbListTables()`, `dbListFields()`.
* **tidyverse + DB**: `tbl()`-Pointer, dplyr-Verbund → SQL, `collect()` lädt Daten.
* **Praxis**: Joins mit DB-Tabellen & lokalen Frames.

## REST APIs in R

* **Prinzip**: Kommunikation über HTTP (`GET`, `POST`, `PUT`, `DELETE`).
* **Antworten**: Statuscodes, Body (JSON), Headers.
* **httr2**: Moderner Workflow (`request()`, `req_url_query()`, `req_auth_basic()`, `resp_body_json()`).
* **Beispiele**: Offene API (`data.tg.ch`), Authentifizierung (`dataspot`).
* **Beispiele mit Authentifizierung können nur innerhalb des kantonalen Netzwerks des Kanton Thurgaus ausgeführt werden**

* **Extras**: Umgebungsvariablen für Credentials, `req_dry_run()` für Tests.

