# Datenschema: Energiewende im Thurgau
## Quelle: data.tg.ch

Die folgenden vier Datensätze stammen vom Open-Data-Portal des Kantons Thurgau (data.tg.ch) und bilden die Grundlage für die Praxisaufgabe. Alle Datensätze werden als CSV mit Semikolon-Trennzeichen bereitgestellt und können direkt über die angegebenen URLs in R eingelesen werden.

---

## Datensatz A: Erneuerbare Elektrizitätsproduktion (div-energie-10)

**Beschreibung:** Jährliche Produktion von erneuerbarem Strom pro Gemeinde und Energieträger (Sonne, Wasser, Biomasse/Biogas, Wind) in Megawattstunden. Ermöglicht Aussagen über den Ausbau erneuerbarer Stromerzeugung auf Gemeindeebene.

**Download-URL:**
```
https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-10/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich
```

**Spalten:**

| Spalte | Typ | Beschreibung |
|--------|-----|--------------|
| `gemeinde_nr` | int | Gemeindenummer (BFS-Nummer) |
| `gemeinde` | chr | Gemeindename |
| `jahr` | int | Erhebungsjahr |
| `energietraeger` | chr | Art des Energieträgers |
| `produktion_mwh` | dbl | Produzierte Energie in Megawattstunden |

**Mögliche Werte `energietraeger`:** `"Sonne"`, `"Wasser"`, `"Biomasse/Biogas"`, `"Wind"`

**Beispieldaten:**

| gemeinde_nr | gemeinde | jahr | energietraeger | produktion_mwh |
|-------------|----------|------|----------------|----------------|
| 4566 | Frauenfeld | 2018 | Sonne | 8432.5 |
| 4566 | Frauenfeld | 2020 | Sonne | 12185.0 |
| 4671 | Kreuzlingen | 2019 | Wasser | 31200.0 |
| 4671 | Kreuzlingen | 2021 | Sonne | 9870.3 |
| 4806 | Arbon | 2022 | Biomasse/Biogas | 4510.0 |

---

## Datensatz B: Endenergieverbrauch Gebäude nach Gemeinden (div-energie-5)

**Beschreibung:** Jährlicher Endenergieverbrauch von Gebäuden pro Gemeinde und Energieträger in Megawattstunden. Enthält fossile Energieträger (Heizöl, Erdgas), erneuerbare Wärmequellen (Holz, Wärmepumpe, Fernwärme, Sonne thermisch) sowie weitere Kategorien.

**Download-URL:**
```
https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-5/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich
```

**Spalten:**

| Spalte | Typ | Beschreibung |
|--------|-----|--------------|
| `gemeinde_nr` | int | Gemeindenummer (BFS-Nummer) |
| `gemeinde` | chr | Gemeindename |
| `jahr` | int | Erhebungsjahr |
| `energietraeger` | chr | Art des Energieträgers |
| `verbrauch_mwh` | dbl | Verbrauchte Energie in Megawattstunden |

**Mögliche Werte `energietraeger`:** `"Heizöl"`, `"Erdgas"`, `"Holz"`, `"Wärmepumpe"`, `"Fernwärme"`, `"Elektrizität"`, `"Sonne thermisch"`, `"Übrige"`

**Beispieldaten:**

| gemeinde_nr | gemeinde | jahr | energietraeger | verbrauch_mwh |
|-------------|----------|------|----------------|---------------|
| 4566 | Frauenfeld | 2019 | Heizöl | 145300.0 |
| 4566 | Frauenfeld | 2021 | Wärmepumpe | 22450.5 |
| 4671 | Kreuzlingen | 2020 | Erdgas | 98760.0 |
| 4806 | Arbon | 2022 | Holz | 11230.0 |
| 4931 | Weinfelden | 2022 | Fernwärme | 8640.0 |

---

## Datensatz C: Endenergieverbrauch Gebäude Kanton (div-energie-4)

**Beschreibung:** Jährlicher Endenergieverbrauch von Gebäuden auf Kantonsebene nach Energieträger in Megawattstunden. Aggregierte Version von Datensatz B ohne Gemeindebezug – ideal für kantonale Trendanalysen.

**Download-URL:**
```
https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-4/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich
```

**Spalten:**

| Spalte | Typ | Beschreibung |
|--------|-----|--------------|
| `jahr` | int | Erhebungsjahr |
| `energietraeger` | chr | Art des Energieträgers |
| `verbrauch_mwh` | dbl | Verbrauchte Energie in Megawattstunden |

**Mögliche Werte `energietraeger`:** Identisch mit Datensatz B

**Beispieldaten:**

| jahr | energietraeger | verbrauch_mwh |
|------|----------------|---------------|
| 2018 | Heizöl | 2845000.0 |
| 2019 | Erdgas | 1124500.0 |
| 2020 | Wärmepumpe | 398200.0 |
| 2021 | Holz | 612800.0 |
| 2022 | Fernwärme | 187400.0 |

---

## Datensatz D: Hauptheizsysteme nach Gemeinden (div-energie-12)

**Beschreibung:** Anzahl Gebäude pro Gemeinde, Jahr und Heizsystemtyp. Ermöglicht Aussagen über den Heizungsbestand und den Umbau hin zu erneuerbaren Heizsystemen (z.B. Rückgang Ölheizungen, Zunahme Wärmepumpen).

**Download-URL:**
```
https://data.tg.ch/api/explore/v2.1/catalog/datasets/div-energie-12/exports/csv?delimiter=%3B&lang=de&timezone=Europe%2FZurich
```

**Spalten:**

| Spalte | Typ | Beschreibung |
|--------|-----|--------------|
| `gemeinde_nr` | int | Gemeindenummer (BFS-Nummer) |
| `gemeinde` | chr | Gemeindename |
| `jahr` | int | Erhebungsjahr |
| `heizsystem` | chr | Art des Heizsystems |
| `anzahl` | int | Anzahl Gebäude mit diesem Heizsystem |

**Mögliche Werte `heizsystem`:** `"Ölheizung"`, `"Gasheizung"`, `"Wärmepumpe"`, `"Holzheizung"`, `"Elektroheizung"`, `"Fernwärme"`, `"Sonstige"`

**Beispieldaten:**

| gemeinde_nr | gemeinde | jahr | heizsystem | anzahl |
|-------------|----------|------|------------|--------|
| 4566 | Frauenfeld | 2018 | Ölheizung | 1842 |
| 4566 | Frauenfeld | 2022 | Wärmepumpe | 634 |
| 4671 | Kreuzlingen | 2020 | Gasheizung | 2105 |
| 4806 | Arbon | 2021 | Holzheizung | 287 |
| 4931 | Weinfelden | 2022 | Ölheizung | 912 |

---

## Klassifikation Energieträger (Datensätze B und C)

| Kategorie | Energieträger |
|-----------|---------------|
| **Fossil** | `"Heizöl"`, `"Erdgas"` |
| **Erneuerbar** | `"Holz"`, `"Wärmepumpe"`, `"Fernwärme"`, `"Sonne thermisch"` |
| **Sonstige** | `"Elektrizität"`, `"Übrige"` |

---

## Hinweise

- **Spaltennamen** können je nach CSV-Export leicht abweichen (z.B. Grossschreibung, Umlaute). Bei Abweichungen: `rename()` oder `rename_with(tolower)` verwenden.
- **Fehlende Werte:** Manche Gemeinden haben nicht für alle Jahre Daten. `NA`-Werte vor der Analyse prüfen und ggf. mit `filter(!is.na(...))` oder `replace_na()` behandeln.
- **Einlesefunktion:** Da das CSV Semikolon-getrennt ist, `read_csv2()` statt `read_csv()` verwenden – oder `read_csv()` mit dem Argument `delim = ";"`.
- **Gemeinde-Verknüpfung:** Für Joins zwischen Datensätzen `gemeinde_nr` (numerisch, stabil) bevorzugen gegenüber `gemeinde` (Name kann variieren).
- **Jahresverfügbarkeit:** Nicht alle Datensätze decken denselben Zeitraum ab. Vor Joins den gemeinsamen Zeitraum mit `intersect(unique(...$jahr), unique(...$jahr))` prüfen.
