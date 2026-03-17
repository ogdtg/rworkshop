
###################################################
### MASTER FILE MIT ALLEN AUFGABEN UND LÖSUNGEN ###
###################################################

############################################
## AUFGABE 1: Daten einlesen (10 Minuten) ##
############################################

#####################################################################################

######################################################################################
# a) EXCEL                                                                           #
#    Lies die Datei bevölkerung.xlsx als data.frame ein.                             #
#    Nenne den data.frame `bev`                                                      #
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `bev` anzeigen zu lassen#
######################################################################################
library(readxl)

bev <- read_excel(path = "rmd/Daten/bevölkerung.xlsx")

head(bev,n = 3)

##########################################################################################
# b) CSV                                                                                 #  
#    Lies die Datei bevölkerung.csv als data.frame ein.                                  #
#    Nenne den data.frame `bev_csv`                                                      #
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `bev_csv` anzeigen zu lassen#
##########################################################################################
   


##########################################################################################
# c) SAS                                                                                 #
#    Lies die Datei bevnatgeb2021.sas7bdat als data.frame ein.                           #  
#    Nenne den data.frame `bevnat`                                                       #
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `bevnat` anzeigen zu lassen #
##########################################################################################


# DIESE DATEI KANN AUS DATENSCHUTZGRÜNDEN NICHT MITGELIEFERT WERDEN
# bevnat <- read_sas("rmd/Daten/bevnatgeb2021.sas7bdat")
# head(bevnat, 3)

################################################################################################
# d) EXCEL (Bonus)                                                                             #  
#    Lies das Tabellenblatt `altersklassen` aus der Datei bevölkerung.xlsx als data.frame ein. #
#    Nenne den data.frame `alter`                                                              #  
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `alter` anzeigen zu lassen        #
################################################################################################
library(readxl)

bev_altersklasse <- read_excel(path = "rmd/Daten/bevölkerung.xlsx" ,
                                       sheet = "altersklassen", 
                                       skip = 2)

#############################################################################################


############################################
## AUFGABE 2: Pipelines |>   (5 Minuten) ##
############################################

num_vec <- c(5,4,6,1,4)

num_vec |> 
  sum() |> 
  sqrt() |> 
  round(digits=1)
####################################################################################################################################################
# num_vec soll aufsummiert und anschliessend die Wurzel aus der Summe gezogen werden. Das Ergebnis soll auf eine Nachkommastelle gerundet werden   #
# Die folgende Codezeile führt diese Operationen aus.                                                                                              #
####################################################################################################################################################



##########################################################################################################################
# a) Verwende den |>  Operator um die oben stehende Codezeile in eine Pipe im Stile von dplyr umzuwandeln                #
#    Wandle die Zeile in EINE Pipe um OHNE Zwischenergebnisse zu speichern. Verwende den Namen des Vektors nur EINMAL.   #
##########################################################################################################################






######################################################################################################




########################
## AUFGABE 3: tidyr() ##
########################

library(tidyr)

abstimmungen <- readRDS("rmd/Daten/Abstimmungen.rds")

#############################################################################################################################################
# a) Bringe die Abstimmungsdaten in eine sinnvolle Form im Sinne von Tidy Data (Wide format)                                                #
#    Alle Werte in der `Ergebnis` Spalte sollen eine einzelne Spalte sein                                                                   #
#    Ihnen soll jeweils der entsprechende Wert aus der Spalte `Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)` untergeordnet werden  #
#    Speichere das Ergebnis als `abstimmungen_tidy`                                                                                         #
#############################################################################################################################################


abstimmungen_tidy <- abstimmungen |> 
  pivot_wider(names_from = Ergebnis,
              values_from = `Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)`) 
  



##################################################################
#  b) Bringe die Daten wieder in die Ursprungsform (long format) #
#     Speichere das Ergebnis unter `abstimmungen_not_tidy`       #
##################################################################

abstimmungen_not_tidy <- abstimmungen_tidy |>  
  pivot_longer(cols = Stimmberechtigte:`Ja in %`,
               names_to = "Ergebnis",
               values_to = "Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)")

######################################################################################################




###################################################
## AUFGABE 4: filter() und select() (10 Minuten) ##
###################################################

library(dplyr)

####################################################################################
# a) Lies die Datei geburten_mod.rds aus dem Ordner Daten als data.frame ein.#
####################################################################################


# AUS DATENSCHUTZGRÜNDEN WURDEN DIE DATEN ANONYMISIERT
geburten <- readRDS("rmd/Daten/geburten_mod.rds")

########################################################################################
# c) Wähle nur die Spalten `geb_day`,`geb_mon`,`geb_year`, `sex`, `nat` und `name` aus.#
#    Behalte nur Geburten nach 2010 deren Staatsangehörigkeit Schweiz (8100) ist.      #
#    Speichere das Resultat in der Variable `geburten_ch_2010`                         #
########################################################################################

geburten |> 
  filter(geb_year>2010 & nat == "8100") |> 
  select(geb_day,geb_mon,geb_year,sex,nat,name) 

###################################
# c) Gebe die ersten 3 Zeilen aus.#
###################################



######################################################################################################




##################################################################
## AUFGABE 5: mutate(), group_by() und summarise() (12 Minuten) ##
##################################################################

# Was passiert in diesem Code Abschnitt?
geburten_ch_2010 <- geburten_ch_2010 %>% 
  mutate(geb_day = ifelse(geb_day<10, paste0("0",geb_day), geb_day),
         geb_mon = ifelse(geb_mon<10, paste0("0",geb_mon), geb_mon))

##################################################################################################################
# a) Erstelle für den in der vorherigen Aufgabe erstellten Datensatz geburten_ch_2010 eine neue Spalte `geb_date`#
#    Die Spalte soll das Geburtsdatum im Format tt.mm.jjjj enthalten.                                            #
#    Verwende dazu die paste0() Funktion.                                                                        #
#    Speichre das Resultat unter dem Namen geburten_ch_2010_date                                                 #
##################################################################################################################


# Führe den untenstehenden Code aus, um aus dem dem string value in geb_date ein von R lesbares Datum zu erstellen
# Hinweis: von R lesbare Datumsangaben sind im Format jjjj-mm-dd
geburten_ch_2010_date <- geburten_ch_2010_date %>% 
  mutate(geb_date = as.Date(geb_date, format = "%d.%m.%Y"))


#########################################################################################
# b) Erstelle eine Variable `age`, welche das Alter des Kindes zum heutigen Tag enthält.#
#    Tipp: das heutige Datum kann mit Sys.Date() ermittelt und verwendet werden         #
#    `age` soll das Alter in vollendeten Lebensjahren beinhalten                        #
#########################################################################################

# Wenn du nicht zum richtigen Ergebnis kommst, schreibe auf was theoretisch erledigt werden muss um das gewünschte Ergebnis zu erreichen



###################################################################
# c) Zähle die Geburten pro (Geburts-)Jahr im Datensatz           #
#    Tipp: Nutze group_by() sowie count() bzw. summarise() und n()#
###################################################################




######################################################################################################


##################################################################
## AUFGABE 6: Filtering joins und mutating Joins (12 Minuten)   ##
##################################################################


codes <- readRDS("rmd/Daten/codes.rds")


# Was macht der untenstehende Code?
geburten_nat <- geburten %>% 
  filter(name != "") %>% 
  mutate(name = tolower(name)) %>% 
  group_by(nat,sex,name) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(nat, sex) %>% 
  filter(n == max(n))
  
#############################################################################################################
# a) Joine die `geburten_nat` mit dem `codes` Datensatz um den Ländernamen zu den Namensdaten hinzuzufügen  #
#    Nutze dafür `left_join()`                                                                              #
#    Schau dir die Datensätze an, um die gemeinsamen Spalten herauszufinden                                 #  
#    Speichere den neuen Datensatz unter dem Namen `geburten_join`                                          #
#############################################################################################################




##############################################################################################################
# b) Wie lautet der häufigste männliche Vorname für Kinder mit der Nationalität Nordmazedonien im Datensatz? #
##############################################################################################################




############################################################################################################



# ##########################################################################
# ## AUFGABE: Meldungen, Warnungen & Fehler (10 Minuten)                  ##
# ##########################################################################
# 
# ####################################################################################
# # a) Error, Warning oder Message?                                                  #
# #    Welchen Ausgabetyp gibt R bei den folgenden Codezeilen aus?                   #
# #    Schreibe den Typ (Error / Warning / Message) als Kommentar hinter jede Zeile. #
# #    Führe den Code erst aus, wenn du eine Vermutung notiert hast.                 #
# ####################################################################################
# 
# library(dplyr)                                    # Typ: ?
# 
# as.integer(c("1", "zwei", "3"))                   # Typ: ?
# 
# mean(c(1, 2, 3))                                  # Typ: ?
# 
# log(-1)                                           # Typ: ?
# 
# filter(kanton == "TG")                            # Typ: ?
# 
# 
# ####################################################################################
# # b) Finde und behebe den Fehler                                                   #
# #    In jedem Abschnitt ist ein Fehler eingebaut.                                  #
# #    Erkläre den Fehler kurz als Kommentar und korrigiere den Code.                #
# ####################################################################################
# 
# # --- Fehler 1 ---
# meine_zahlen <- c(10, 20, 30, 40, 50)
# mittelwert   <- Mean(meine_zahlen)
# # Fehler:
# 
# 
# # --- Fehler 2 ---
# library(readr)
# df <- read_csv("rmd/Daten/bevölkerung.csv"
# head(df, 3)
# # Fehler:
# 
# 
# # --- Fehler 3 ---
# zahlen <- c(1, 4, 9, 16, 25)
# zahlen |>
#   sqrt() |>
#   rund(digits = 2)
# # Fehler:
# 
# 
# # --- Fehler 4 ---
# df_fehler <- data.frame(
#   kanton = c("TG", "ZH", "BE"),
#   wert   = c(100, 200, 150)
# )
# df_fehler |> filter(kanton = "TG")
# # Fehler:
# 
# 
# # --- Fehler 5 ---
# mein_text <- "Hallo, Welt
# nchar(mein_text)
# # Fehler:
# 
# 
# ####################################################################################
# # c) Hilfe benutzen                                                                #
# #    Öffne die Hilfedokumentation zur Funktion `str_pad()` (Package stringr).     #
# #    Beantworte die Fragen als Kommentar.                                          #
# ####################################################################################
# 
# library(stringr)
# ?str_pad
# 
# # 1. Was macht str_pad()?
# #    Antwort:
# 
# # 2. Was bewirkt der Parameter `side`? Welche Werte kann er annehmen?
# #    Antwort:
# 
# # 3. Führe das erste Beispiel von der Hilfeseite aus:
# 
# 
# ####################################################################################
# # d) Bonus: Warning verstehen und beheben                                          #
# #    Führe den Code aus. Welche Warnung gibt R aus, und warum?                    #
# #    Wie kannst du die Summe trotzdem korrekt berechnen?                           #
# ####################################################################################
# 
# messwerte <- c("12.5", "8.3", "keine Angabe", "15.1", "n/a")
# zahlen    <- as.numeric(messwerte)
# summe     <- sum(zahlen)
# summe
# 
# # Warnung lautet:
# # Warum:
# # Lösung (korrigierter Code):


############################################################################################################

#############################################################
## AUFGABE 7: Arbeiten mit stringr und Regular Expressions ##
#############################################################

library(stringr)
library(BFS)

# Mit dem BFS Package kann direkt auf die Stattab Tabellen zugegriffen werden
# In diesem Beispiel verwenden wir die Abstimmungsdaten von Stattab für die Gesamtschweiz und den Thurgau

abstimmungen <- bfs_get_data(language = "de", number_bfs = "px-x-1703030000_100",
                             query = list("Kanton" = c("CH","TG")))

####################################################################################################################
# a) Bringe die Tabelle in ein Tidy Format, sodass jeder Wert in `Ergebnis` einer eigenen Spalte entspricht        #
#    Die dazugehörigen Werte sollen aus der Spalte `Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)`         #
#    Nutze das tidyr package und eine der vohin besprochenen Funktionen um die Tabelle zu verbreitern (wide Format)#
#    Speichere das Ergebnis in `abstimmungen_tidy`                                                                 #
####################################################################################################################


  

###################################################################
# b) Erstelle eine neue Variable `date`.                          #
#    Extrahiere dazu das Datum aus der Spalte `Datum und Vorlage` #
#    Nutze dafür den Regex \\d\\d\\d\\d-\\d\\d-\\d\\d             #
###################################################################




########################################################################################################################
# c) Entferne das Datum aus der Spalte `Datum und Vorlage` und wandle die `date` Spalte in ein von R lesbares Datum um.#
########################################################################################################################    



##############################################################################################################
# d) Entferne die Leerzeichen anführenden Leerzeichen aus der Spalte `Datum und Vorlage` (" Test " -> "Test")#
##############################################################################################################





############################################################################################################

#############################################################
## AUFGABE 8:lubridate und das Arbeiten mit Datumsangaben  ##
#############################################################

library(lubridate)

##########################################################################################################################
# a) Betrachten wir den Datensatz `geburten`. Wir möchten das Geburtsdatum noch genauer wissen und beziehen deshalb      #
#    die Geburtsstunde des Kindes mit ein. Demnach bilden wir aus dem Geburtsjahr (geb_year), dem Geburtsmonat (geb_mon),# 
#    dem Geburtstag und der Geburtsstunde eine neue Variable `geb_fulldate`, die ein von R lesbares Datum                #
#    vom Typ `datetime` enthält.                                                                                         #
#    Füge diese neue Spalte zum Datensatz `geburten` hinzu und verwende das `lubridate` package                          #
##########################################################################################################################



#########################################################################################################
# b) Was ist der Grund für die Warning Message wonach das parsing bei einigen Zeilen fehlgeschlagen ist #
#########################################################################################################


######################################################
# c) Wie könnte man dieses Problem theoretisch lösen #
######################################################






##############################################################
## AUFGABE 9: Conditional Statements: case_when und ifelse  ##
##############################################################

#################################################################################################################################
# a) Erstelle den Datensatz `geburten_nat_eltern` aus dem `geburten` Datensatz:                                                                              #
#     - Spalten auswählen: behalte nur stat_jahr, nat_m und nat_V                                                               #
#     - Filtern: nur Daten behalten die keine NAs in nat_m oder nat_V enthalten (Staatsangehörigkeit Mutter und Vater) besitzen #
#################################################################################################################################


  

#######################################################################################################
# b) Verwende Conditional Statements um die neue Variable `eltern_nat_type`:                          #
#     - Wenn beide Elternteile Ausländer sind, soll "beide Elternteile Ausländer" eingetragen werden  #
#     - Wenn beide Elternteile Schweizer sind, soll "beide Elternteile Schweizer" eingetragen werden  #  
#     - Wenn der Vater Ausländer ist, soll "Vater Ausländer" eingetragen werden                       #
#     - Wenn die Mutter Ausländerin ist, soll "Mutter Ausländer" eingetragen werden                   #          
#     - Wenn keine der Kategorien zutrifft soll "unbekannt" eingetragen werden (Kontrolle)            #
#######################################################################################################





##################################################################################
# c) Erstelle einen data.frame `geburten_nat_eltern_count`, der zeigt,           #                                      
#    wie hoch die Anzahl der verschiedenen Elternteil-Kombinationen pro Jahr ist #
##################################################################################







  


#####################################
## AUFGABE 10: while und for Loops ##
#####################################

####################################################################################################################################################################
# Lade den Datensatz `Daten/firma.rds` und berechne die jährlichen Auszahlungsbeträge pro Mitarbeiter inklusive Boni:                                              #
#                                                                                                                                                                  #
#  - Mitarbeiter mit einem Monatslohn über 10000 bekommen keinen Bonus                                                                                             #
#  - **Sehr Gut** bewertete Mitarbeiter bekommen einen Bonus in Höhe von **70% ihres Monatseinkommens**                                                            #
#  - **Gut** bewertete Mitarbeiter bekommen einen Bonus von **40% ihres Monatseinkommens**, bei **10 oder mehr Dienstjahren** bekommen sie **50%**                 #
#  - Bewertungen, die weder `Gut` noch `Sehr Gut` sind bekommen den Standardbonus von **300 CHF**                                                                  #
#                                                                                                                                                                  #
#  Schreibe einen Loop, welcher eine Liste befüllt mit dem String "NAME erhält AUSZAHLUNG CHF im Jahr inkl. Bonus" für jede/n MitarbeiterIn im Datensatz.          #
#  Sollte ein/e MitarbiertIn keinen Bonus erhalten, sollte der String wie folgt aussehen: "NAME erhält AUSZAHLUNG CHF im Jahr. NAME erhält keinen Bonus"           #
####################################################################################################################################################################
  

firma <- readRDS("rmd/Daten/firma.rds")


lohn_list <- list()






##################################
## AUFGABE 11: Apply Funktionen ##
##################################

# Gegeben ist ein Auszug aus dem Mikrezensus Mobilität und Verkehr 2021 (haush_sample.rds) sowie eine Liste von Fahrzeugklassen


fahrzeug_klassen <- list(velo_total = c("f32200a","f32200b","f32200c"),
                         evelo = c("f32200b","f32200c"),
                         auto = c("f30100"),
                         motorrad = c("f31200"))


haush_sample <- readRDS("rmd/Daten/haushalte_sample.rds") 



##########################################################################################################################
# Für jede Fahrzeuklasse soll ein Datensatz erstellt werden, welcher den Mittelwert und die Gesamtnzahl der Fahrzeuge in #
# der entsprechenden Fahrzeugklasse pro Bezirk enthält. Verwende eine apply Funktion                                     #
# - Reihen bei denen die Anzahl kleiner 0 ist sind NAs und müssen entfernt werden                                        #
# - filtere die entsprechende Fahrzeugklasse                                                                             #
# - gruppiere die Daten nach Bezirk                                                                                      #
# - nutze summarise um die Kennzahlen zu errechnen                                                                       #
##########################################################################################################################






##################################
## AUFGABE 12: purrr Funktionen ##
##################################

# Konvertiere den Workfloe in eine Pipeline unter Verwendung der `map` Funktionen.
# Achte darauf die passende `map` Funktion zu verwenden und keine unnötigen Zusatzschritte zu unternehmen.
# Es wird der Datensatz haush_sample aus der vorherigen Aufgabe verwendet 
# Ausserdem wird das Siedlungsverzeichnis verwendet


# a) Gesamtanzahl aller Fahrzeuge pro Bezirk

bezirke <- unique(haush_sample$bezirk_name)

for_loop_list <- list()

for (bezirk in bezirke){
  for_loop_list[[bezirk]] <- haush_sample %>% 
    filter(bezirk_name == bezirk) %>% 
    summarise(total_anzahl_fahrzeuge = sum(anzahl))
  
}





# b) Anzahl Ortschaften pro Bezirk

# devtools::install_github("ogdtg/odsAPI")
library(odsAPI)
odsAPI::set_domain("kantonthurgau.opendatasoft.com")
svz <- odsAPI::get_dataset(dataset_id = "sk-stat-112")

anzahl_ortschaften <- sapply(bezirke,function(x){
  svz %>% 
    filter(bezirk_bezeichnung==x) %>% 
    distinct(ortschaft) %>% 
    nrow()
})


#############################################################################################################
# Gegeben ist die Liste gr_data_bezirke.                                                                    #
# Sie enthält Daten zu den Kandidatenstimmen bei den Grossratswahlen 2012-2020                              #
# Jedes der 15 Listenelmente stellt das Ergebnis eines Bezirks in einem bestimmten Jahr dar                 #
# Erstelle einen zusammenhängenden Datensatz, der den/die Kandidierende/n für jedes Jahr und jeden Bezirk   #
# mit den Meisten Stimmen enthält. Der Datensatz soll ausserdem den Bezirknamen und das Jahr enthalten,     #
# damit man das Ergebnis später nachvollzihen kann.                                                         #    
# Verwende die passende map Funktion                                                                        #    
#                                                                                                           #
# Der Datensatz `meiste_stimmen_bezirk` enthält das Ergebnis                                                #
#############################################################################################################

# Der Datensatz `meiste_stimmen_bezirk` enthält das Ergebnis



gr_data_bezirke <- readRDS("rmd/Daten/gr_wahlen_bezirk.rds")
meiste_stimmen_bezirk <- readRDS("rmd/Daten/meiste_stimmen_bezirk.rds")





############################
## AUFGABE 13: Funktionen ##
############################

###############################################################################################################
# a) Schreibe eine eigene Funktion zur Berechnung der Varianz eines numerischen Vektors und nenne sie varianz #
#    (siehe Folie)                                                                                            #
###############################################################################################################



test_vec <- c(4,5,6,4,9,7,2,9,1,4)
var(test_vec)
varianz(test_vec)

####################################################################################################
# b) Vereinfache den untenstehenden Code, sodass so wenig Code wie möglich geschrieben werden muss #
####################################################################################################

df_ch_2010 <- geburten %>% 
  filter(nat == 8100) %>% 
  filter(stat_jahr == 2010) %>% 
  group_by(stat_jahr,sex,nat) %>%
  summarise(n = n ()) %>% 
  left_join(codes, by = c("nat"="code"))


df_alb_2015 <- geburten %>% 
  filter(nat == 8201) %>% 
  filter(stat_jahr == 2015) %>% 
  group_by(stat_jahr,sex,nat) %>%
  summarise(n = n ()) %>% 
  left_join(codes, by = c("nat"="code"))


df_de_2020 <- geburten %>% 
  filter(nat == 8207) %>% 
  filter(stat_jahr == 2020) %>% 
  group_by(stat_jahr,sex,nat) %>%
  summarise(n = n ()) %>% 
  left_join(codes, by = c("nat"="code"))


df_it_1999 <- geburten %>% 
  filter(nat == 8218) %>% 
  filter(stat_jahr == 1999) %>% 
  group_by(stat_jahr,sex,nat) %>%
  summarise(n = n ()) %>% 
  left_join(codes, by = c("nat"="code"))



#################################################
## AUFGABE 14: Daten visualisieren mit ggplot2 ##
#################################################

library(ggplot2)

# Daten einlesen
heirat <- readRDS("rmd/Daten/heirat.rds")


########################################################################################################################
# Stelle das erreichte Alter von Männern und Frauen im Zeitverlauf gegenüber.                                          #
#                                                                                                                      #
# a) Schaue dir den vorbereiten Datensatz `heirat_mod` an, bevor du die Visualisierung angehst                         #
# b) Erzeuge ein Liniendiagramm, welches die Altersentwicklung von Männern und Frauen bei der Hochzeit abbildet        #
#    Das Liniendiagramm soll eine Linie pro Geschlecht enthalten. Beide Linien sollen unterschiedliche Farben haben.   #
# c) Füge einen Titel sowie eine Beschriftung für X- und Y-Achse hinzu.                                                #
#    Verwende dafür die labs() Funktion (?ggplot2::labs() für Hilfe)                                                   #
########################################################################################################################

heirat_mod <- heirat %>% 
  select(SJAHR_N,M_ALTER_ANNAEHERND_N,F_ALTER_ANNAEHERND_N) %>% 
  group_by(SJAHR_N) %>% 
  summarise(Frau = mean(F_ALTER_ANNAEHERND_N, na.rm = TRUE),
            Mann = mean(M_ALTER_ANNAEHERND_N, na.rm = TRUE)) %>% 
  filter(SJAHR_N<2022) %>% 
  pivot_longer(cols = c("Frau","Mann"), names_to = "geschlecht", values_to = "alter") 






##########################################
## AUFGABE 15: Daten in Excel schreiben ##
##########################################
library(openxlsx)

#############################################################################
# Repliziere die Tabelle in rmd/Daten/openxlsx_table.xlsx                #
#                                                                           #
# Speichere die Datei unter rmd/Daten/openxlsx_table_DEIN_KÜRZEL.xlsx    #
#############################################################################




##########################################
## AUFGABE 16: Daten in Excel schreiben ##
##########################################

###########################################################################################################
# Gegeben sind zwei Datensätze                                                                            #
#  - rmd/Daten/gemeinde_order.rds: enthält alle Gemeinden in der korrekten Reihenfolge wie im Excel    #
#  - rmd/Daten/geburten_24.rds: enthält Gesamtgeburtenzahlen für das Jahr 2024 nach Gemeinde, Bezirk   #
#    und für den Gesamtkanton                                                                             #
#                                                                                                         #
# Gegeben ist ausserdem die Datei rmd/Daten/2023_Gde_Geb_ab2000.xlsx                                   #
#                                                                                                         #
# Füge die Geburtenzahlen für das Jahr 2024 in die Excel 2023_Gde_Geb_ab2000.xlsx ein und speichere       #
# sie als 2024_Gde_Geb_ab2000_DEIN_KÜRZEL.xlsx neu ab                                                     #
###########################################################################################################  

library(TGexcel)


gemeinde_order <- readRDS("rmd/Daten/gemeinde_order.rds")
geburten_24 <- readRDS("rmd/Daten/geburten_24.rds")







# AUFGBAE 17 DS intern


############################################
## AUFGABE 18: tidyverse und Datenbanken  ##
############################################

###############################################################################################################################       
# Errechne den Ausländeranteil pro Gemeinde für die Jahre 2015 bis 2022.                                                      #
#                                                                                                                             #
# Beachte, dass der finale Datensatz lokal als normaler R data.frame nutzbar sein muss.                                       #
###############################################################################################################################

library(DBI)
library(odbc)
library(tidyverse)

con <- dbConnect(RSQLite::SQLite(), "rmd/Daten/landing_local.db")




