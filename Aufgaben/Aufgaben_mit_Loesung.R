
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

bev <- read_excel("rmd/Daten/bevölkerung.xlsx") 
head(bev, 3)


##########################################################################################
# b) CSV                                                                                 #  
#    Lies die Datei bevölkerung.csv als data.frame ein.                                  #
#    Nenne den data.frame `bev_csv`                                                      #
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `bev_csv` anzeigen zu lassen#
##########################################################################################
   
library(readr)

bev_csv <- read.csv("rmd/Daten/bevölkerung.csv")
head(bev_csv, 3)

##########################################################################################
# c) SAS                                                                                 #
#    Lies die Datei bevnatgeb2021.sas7bdat als data.frame ein.                           #  
#    Nenne den data.frame `bevnat`                                                       #
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `bevnat` anzeigen zu lassen #
##########################################################################################

library(haven)

bevnat <- read_sas("rmd/Daten/bevnatgeb2021.sas7bdat")
head(bevnat, 3)

################################################################################################
# d) EXCEL (Bonus)                                                                             #  
#    Lies das Tabellenblatt `altersklassen` aus der Datei bevölkerung.xlsx als data.frame ein. #
#    Nenne den data.frame `alter`                                                              #  
#    Benutze die head() Funktion, um die ersten 3 Zeilen von `alter` anzeigen zu lassen        #
################################################################################################
alter <- read_excel("rmd/Daten/bevölkerung.xlsx", sheet = "altersklassen",skip=2) 
head(alter, 3)

write_csv(x=alter,file="rmd/Output/Felix/alter.csv", na ="")

#############################################################################################


############################################
## AUFGABE 2: magrittr:: %>%  (5 Minuten) ##
############################################

num_vec <- c(5,4,6,1,4)

####################################################################################################################################################
# num_vec soll aufsummiert und anschliessend die Wurzel aus der Summe gezogen werden. Das Ergebnis soll auf eine Nachkommastelle gerundet werden   #
# Die folgende Codezeile führt diese Operationen aus.                                                                                              #
####################################################################################################################################################

round(sqrt(sum(num_vec)),1)


library(magrittr)
##########################################################################################################################
# a) Verwende den %>% Operator um die oben stehende Codezeile in eine Pipe im Stile von dplyr umzuwandeln                #
#    Wandle die Zeile in EINE Pipe um OHNE Zwischenergebnisse zu speichern. Verwende den Namen des Vektors nur EINMAL.   #
##########################################################################################################################

num_vec %>% 
  sum() %>% 
  sqrt() %>% 
  round(1) 




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


abstimmungen_tidy <- abstimmungen %>% 
  pivot_wider(names_from = "Ergebnis", values_from = "Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)")


##################################################################
#  b) Bringe die Daten wieder in die Ursprungsform (long format) #
#     Speichere das Ergebnis unter `abstimmungen_not_tidy`       #
##################################################################

abstimmungen_not_tidy <- abstimmungen_tidy %>% 
  pivot_longer(cols = Stimmberechtigte:`Ja in %`,
               names_to = "Ergebnis",
               values_to = "Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)")

######################################################################################################




###################################################
## AUFGABE 4: filter() und select() (10 Minuten) ##
###################################################

library(dplyr)

####################################################################################
# a) Lies die Datei bevnatgeb2021.sas7bdat aus dem Ordner Daten als data.frame ein.#
####################################################################################

geburten <- read_sas("rmd/Daten/bevnatgeb2021.sas7bdat")

########################################################################################
# c) Wähle nur die Spalten `geb_day`,`geb_mon`,`geb_year`, `sex`, `nat` und `name` aus.#
#    Behalte nur Geburten nach 2010 deren Staatsangehörigkeit Schweiz (8100) ist.      #
#    Speichere das Resultat in der Variable `geburten_ch_2010`                         #
########################################################################################

geburten_ch_2010 <- geburten %>% 
  dplyr::select(geb_day,geb_mon,geb_year,sex,nat,name) %>% 
  filter(geb_year > 2010 & nat == 8100)

###################################
# c) Gebe die ersten 3 Zeilen aus.#
###################################

head(geburten_ch_2010,3)


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
geburten_ch_2010_date <- geburten_ch_2010 %>% 
  mutate(geb_date = paste0(geb_day,".",geb_mon,".",geb_year))

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
# 1. Abstand zwischen Sys.Date() und geb_date errechnen
# 2. Resultierende Tage durch 365 teilen um Jahre zu erhalten
# 3. Ergebnis nach unten Runden um vollendete Lebensjahre zu erhalten

geburten_ch_2010_date <- geburten_ch_2010_date %>%
  mutate(
    age = Sys.Date() - geb_date,
    age = age / 365,
    age = floor(age),
    age = as.numeric(age)
  )

head(geburten_ch_2010_date,3)


###################################################################
# c) Zähle die Geburten pro (Geburts-)Jahr im Datensatz           #
#    Tipp: Nutze group_by() sowie count() bzw. summarise() und n()#
###################################################################


geburten_ch_2010_date %>% 
  group_by(geb_year) %>% 
  summarise(n = n())

geburten_ch_2010_date %>% 
  group_by(geb_year) %>% 
  count()


geburten_ch_2010_date %>% 
  group_by(geb_year) %>% 
  mutate(total = n()) %>% 
  filter(sex == 2) %>% 
  ungroup() %>% 
  group_by(geb_year,sex,total) %>% 
  summarise(girls = n())

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

geburten_join <- geburten_nat %>% 
  left_join(codes , by = c("nat"="code"))


##############################################################################################################
# b) Wie lautet der häufigste männliche Vorname für Kinder mit der Nationalität Nordmazedonien im Datensatz? #
##############################################################################################################


geburten_join %>% 
  filter(country=="Nordmazedonien") %>% 
  filter(sex == 1)


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


abstimmungen_tidy <- abstimmungen %>% 
  pivot_wider(names_from = "Ergebnis", values_from = "Volksabstimmungen (Ergebnisse Ebene Kanton seit 1866)")
  

###################################################################
# b) Erstelle eine neue Variable `date`.                          #
#    Extrahiere dazu das Datum aus der Spalte `Datum und Vorlage` #
#    Nutze dafür den Regex \\d\\d\\d\\d-\\d\\d-\\d\\d             #
###################################################################

abstimmungen_tidy <- abstimmungen_tidy %>% 
  mutate(date = str_extract(`Datum und Vorlage`, pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\d"))



########################################################################################################################
# c) Entferne das Datum aus der Spalte `Datum und Vorlage` und wandle die `date` Spalte in ein von R lesbares Datum um.#
########################################################################################################################    

abstimmungen_tidy <- abstimmungen_tidy %>% 
  mutate(`Datum und Vorlage` = str_remove(`Datum und Vorlage`, pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\d")) %>% 
  mutate(date = as.Date(date))


##############################################################################################################
# d) Entferne die Leerzeichen anführenden Leerzeichen aus der Spalte `Datum und Vorlage` (" Test " -> "Test")#
##############################################################################################################

abstimmungen_tidy <- abstimmungen_tidy %>% 
  mutate(`Datum und Vorlage` = str_trim(`Datum und Vorlage`))



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

geburten_time <- geburten %>% 
  select(geb_year,geb_mon,geb_day,geb_time) %>% 
  mutate(geb_fulldate = paste0(geb_year,"/",geb_mon,"/",geb_day," ",geb_time)) %>% 
  mutate(geb_fulldate = ymd_h(geb_fulldate))

#########################################################################################################
# b) Was ist der Grund für die Warning Message wonach das parsing bei einigen Zeilen fehlgeschlagen ist #
#########################################################################################################

# geb_time ist 99. Dies steht wahrscheinlich für "unbekannt". Da 99>24 kann lubridate keine Stundenangabe erzeugen

######################################################
# c) Wie könnte man dieses Problem theoretisch lösen #
######################################################

# 1. 99 zu 0 Uhr umwandeln -> Verfälschung da dies nicht dem wahrenWert entspricht
# 2. Daten entfernen 
# 3. NAs belassen
# 4. Daten nur auf ymd Basis berechnen




##############################################################
## AUFGABE 9: Conditional Statements: case_when und ifelse  ##
##############################################################

#################################################################################################################################
# a) Erstelle den Datensatz `geburten_nat_eltern` aus dem `geburten` Datensatz:                                                                              #
#     - Spalten auswählen: behalte nur stat_jahr, nat_m und nat_V                                                               #
#     - Filtern: nur Daten behalten die keine NAs in nat_m oder nat_V enthalten (Staatsangehörigkeit Mutter und Vater) besitzen #
#################################################################################################################################

geburten_nat_eltern <- geburten %>% 
  select(stat_jahr,nat_m,nat_V ) %>% 
  filter(!is.na(nat_m)) %>% 
  filter(!is.na(nat_V)) 
  

#######################################################################################################
# b) Verwende Conditional Statements um die neue Variable `eltern_nat_type`:                          #
#     - Wenn beide Elternteile Ausländer sind, soll "beide Elternteile Ausländer" eingetragen werden  #
#     - Wenn beide Elternteile Schweizer sind, soll "beide Elternteile Schweizer" eingetragen werden  #  
#     - Wenn der Vater Ausländer ist, soll "Vater Ausländer" eingetragen werden                       #
#     - Wenn die Mutter Ausländerin ist, soll "Mutter Ausländer" eingetragen werden                   #          
#     - Wenn keine der Kategorien zutrifft soll "unbekannt" eingetragen werden (Kontrolle)            #
#######################################################################################################


geburten_nat_eltern <- geburten_nat_eltern %>% 
  mutate(eltern_nat_type = case_when(
    nat_m == 8100 & nat_V == 8100 ~ "beide Elternteile Schweizer",
    nat_m != 8100 & nat_V != 8100 ~ "beide Elternteile Ausländer",
    nat_m != 8100 & nat_V == 8100 ~ "Mutter Ausländer",
    nat_m == 8100 & nat_V != 8100 ~ "Vater Ausländer",
    TRUE ~ "unbekannt"
  ))


##################################################################################
# c) Erstelle einen data.frame `geburten_nat_eltern_count`, der zeigt,           #                                      
#    wie hoch die Anzahl der verschiedenen Elternteil-Kombinationen pro Jahr ist #
##################################################################################

geburten_nat_eltern_count <- geburten_nat_eltern %>% 
  group_by(stat_jahr,eltern_nat_type) %>% 
  count()

library(ggplot2)
geburten_nat_eltern_count %>% 
  ggplot(aes(stat_jahr,n, color = eltern_nat_type)) +
  geom_line()





  


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

for (i in seq_along(firma$name)) {
  # Monatslohn über 10000?
  if (firma$monatslohn[i]>10000){
    jahreslohn <- firma$monatslohn[i]*12
    lohn_list[[i]] <- paste0(firma$name[i], " erhält ",jahreslohn," CHF im Jahr. ",firma$name[i]," erhält keinen Bonus.")
    next # Weiter zur nächsten Iteration
  }
  # bewertung sehr gut -> 70% des Monatslohns
  if (firma$bewertung[i]=="Sehr gut"){
    bonus <- firma$monatslohn[i]*0.7
  }else if (firma$bewertung[i]=="Gut") {
    if (firma$dienstjahre[i]>=10){
      bonus <- firma$monatslohn[i]*0.5 # Gut und 10 oder mehr Dienstjahre
    } else {
      bonus <- firma$monatslohn[i]*0.4 # Gut und weniger als 10 Jahre
    }
  } else {
    bonus <- 300 # Standardbonus
  }
  # Auszahlungsbetrag pro Jahr berechnen
  jahreslohn <- firma$monatslohn[i]*12 + bonus
  
  # String erstellen
  lohn_list[[i]] <- paste0(firma$name[i], " erhält ",jahreslohn," CHF im Jahr inkl. Bonus.")
}

lohn_list




##################################
## AUFGABE 11: Apply Funktionen ##
##################################

# Gegeben ist ein Auszug aus dem Mikrezensus Mobilität und Verkehr 2021 (haush_sample.rds) sowie eine Liste von Fahrzeugklassen


fahrzeug_klassen <- list(velo_total = c("f32200a","f32200b","f32200c"),
                         evelo = c("f32200b","f32200c"),
                         auto = c("f30100"),
                         motorrad = c("f31200"))


haush_sample <- readRDS("rmd/Daten/hashalte_sample.rds")



##########################################################################################################################
# Für jede Fahrzeuklasse soll ein Datensatz erstellt werden, welcher den Mittelwert und die Gesamtnzahl der Fahrzeuge in #
# der entsprechenden Fahrzeugklasse pro Bezirk enthält. Verwende eine apply Funktion                                     #
# - Reihen bei denen die Anzahl kleiner 0 ist sind NAs und müssen entfernt werden                                        #
# - filtere die entsprechende Fahrzeugklasse                                                                             #
# - gruppiere die Daten nach Bezirk                                                                                      #
# - nutze summarise um die Kennzahlen zu errechnen                                                                       #
##########################################################################################################################



lapply(fahrzeug_klassen, function(x){
  temp <- haush_sample %>% 
    filter(anzahl>=0) %>% 
    filter(fahrzeug %in% x) %>% 
    group_by(bezirk_name) %>% 
    summarise(gesamtanzahl = sum(anzahl),
              durchscnitt = mean(anzahl))
})



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

map_list_fz <- bezirke %>% 
  map(~ haush_sample %>% 
        filter(bezirk_name == .x) %>% 
        summarise(total_anzahl_fahrzeuge = sum(anzahl)))



# b) Anzahl Ortschaften pro Bezirk


library(odsAPI)
odsAPI::set_domain("kantonthurgau.opendatasoft.com")
svz <- get_dataset(dataset_id = "sk-stat-112")

anzahl_ortschaften <- sapply(bezirke,function(x){
  svz %>% 
    filter(bezirk_bezeichnung==x) %>% 
    distinct(ortschaft) %>% 
    nrow()
})


map_anzahl_ortschaften <- bezirke %>% 
  map_dbl(~ svz %>% 
            filter(bezirk_bezeichnung == .x)%>% 
            distinct(ortschaft) %>% 
            nrow())

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


meiste_stimmen <- gr_data_bezirke %>% 
  map_df(~ .x %>% 
           group_by(jahr,bezirk_bez,kand_nachname,kand_vorname,liste_kand_id) %>% 
           summarise(kand_stimmen_total = sum(kand_stimmen_total)) %>% 
           ungroup() %>% 
           filter(kand_stimmen_total ==max(kand_stimmen_total)))


############################
## AUFGABE 13: Funktionen ##
############################

###############################################################################################################
# a) Schreibe eine eigene Funktion zur Berechnung der Varianz eines numerischen Vektors und nenne sie varianz #
#    (siehe Folie)                                                                                            #
###############################################################################################################

varianz <- function(x){
  vals <- sapply(x, function(xi){
    temp <- (xi-mean(x))^2
  })
  result <- 1/(length(x)-1)*sum(vals)
  return(result)
}

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


summarise_geburten <- function(land,jahr,df = geburten,country_codes = codes){

  code <- country_codes %>% 
    filter(country==land) %>% 
    pull(code)
  
  result <- geburten %>% 
    filter(nat == code) %>% 
    filter(stat_jahr == jahr) %>% 
    group_by(stat_jahr,sex,nat) %>%
    summarise(n = n ()) %>% 
    left_join(codes, by = c("nat"="code"))
  
  return(result)
}


df_ch_2010_2 <- summarise_geburten(land = "Schweiz", jahr = 2010)
df_alb_2015_2 <- summarise_geburten(land = "Albanien", jahr = 2015)
df_de_2020_2 <- summarise_geburten(land = "Deutschland", jahr = 2020)
df_it_1999_2 <- summarise_geburten(land = "Italien", jahr = 1999)

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

heirat_mod %>% 
  ggplot(aes(x=SJAHR_N,y = alter)) +
  geom_line(aes(color = geschlecht)) +
  labs(title = "Thurgauer heiraten immer später", x = "Jahr", y="Durchschnittliches Alter bei Heirat")+
  theme_light()





##########################################
## AUFGABE 15: Daten in Excel schreiben ##
##########################################
library(tidyverse)
library(openxlsx)

#############################################################################
# Repliziere die Tabelle in rmd/Daten/openxlsx_table.xlsx                #
#                                                                           #
# Speichere die Datei unter rmd/Daten/openxlsx_table_DEIN_KÜRZEL.xlsx    #
#############################################################################


wb <- createWorkbook()
sheetname <- "Iris Data"
data <- iris

# Sheet hinzufügen
addWorksheet(wb,sheetname)

# Dateneintragen
writeData(wb,x=data,sheet = sheetname)

# Style erzeugen
header_style <- createStyle(fontColour = "white",fgFill = "red", textDecoration = "bold")

# Style anwenden
addStyle(wb,sheetname,header_style,rows = 1,cols = 1:ncol(data),gridExpand = TRUE)

# Workbook abspeichern 
saveWorkbook(wb,"rmd/Daten/openxlsx_table.xlsx",overwrite = TRUE)


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




excel_data <- gemeinde_order |> 
  left_join(geburten_24, by = c("BFS-Nr.1"="bfs_nr")) |> 
  select(anzahl_geburten) |> 
  setNames("2024")



wb <- TGexcel::create_xlsx_spalte(xlsx_path = "rmd/Daten/2023_Gde_Geb_ab2000.xlsx",data = excel_data,year = 2024,dataStart = 4)

save_tg_workbook(wb, "rmd/Daten/2024_Gde_Geb_ab2000_FL.xlsx",tg_header = F,overwrite = T)



##########################################
## AUFGABE 17: Referenzdaten beziehen   ##
##########################################

devtools::load_all("/r-proj/stat/ogd/dataspotR")

# Sys.setenv(DATASPOT_PW="XXXXXXXX")
# Sys.setenv(DATASPOT_USER="XXXXXXXX")

ref_reduced <- get_referenzdaten_list() |> 
  select(id,label,title,createdBy)


r1 <- get_referenzdaten("Religion (ReligionII) SE") |> 
  select(id,shortText,code,createdBy) |> 
  filter(code==9)

paste0("Dem Wert 9 entspricht ",r1$shortText)


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

tbl(con,"geres_bestand_2015_2022") %>% 
  mutate(ausl = ifelse(StaatsangehoerigkeitID==8100,"Schweiz","Ausland")) %>% 
  collect() %>% 
  group_by(Statistikjahr,ausl,BFS_Nr) %>% 
  summarise(n = sum(Anz_StBev)) %>% 
  tidyr::pivot_wider(names_from = ausl,values_from = n) %>% 
  mutate(total = Ausland+Schweiz) %>% 
  mutate(auslaenderanteil = Ausland/total*100) %>% 
  select(Statistikjahr,BFS_Nr,auslaenderanteil ) %>% 
  head(4)


