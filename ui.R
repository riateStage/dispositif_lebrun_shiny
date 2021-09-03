#####################################
######## Shiny - Dispositifs ######## 
#####################################


# /!\ Note : 
# Ce script fait appel à des données contenus dans les dossiers "geom/output" et "Dispositifs/Output"


# Packages

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(sf)
library(dplyr)
library(cartography)
library(readxl)


# Préparation des données


## Paris


### Ouverture des bases
fuaPa_DC <- read_xlsx("input/Dispositifs_Paris.xlsx", sheet = "Brut")
fuaPa_COM <- st_read("input/geom_Paris_FUA.gpkg", layer = "comFu")
fuaPa_DEP <- st_read("input/geom_Paris_FUA.gpkg", layer = "depFu")
fuaPa_MET <- st_read("input/geom_Paris_FUA.gpkg", layer = "metroFu")
fuaPa_STUDY <- st_read("input/geom_Paris_FUA.gpkg", layer = "studyFu")
fuaPa_DC <- subset(fuaPa_DC, fuaPa_DC$CODGEO_2019 == fuaPa_DC$CODGEO_2003)

### Jointure
fuaPa_DC <- merge(fuaPa_DC, fuaPa_COM, by.x = "CODGEO_2019", by.y = "INSEE_COM", all.x = T)
fuaPa_DC <- fuaPa_DC[,c(1:58,70)]
fuaPa_DC <- st_as_sf(fuaPa_DC)

names(fuaPa_DC)[59] <- "geometry"

st_geometry(fuaPa_DC) <- "geometry"

### Allègement de la base : suppression des variables inutiles car redondantes (cette partie est spécifique à chaque territoire)
fuaPa_DC <- fuaPa_DC[,c("CODGEO_2019", "LIBGEO_2019", "ZONE_ABC_2003", "ZONE_ABC_2006", "ZONE_ABC_2009", "ZONE_ABC_2010", "ZONE_ABC_2014", "ZONE_ABC_2019", "ZONE_123_2003", "ZONE_123_2005", "ACV", "ORT", "POPULATION", "DATE_AGREM_P", "DATE_AGREM_M")]


# Fusion des arrondissements parisiens

Paris <- subset(fuaPa_DC, substr(fuaPa_DC$LIBGEO_2019, 0,5) == "Paris")
Paris <- aggregate(Paris,  by = list(Paris$ZONE_ABC_2014), FUN = head, 1)
Paris$LIBGEO_2019 <- "Paris"
Paris$CODGEO_2019 <- "75056"
Paris$POPULATION <- 2210875
Paris <- Paris[,c(2:17)]

fuaPa_DC <- subset(fuaPa_DC, substr(fuaPa_DC$LIBGEO_2019, 0,5) != "Paris")
fuaPa_DC <- rbind(fuaPa_DC, Paris)
remove(Paris)

### Création des champs éligibilité à l'investissement locatif (ELIG_IL), à l'APL accession (ELIG_APL), au PTZ dans l'ancien (ELIG_PTZA)
fuaPa_DC$ELIG_IL_2003 <- "Oui"

fuaPa_DC$ELIG_IL_2009[fuaPa_DC$ZONE_ABC_2009 != "C"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2009[fuaPa_DC$ZONE_ABC_2009 == "C"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2012[fuaPa_DC$ZONE_ABC_2009 != "C"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2012[fuaPa_DC$ZONE_ABC_2009 == "C"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2013[fuaPa_DC$ZONE_ABC_2009 != "C" & fuaPa_DC$ZONE_ABC_2009 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2013[fuaPa_DC$ZONE_ABC_2009 == "C" | fuaPa_DC$ZONE_ABC_2009 == "B2"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2014[fuaPa_DC$ZONE_ABC_2014 != "C" & fuaPa_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2014[fuaPa_DC$ZONE_ABC_2014 == "C" | fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2015[fuaPa_DC$ZONE_ABC_2014 != "C" & fuaPa_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2015[fuaPa_DC$ZONE_ABC_2014 == "C" | fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2016[fuaPa_DC$ZONE_ABC_2014 != "C" & fuaPa_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2016[fuaPa_DC$ZONE_ABC_2014 == "C" | fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2017[fuaPa_DC$ZONE_ABC_2014 != "C" & fuaPa_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2017[fuaPa_DC$ZONE_ABC_2014 == "C" | fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2018[fuaPa_DC$ZONE_ABC_2014 != "C" & fuaPa_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2018[fuaPa_DC$ZONE_ABC_2014 == "C" | fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaPa_DC$ELIG_IL_2019[fuaPa_DC$ZONE_ABC_2019 != "C" & fuaPa_DC$ZONE_ABC_2019 != "B2"] <- "Eligible\n(zonage)"
fuaPa_DC$ELIG_IL_2019[fuaPa_DC$ZONE_ABC_2019 == "C" | fuaPa_DC$ZONE_ABC_2019 == "B2"] <- "Non éligible"


## Modification de la variable éligibilité
fuaPa_DC$ELIG_IL_2012[fuaPa_DC$DATE_AGREM_M == "2012"] <- "Eligible\n(agrément)" # Une seule commune de la FUA de Paris a reçu cet agrément, en 2012

fuaPa_DC$ELIG_IL_2013[fuaPa_DC$DATE_AGREM_P == "2013"] <- "Eligible\n(agrément)"

fuaPa_DC$ELIG_IL_2013[fuaPa_DC$DATE_AGREM_P == "2013"] <- "Eligible\n(agrément)"

fuaPa_DC$ELIG_IL_2014[fuaPa_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaPa_DC$ELIG_IL_2014[fuaPa_DC$DATE_AGREM_P == "2013" & fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"

fuaPa_DC$ELIG_IL_2015[fuaPa_DC$DATE_AGREM_P == "2015" | fuaPa_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaPa_DC$ELIG_IL_2015[fuaPa_DC$DATE_AGREM_P == "2013" & fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"

fuaPa_DC$ELIG_IL_2016[fuaPa_DC$DATE_AGREM_P == "2015" | fuaPa_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaPa_DC$ELIG_IL_2016[fuaPa_DC$DATE_AGREM_P == "2013" & fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"

fuaPa_DC$ELIG_IL_2017[fuaPa_DC$DATE_AGREM_P == "2015" | fuaPa_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaPa_DC$ELIG_IL_2017[fuaPa_DC$DATE_AGREM_P == "2013" & fuaPa_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"


### Création d'un champ sur l'éligibilité au Denormandie (ELIG_DEN) à partir des champs ACV et ORT (est éligible au Denormandie une commune qui est associé au ACV ou à une ORT - cas qu'on veut dissocier)
fuaPa_DC$ELIG_DEN <- "Non"
fuaPa_DC$ELIG_DEN[fuaPa_DC$ACV == "Oui" & fuaPa_DC$ORT == "Oui"] <- "ACV et ORT"
fuaPa_DC$ELIG_DEN[fuaPa_DC$ACV == "Oui" & fuaPa_DC$ORT == "Non"] <- "ACV seul"
fuaPa_DC$ELIG_DEN[fuaPa_DC$ACV == "Non" & fuaPa_DC$ORT == "Oui"] <- "ORT seule"


### Création d'un champ sur le changement de statut d'éligibilité aux aides fiscales à l'investissement locatif par rapport à l'année précédente (CHG_ELIG) ; on procède année par année de 2009 à 2019

fuaPa_DC$CHG_ELIG_2009 <- "Pas de\nchangement"
fuaPa_DC$CHG_ELIG_2009[fuaPa_DC$ELIG_IL_2009 == "Non éligible"] <- "Sorties\n(loi Scellier)"

# Aucun changement en 2010 et 2012
fuaPa_DC$CHG_ELIG_2010 <- "Pas de\nchangement"
fuaPa_DC$CHG_ELIG_2011 <- "Pas de\nchangement"

fuaPa_DC$CHG_ELIG_2012 <- ifelse(fuaPa_DC$ELIG_IL_2012 == fuaPa_DC$ELIG_IL_2009, "Pas de\nchangement", "Entrées\n(agrément)") # 1 seul changement lié à un agrément

fuaPa_DC$CHG_ELIG_2013 <- ifelse(fuaPa_DC$ELIG_IL_2013 == fuaPa_DC$ELIG_IL_2012, "Pas de\nchangement", "Changement")
fuaPa_DC$CHG_ELIG_2013 <- ifelse(fuaPa_DC$ELIG_IL_2013 == "Non éligible" & fuaPa_DC$CHG_ELIG_2013 == "Changement" & fuaPa_DC$ELIG_IL_2012 == "Eligible\n(agrément)", "Sorties\n(fin agrément)", fuaPa_DC$CHG_ELIG_2013)
fuaPa_DC$CHG_ELIG_2013 <- ifelse(fuaPa_DC$ELIG_IL_2013 == "Non éligible" & fuaPa_DC$CHG_ELIG_2013 == "Changement" & fuaPa_DC$ELIG_IL_2012 == "Eligible\n(zonage)", "Sorties\n(loi Duflot)", fuaPa_DC$CHG_ELIG_2013)
fuaPa_DC$CHG_ELIG_2013 <- ifelse(fuaPa_DC$ELIG_IL_2013 == "Eligible\n(agrément)" & fuaPa_DC$CHG_ELIG_2013 == "Changement", "Sorties\npar loi Duflot\net entrées\npar agrément", fuaPa_DC$CHG_ELIG_2013)

fuaPa_DC$CHG_ELIG_2014 <- ifelse(fuaPa_DC$ELIG_IL_2014 == fuaPa_DC$ELIG_IL_2013, "Pas de\nchangement", "Changement")
fuaPa_DC$CHG_ELIG_2014 <- ifelse(fuaPa_DC$ELIG_IL_2014 == "Eligible\n(zonage)" & fuaPa_DC$CHG_ELIG_2014 == "Changement", "Entrées\n(zonage)", fuaPa_DC$CHG_ELIG_2014)
fuaPa_DC$CHG_ELIG_2014 <- ifelse(fuaPa_DC$ELIG_IL_2014 == "Eligible\n(agrément)" & fuaPa_DC$CHG_ELIG_2014 == "Changement", "Entrées\n(agrément)", fuaPa_DC$CHG_ELIG_2014)
fuaPa_DC$CHG_ELIG_2014 <- ifelse(fuaPa_DC$ELIG_IL_2014 == "Non éligible" & fuaPa_DC$CHG_ELIG_2014 == "Changement", "Sorties\n(zonage)", fuaPa_DC$CHG_ELIG_2014) # Les seules sorties possibles en 2014 sont par zonage

fuaPa_DC$CHG_ELIG_2015 <- ifelse(fuaPa_DC$ELIG_IL_2015 == fuaPa_DC$ELIG_IL_2014, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaPa_DC$CHG_ELIG_2016 <- ifelse(fuaPa_DC$ELIG_IL_2016 == fuaPa_DC$ELIG_IL_2015, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaPa_DC$CHG_ELIG_2017 <- ifelse(fuaPa_DC$ELIG_IL_2017 == fuaPa_DC$ELIG_IL_2016, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaPa_DC$CHG_ELIG_2018 <- ifelse(fuaPa_DC$ELIG_IL_2018 == fuaPa_DC$ELIG_IL_2017, "Pas de\nchangement", "Sorties\n(agrément)") # Les seules sorties possibles cette année le sont pas agrément (fin du dispositif des agréments)

fuaPa_DC$CHG_ELIG_2019 <- ifelse(fuaPa_DC$ELIG_IL_2019 == fuaPa_DC$ELIG_IL_2018, "Pas de\nchangement", "Changement")



### Création de l'étiquette concernant les noms de départements (on procède à la main par gain de temps)
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "02"] <- "Aisne (02)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "10"] <- "Aube (10)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "27"] <- "Eure (27)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "28"] <- "Eure-et-Loir (28)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "41"] <- "Loir-et-Cher (41)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "45"] <- "Loiret (45)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "51"] <- "Marne (51)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "60"] <- "Oise (60)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "75"] <- "Paris (75)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "76"] <- "Seine-Maritime (76)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "77"] <- "Seine-et-Marne (77)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "78"] <- "Yvelines (78)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "89"] <- "Yonne (89)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "91"] <- "Essonne (91)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "92"] <- "Hauts-de-Seine (92)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "93"] <- "Seine-Saint-Denis (93)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "94"] <- "Val-de-Marne (94)"
fuaPa_DEP$NOM_DEP[fuaPa_DEP$DEP == "95"] <- "Val-d'Oise (95)"

### Les secteurs TVA réduite
QPV_Pa <- st_read("input/Paris_TVA.gpkg", layer = "QPV_Fu")
PNRU_Pa <- st_read("input/Paris_TVA.gpkg", layer = "PNRU_Fu")
PNRU_Pa$elig_fin <- PNRU_Pa$date_fin + 2

### Pour l'affichage des noms de communes
fuaPa_Names = fuaPa_DC %>% 
  select(CODGEO_2019, LIBGEO_2019,POPULATION, geometry) %>%
  mutate(DEP = substr(fuaPa_DC$CODGEO_2019,0,2)) %>%
  arrange(desc(POPULATION)) %>% 
  group_by(DEP) %>% slice(1)


## Lyon


### Ouverture des bases
fuaLy_DC <- read_xlsx("input/Dispositifs_Lyon.xlsx", sheet = "Brut")
fuaLy_COM <- st_read("input/geom_Lyon_FUA.gpkg", layer = "comFu")
fuaLy_DEP <- st_read("input/geom_Lyon_FUA.gpkg", layer = "depFu")
fuaLy_MET <- st_read("input/geom_Lyon_FUA.gpkg", layer = "metroFu")
fuaLy_STUDY <- st_read("input/geom_Lyon_FUA.gpkg", layer = "studyFu")
fuaLy_DC <- subset(fuaLy_DC, fuaLy_DC$CODGEO_2019 == fuaLy_DC$CODGEO_2003)

### Jointure
fuaLy_DC <- merge(fuaLy_DC, fuaLy_COM, by.x = "CODGEO_2019", by.y = "INSEE_COM", all.x = T)
fuaLy_DC <- fuaLy_DC[,c(1:58,70)]
fuaLy_DC <- st_as_sf(fuaLy_DC)

names(fuaLy_DC)[59] <- "geometry"
st_geometry(fuaLy_DC) <- "geometry"

### Allègement de la base : suppression des variables inutiles car redondantes (cette partie est spécifique à chaque territoire)
fuaLy_DC <- fuaLy_DC[,c("CODGEO_2019", "LIBGEO_2019", "ZONE_ABC_2003", "ZONE_ABC_2006", "ZONE_ABC_2009", "ZONE_ABC_2014", "ZONE_ABC_2019", "ZONE_123_2003", "ACV", "ORT", "POPULATION", "DATE_AGREM_P", "DATE_AGREM_M")]


# Fusion des arrondissements lyonnais

Lyon <- subset(fuaLy_DC, substr(fuaLy_DC$LIBGEO_2019, 0,4) == "Lyon")
Lyon <- aggregate(Lyon,  by = list(Lyon$ZONE_ABC_2014), FUN = head, 1)
Lyon$LIBGEO_2019 <- "Lyon"
Lyon$CODGEO_2019 <- "69123"
Lyon$POPULATION <- 523164

Lyon <- Lyon[,c(2:15)]

fuaLy_DC <- subset(fuaLy_DC, substr(fuaLy_DC$LIBGEO_2019, 0,4) != "Lyon")
fuaLy_DC <- rbind(fuaLy_DC, Lyon)
remove(Lyon)

### Création des champs éligibilité à l'investissement locatif (ELIG_IL), à l'APL accession (ELIG_APL), au PTZ dans l'ancien (ELIG_PTZA)
fuaLy_DC$ELIG_IL_2003 <- "Oui"

fuaLy_DC$ELIG_IL_2009[fuaLy_DC$ZONE_ABC_2009 != "C"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2009[fuaLy_DC$ZONE_ABC_2009 == "C"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2012[fuaLy_DC$ZONE_ABC_2009 != "C"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2012[fuaLy_DC$ZONE_ABC_2009 == "C"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2013[fuaLy_DC$ZONE_ABC_2009 != "C" & fuaLy_DC$ZONE_ABC_2009 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2013[fuaLy_DC$ZONE_ABC_2009 == "C" | fuaLy_DC$ZONE_ABC_2009 == "B2"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2014[fuaLy_DC$ZONE_ABC_2014 != "C" & fuaLy_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2014[fuaLy_DC$ZONE_ABC_2014 == "C" | fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2015[fuaLy_DC$ZONE_ABC_2014 != "C" & fuaLy_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2015[fuaLy_DC$ZONE_ABC_2014 == "C" | fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2016[fuaLy_DC$ZONE_ABC_2014 != "C" & fuaLy_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2016[fuaLy_DC$ZONE_ABC_2014 == "C" | fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2017[fuaLy_DC$ZONE_ABC_2014 != "C" & fuaLy_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2017[fuaLy_DC$ZONE_ABC_2014 == "C" | fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2018[fuaLy_DC$ZONE_ABC_2014 != "C" & fuaLy_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2018[fuaLy_DC$ZONE_ABC_2014 == "C" | fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaLy_DC$ELIG_IL_2019[fuaLy_DC$ZONE_ABC_2019 != "C" & fuaLy_DC$ZONE_ABC_2019 != "B2"] <- "Eligible\n(zonage)"
fuaLy_DC$ELIG_IL_2019[fuaLy_DC$ZONE_ABC_2019 == "C" | fuaLy_DC$ZONE_ABC_2019 == "B2"] <- "Non éligible"


## Modification de la variable éligibilité
fuaLy_DC$ELIG_IL_2012[fuaLy_DC$DATE_AGREM_M == "2012"] <- "Eligible\n(agrément)" # 5 communes ont reçu cet agrément, toutes en 2012.

fuaLy_DC$ELIG_IL_2013[fuaLy_DC$DATE_AGREM_P == "2013"] <- "Eligible\n(agrément)"

fuaLy_DC$ELIG_IL_2014[fuaLy_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaLy_DC$ELIG_IL_2014[fuaLy_DC$DATE_AGREM_P == "2013" & fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"

fuaLy_DC$ELIG_IL_2015[fuaLy_DC$DATE_AGREM_P == "2015" | fuaLy_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaLy_DC$ELIG_IL_2015[fuaLy_DC$DATE_AGREM_P == "2013" & fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"

fuaLy_DC$ELIG_IL_2016[fuaLy_DC$DATE_AGREM_P == "2016" | fuaLy_DC$DATE_AGREM_P == "2015" | fuaLy_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaLy_DC$ELIG_IL_2016[fuaLy_DC$DATE_AGREM_P == "2013" & fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"

fuaLy_DC$ELIG_IL_2017[fuaLy_DC$DATE_AGREM_P == "2016" | fuaLy_DC$DATE_AGREM_P == "2015" | fuaLy_DC$DATE_AGREM_P == "2014"] <- "Eligible\n(agrément)"
fuaLy_DC$ELIG_IL_2017[fuaLy_DC$DATE_AGREM_P == "2013" & fuaLy_DC$ZONE_ABC_2014 == "B2"] <- "Eligible\n(agrément)"


### Création d'un champ sur l'éligibilité au Denormandie (ELIG_DEN) à partir des champs ACV et ORT (est éligible au Denormandie une commune qui est associé au ACV ou à une ORT - cas qu'on veut dissocier)
fuaLy_DC$ELIG_DEN <- "Non"
fuaLy_DC$ELIG_DEN[fuaLy_DC$ACV == "Oui" & fuaLy_DC$ORT == "Oui"] <- "ACV et ORT"
fuaLy_DC$ELIG_DEN[fuaLy_DC$ACV == "Oui" & fuaLy_DC$ORT == "Non"] <- "ACV seul"
fuaLy_DC$ELIG_DEN[fuaLy_DC$ACV == "Non" & fuaLy_DC$ORT == "Oui"] <- "ORT seule"



### Création d'un champ sur le changement de statut d'éligibilité aux aides fiscales à l'investissement locatif par rapport à l'année précédente (CHG_ELIG) ; on procède année par année de 2009 à 2019

fuaLy_DC$CHG_ELIG_2009 <- "Pas de\nchangement"
fuaLy_DC$CHG_ELIG_2009[fuaLy_DC$ELIG_IL_2009 == "Non éligible"] <- "Sorties\n(loi Scellier)"

# Aucun changement en 2010 et 2012
fuaLy_DC$CHG_ELIG_2010 <- "Pas de\nchangement"
fuaLy_DC$CHG_ELIG_2011 <- "Pas de\nchangement"

fuaLy_DC$CHG_ELIG_2012 <- ifelse(fuaLy_DC$ELIG_IL_2012 == fuaLy_DC$ELIG_IL_2009, "Pas de\nchangement", "Entrées\n(agrément)") # 1 seul changement lié à un agrément

fuaLy_DC$CHG_ELIG_2013 <- ifelse(fuaLy_DC$ELIG_IL_2013 == fuaLy_DC$ELIG_IL_2012, "Pas de\nchangement", "Changement")
fuaLy_DC$CHG_ELIG_2013 <- ifelse(fuaLy_DC$ELIG_IL_2013 == "Non éligible" & fuaLy_DC$CHG_ELIG_2013 == "Changement" & fuaLy_DC$ELIG_IL_2012 == "Eligible\n(agrément)", "Sorties\n(fin agrément)", fuaLy_DC$CHG_ELIG_2013)
fuaLy_DC$CHG_ELIG_2013 <- ifelse(fuaLy_DC$ELIG_IL_2013 == "Non éligible" & fuaLy_DC$CHG_ELIG_2013 == "Changement" & fuaLy_DC$ELIG_IL_2012 == "Eligible\n(zonage)", "Sorties\n(loi Duflot)", fuaLy_DC$CHG_ELIG_2013)
fuaLy_DC$CHG_ELIG_2013 <- ifelse(fuaLy_DC$ELIG_IL_2013 == "Eligible\n(agrément)" & fuaLy_DC$CHG_ELIG_2013 == "Changement", "Sorties\npar loi Duflot\net entrées\npar agrément", fuaLy_DC$CHG_ELIG_2013) # Les seules communes agrées en 2013 étaient éligibles sur zonage jusqu'à l'année précédente


fuaLy_DC$CHG_ELIG_2014 <- ifelse(fuaLy_DC$ELIG_IL_2014 == fuaLy_DC$ELIG_IL_2013, "Pas de\nchangement", "Changement")
fuaLy_DC$CHG_ELIG_2014 <- ifelse(fuaLy_DC$ELIG_IL_2014 == "Eligible\n(zonage)" & fuaLy_DC$CHG_ELIG_2014 == "Changement", "Entrées\n(zonage)", fuaLy_DC$CHG_ELIG_2014)
fuaLy_DC$CHG_ELIG_2014 <- ifelse(fuaLy_DC$ELIG_IL_2014 == "Eligible\n(agrément)" & fuaLy_DC$CHG_ELIG_2014 == "Changement", "Entrées\n(agrément)", fuaLy_DC$CHG_ELIG_2014)
fuaLy_DC$CHG_ELIG_2014 <- ifelse(fuaLy_DC$ELIG_IL_2014 == "Non éligible" & fuaLy_DC$CHG_ELIG_2014 == "Changement", "Sorties\n(zonage)", fuaLy_DC$CHG_ELIG_2014) # Les seules sorties possibles en 2014 sont pas zonage

fuaLy_DC$CHG_ELIG_2015 <- ifelse(fuaLy_DC$ELIG_IL_2015 == fuaLy_DC$ELIG_IL_2014, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaLy_DC$CHG_ELIG_2016 <- ifelse(fuaLy_DC$ELIG_IL_2016 == fuaLy_DC$ELIG_IL_2015, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaLy_DC$CHG_ELIG_2017 <- ifelse(fuaLy_DC$ELIG_IL_2017 == fuaLy_DC$ELIG_IL_2016, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaLy_DC$CHG_ELIG_2018 <- ifelse(fuaLy_DC$ELIG_IL_2018 == fuaLy_DC$ELIG_IL_2017, "Pas de\nchangement", "Sorties\n(agrément)") # Les seules sorties possibles cette année le sont pas agrément (fin du dispositif des agréments)

fuaLy_DC$CHG_ELIG_2019 <- ifelse(fuaLy_DC$ELIG_IL_2019 == fuaLy_DC$ELIG_IL_2018, "Pas de\nchangement", "Entrées\n(zonage)")


### Création de l'étiquette concernant les noms de départements (on procède à la main par gain de temps)
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "01"] <- "Ain (01)"
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "07"] <- "Ardèche (07)"
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "38"] <- "Isère (38)"
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "42"] <- "Loire (42)"
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "43"] <- "Haute-Loire (43)"
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "69"] <- "Rhône (69)"
fuaLy_DEP$NOM_DEP[fuaLy_DEP$DEP == "71"] <- "Saône-et-Loire (71)"


### Les secteurs TVA réduite
QPV_Ly <- st_read("input/Lyon_TVA.gpkg", layer = "QPV_Fu")
PNRU_Ly <- st_read("input/Lyon_TVA.gpkg", layer = "PNRU_Fu")

PNRU_Ly$elig_fin <- PNRU_Ly$date_fin + 2


### Pour l'affichage des noms de communes
fuaLy_Names = fuaLy_DC %>% 
  select(CODGEO_2019, LIBGEO_2019,POPULATION, geometry) %>%
  mutate(DEP = substr(fuaLy_DC$CODGEO_2019,0,2)) %>%
  arrange(desc(POPULATION)) %>% 
  group_by(DEP) %>% slice(1:2)



## Avignon

### Ouverture des bases
fuaAv_DC <- read_xlsx("input/Dispositifs_Avignon.xlsx")
fuaAv_COM <- st_read("input/geom_Avignon_FUA.gpkg", layer = "comFu")
fuaAv_DEP <- st_read("input/geom_Avignon_FUA.gpkg", layer = "depFu")
fuaAv_MET <- st_read("input/geom_Avignon_FUA.gpkg", layer = "metroFu")
fuaAv_STUDY <- st_read("input/geom_Avignon_FUA.gpkg", layer = "studyFu")

### Jointure
fuaAv_DC <- merge(fuaAv_DC, fuaAv_COM, by.x = "CODGEO_2019", by.y = "INSEE_COM", all.x = T)
fuaAv_DC <- fuaAv_DC[,c(1:55,70)]
fuaAv_DC <- st_as_sf(fuaAv_DC)


### Allègement de la base : suppression des variables inutiles car redondantes (cette partie est spécifique à chaque territoire)
fuaAv_DC <- fuaAv_DC[,c("CODGEO_2019", "LIBGEO_2019", "ZONE_ABC_2003", "ZONE_ABC_2006", "ZONE_ABC_2009", "ZONE_ABC_2014", "ZONE_123_2003", "ACV", "POPULATION", "DATE_AGREM")]


### Création des champs éligibilité à l'investissement locatif (ELIG_IL), à l'APL accession (ELIG_APL), au PTZ dans l'ancien (ELIG_PTZA)
fuaAv_DC$ELIG_IL_2003 <- "Oui"

fuaAv_DC$ELIG_IL_2009[fuaAv_DC$ZONE_ABC_2009 != "C"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2009[fuaAv_DC$ZONE_ABC_2009 == "C"] <- "Non éligible"

fuaAv_DC$ELIG_IL_2013[fuaAv_DC$ZONE_ABC_2009 != "C" & fuaAv_DC$ZONE_ABC_2009 != "B2"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2013[fuaAv_DC$ZONE_ABC_2009 == "C" | fuaAv_DC$ZONE_ABC_2009 == "B2"] <- "Non éligible"

fuaAv_DC$ELIG_IL_2014[fuaAv_DC$ZONE_ABC_2014 != "C" & fuaAv_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2014[fuaAv_DC$ZONE_ABC_2014 == "C" | fuaAv_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaAv_DC$ELIG_IL_2015[fuaAv_DC$ZONE_ABC_2014 != "C" & fuaAv_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2015[fuaAv_DC$ZONE_ABC_2014 == "C" | fuaAv_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaAv_DC$ELIG_IL_2016[fuaAv_DC$ZONE_ABC_2014 != "C" & fuaAv_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2016[fuaAv_DC$ZONE_ABC_2014 == "C" | fuaAv_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaAv_DC$ELIG_IL_2017[fuaAv_DC$ZONE_ABC_2014 != "C" & fuaAv_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2017[fuaAv_DC$ZONE_ABC_2014 == "C" | fuaAv_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"

fuaAv_DC$ELIG_IL_2018[fuaAv_DC$ZONE_ABC_2014 != "C" & fuaAv_DC$ZONE_ABC_2014 != "B2"] <- "Eligible\n(zonage)"
fuaAv_DC$ELIG_IL_2018[fuaAv_DC$ZONE_ABC_2014 == "C" | fuaAv_DC$ZONE_ABC_2014 == "B2"] <- "Non éligible"


## Modification de la variable éligibilité
fuaAv_DC$ELIG_IL_2013[fuaAv_DC$DATE_AGREM == "2013"] <- "Eligible\n(agrément)"
fuaAv_DC$ELIG_IL_2015[fuaAv_DC$DATE_AGREM == "2015"] <- "Eligible\n(agrément)"
fuaAv_DC$ELIG_IL_2016[fuaAv_DC$DATE_AGREM == "2015"] <- "Eligible\n(agrément)"
fuaAv_DC$ELIG_IL_2017[fuaAv_DC$DATE_AGREM == "2015"] <- "Eligible\n(agrément)"


### Création du champ pour le Denormandie (surtout au cas où des ORT seraient un jour signées sur le territoire)
fuaAv_DC$ELIG_DEN <- "Non"
fuaAv_DC$ELIG_DEN[fuaAv_DC$ACV == "Oui"] <- "ACV seul"



### Création d'un champ sur le changement de statut d'éligibilité aux aides fiscales à l'investissement locatif par rapport à l'année précédente (CHG_ELIG) ; on procède année par année de 2009 à 2019

fuaAv_DC$CHG_ELIG_2009 <- "Pas de\nchangement"
fuaAv_DC$CHG_ELIG_2009[fuaAv_DC$ELIG_IL_2009 == "Non éligible"] <- "Sorties\n(loi Scellier)"

# Aucun changement en 2010 et 2013
fuaAv_DC$CHG_ELIG_2010 <- "Pas de\nchangement"
fuaAv_DC$CHG_ELIG_2011 <- "Pas de\nchangement"
fuaAv_DC$CHG_ELIG_2012 <- "Pas de\nchangement"

fuaAv_DC$CHG_ELIG_2013 <- ifelse(fuaAv_DC$ELIG_IL_2013 == fuaAv_DC$ELIG_IL_2009, "Pas de\nchangement", "Changement")
fuaAv_DC$CHG_ELIG_2013 <- ifelse(fuaAv_DC$ELIG_IL_2013 == "Non éligible" & fuaAv_DC$CHG_ELIG_2013 == "Changement" & fuaAv_DC$ELIG_IL_2009 == "Eligible\n(zonage)", "Sorties\n(loi Duflot)", fuaAv_DC$CHG_ELIG_2013)
fuaAv_DC$CHG_ELIG_2013 <- ifelse(fuaAv_DC$ELIG_IL_2013 == "Eligible\n(agrément)" & fuaAv_DC$CHG_ELIG_2013 == "Changement", "Sorties\npar loi Duflot\net entrées\npar agrément", fuaAv_DC$CHG_ELIG_2013) # Les seules communes agrées en 2013 étaient éligibles sur zonage jusqu'à l'année précédente

fuaAv_DC$CHG_ELIG_2014 <- ifelse(fuaAv_DC$ELIG_IL_2014 == fuaAv_DC$ELIG_IL_2013, "Pas de\nchangement", "Changement")
fuaAv_DC$CHG_ELIG_2014 <- ifelse(fuaAv_DC$ELIG_IL_2014 == "Eligible\n(zonage)" & fuaAv_DC$CHG_ELIG_2014 == "Changement", "Entrées\n(zonage)", fuaAv_DC$CHG_ELIG_2014)
fuaAv_DC$CHG_ELIG_2014 <- ifelse(fuaAv_DC$ELIG_IL_2014 == "Eligible\n(agrément)" & fuaAv_DC$CHG_ELIG_2014 == "Changement", "Entrées\n(agrément)", fuaAv_DC$CHG_ELIG_2014)
fuaAv_DC$CHG_ELIG_2014 <- ifelse(fuaAv_DC$ELIG_IL_2014 == "Non éligible" & fuaAv_DC$CHG_ELIG_2014 == "Changement", "Sorties\n(zonage)", fuaAv_DC$CHG_ELIG_2014) # Les seules sorties possibles en 2014 sont pas zonage

fuaAv_DC$CHG_ELIG_2015 <- ifelse(fuaAv_DC$ELIG_IL_2015 == fuaAv_DC$ELIG_IL_2014, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaAv_DC$CHG_ELIG_2016 <- ifelse(fuaAv_DC$ELIG_IL_2016 == fuaAv_DC$ELIG_IL_2015, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaAv_DC$CHG_ELIG_2017 <- ifelse(fuaAv_DC$ELIG_IL_2017 == fuaAv_DC$ELIG_IL_2016, "Pas de\nchangement", "Entrées\n(agrément)") # Les seules entrées possibles cette année le sont pas agrément

fuaAv_DC$CHG_ELIG_2018 <- ifelse(fuaAv_DC$ELIG_IL_2018 == fuaAv_DC$ELIG_IL_2017, "Pas de\nchangement", "Sorties\n(agrément)") # Les seules sorties possibles cette année le sont pas agrément (fin du dispositif des agréments)

fuaAv_DC$CHG_ELIG_2019 <- "Pas de\nchangement"


### Création de l'étiquette concernant les noms de départements (on procède à la main par gain de temps)
fuaAv_DEP$NOM_DEP[fuaAv_DEP$DEP == "13"] <- "Bouches-du-Rhône (13)"
fuaAv_DEP$NOM_DEP[fuaAv_DEP$DEP == "30"] <- "Gard (30)"
fuaAv_DEP$NOM_DEP[fuaAv_DEP$DEP == "84"] <- "Vaucluse (84)"

### Les secteurs TVA réduite
QPV_Av <- st_read("input/Avignon_TVA.gpkg", layer = "QPV_Fu")
PNRU_Av <- st_read("input/Avignon_TVA.gpkg", layer = "PNRU_Fu")
PNRU_Av$elig_fin <- PNRU_Av$date_fin + 2


### Pour l'affichage des noms de communes
fuaAv_Names = fuaAv_DC %>% 
  select(1, 2,9, 11) %>%
  mutate(DEP = substr(fuaAv_DC$CODGEO_2019,0,2)) %>%
  arrange(desc(POPULATION)) %>% 
  group_by(DEP) %>% slice(1:3)




# Exécution du shiny

ui <- fluidPage(
  
  tags$h1("Les dispositifs de soutien à l'investissement immobilier"),
  
  helpText(
    p("Travail réalisé dans le cadre du programme ANR PRC",
      a("WIsDHoM (ANR-18-CE41-0004)",     href="https://anr.fr/Project-ANR-18-CE41-0004", target="_blank"),
      "- Wealth Inequalities and the Dynamics of Housing Market. Interpreting real-estate market-based regime of spatial inequalities 2019-2022."),
  ),
  
  tags$br(),
                      sidebarPanel(width = 3,
                                   
                                   radioButtons(inputId = "MapArea",
                                                label = "Aire à cartographier (FUA)",
                                                choices = c("Paris", "Lyon", "Avignon"),
                                                selected = "Paris"), 
                                   
                                   radioButtons(inputId = "MapObj",
                                                label = "Objet de la carte",
                                                choices = c("Zonages", "Dispositifs"),
                                                selected = "Zonages"), 
                                   
                                   conditionalPanel('input.MapObj == "Zonages"', 
                                                    radioButtons(inputId = "Zonage",
                                                                 label = "Type de zonage",
                                                                 choices = c("Classement A/B/C", "Classement 1/2/3"),
                                                                 selected = "Classement A/B/C")),
                                   
                                   conditionalPanel('input.MapObj == "Zonages" & input.Zonage == "Classement A/B/C"', helpText("Le classement A/B/C a été instauré en 2003 et modifié en 2006, 2009, 2010 et 2014.")),
                                   
                                   conditionalPanel('input.MapObj == "Zonages" & input.Zonage == "Classement 1/2/3"', helpText("Le classement 1/2/3 n'a connu aucune modification entre 2003 et 2019 sur les FUA de Lyon et d'Avignon.")),
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs"', 
                                                    radioButtons(inputId = "Dispositif",
                                                                 label = "Type de dispositif",
                                                                 choices = c("Aide aux investissements locatifs (Scellier, Duflot, Pinel)", "TVA réduite", "APL accession", "PTZ dans le secteur ancien", "Aide à la rénovation (Denormandie)"),
                                                                 selected = "Aide aux investissements locatifs (Scellier, Duflot, Pinel)")),
                                   
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs" & input.Dispositif == "Aide aux investissements locatifs (Scellier, Duflot, Pinel)"', 
                                                    radioButtons(inputId = "Changements",
                                                                 label = "Mode de représentation",
                                                                 choices = c("Eligibilité au 31 décembre", "Changement par rapport à l'année précédente"),
                                                                 selected = "Eligibilité au 31 décembre")),
                                   
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs" & input.Dispositif == "Aide aux investissements locatifs (Scellier, Duflot, Pinel)"', helpText("L'aide à l'investissement locatif est restreinte sur des critères géographiques depuis 2009. Les conditions d'éligibilité ont été modifiées en 2009 (loi Scellier), 2013 (loi Duflot), et 2018 (loi Pinel II)")),
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs" & input.Dispositif == "TVA réduite"', helpText("La TVA réduite s'applique aux secteurs ANRU (+500m jusqu'en 2013 ; +300m ensuite) et aux quartiers prioritaires des politiques de la ville créés en 2015 (+300m ; +500m depuis 2017 s'il s'agit d'un secteur du NPNRU).")),
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs" & input.Dispositif == "APL accession"', helpText("L'APL accession est limitée depuis 2018 aux communes de zone 3.")),
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs" & input.Dispositif == "PTZ dans le secteur ancien"', helpText("Le PTZ dans l'ancien a été instauré en 2005 pour toutes les communes, supprimé au 1er janvier 2011, réinstauré sous des conditions plus restrictives en 2012 et limité en 2018 aux zones B2 et C.")),
                                   
                                   conditionalPanel('input.MapObj == "Dispositifs" & input.Dispositif == "Aide à la rénovation (Denormandie)"', helpText("Le dispositif Denormandie n'est entré en vigueur qu'en 2019. Y sont éligibles les communes du programme Action coeur de ville (ACV) et celles objet d'une opération de revitalisation de territoire (ORT)")),
                                   
                                   sliderInput(inputId = "annee",
                                               label = "Année",
                                               value = 2019, min = 2003, max = 2019, sep = ""),
                                   helpText("La carte correspond à la situation au 31 décembre de l'année sélectionnée."),
                                   
                                   radioButtons(inputId = "show_NamesFinder",
                                                label = "Afficher :",
                                                choices = c("Noms des communes les plus peuplées de\nchaque département (1 pour Paris,\n2 pour Lyon, 3 pour Avignon)", "Noms (et code) des départements", "Aucun"),
                                                selected = "Aucun"),
                                   
                                   downloadButton("downloadData", "Télécharger la carte (.svg)"),
                                   
                                   helpText("Attention, la carte de Paris pèse 14 Mo.")
                                   
                                   
                      ),
  
                      mainPanel(
                        
                        
                        withSpinner(plotOutput(outputId = "MainMap", width = 970, height = 750)),
                        
                      )
             
)




server <- function(input, output) {
  

  plotInput <- function(){
    
    # Cartographie de Paris
    
    if(input$MapArea == "Paris"){
      plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
      plot(st_geometry(fuaPa_STUDY), col = "lightgray", border = NA, add = TRUE)
      
      if(input$MapObj == "Zonages"){
        
        # Cartographie des zonages A/B/C
        
        if(input$Zonage == "Classement A/B/C" & input$annee >= 2014){
          typoLayer(x = fuaPa_DC, var = "ZONE_ABC_2014",
                    col = c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("A bis", "A", "B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2010:2013){
          typoLayer(x = fuaPa_DC, var = "ZONE_ABC_2010",
                    col = c("#993404","#d95f0e","#fe9929", "#fed98e", "#ffffd4"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("A bis", "A", "B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee == 2009){
          typoLayer(x = fuaPa_DC, var = "ZONE_ABC_2009",
                    col = c("#d95f0e","#fe9929", "#fed98e", "#ffffd4"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("A", "B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2006:2008){
          typoLayer(x = fuaPa_DC, var = "ZONE_ABC_2006",
                    col = c("#d95f0e","#fe9929", "#fed98e", "#ffffd4"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("A", "B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2003:2005){
          typoLayer(x = fuaPa_DC, var = "ZONE_ABC_2003",
                    col = c("#d95f0e","#feb24c", "#ffffd4"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("A", "B", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        # Cartographie des zonages 1/2/3
        
        if(input$Zonage == "Classement 1/2/3" & input$annee%in%2003:2004){
          typoLayer(x = fuaPa_DC, var = "ZONE_123_2003",
                    col = c("#4eb3d3", "#a8ddb5", "#e0f3db"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("1", "2", "3"),
                    legend.title.txt = "Zone du\nclassement\n1/2/3", add = TRUE)}
        
        if(input$Zonage == "Classement 1/2/3" & input$annee > 2004){
          typoLayer(x = fuaPa_DC, var = "ZONE_123_2005",
                    col = c("#4eb3d3", "#a8ddb5", "#e0f3db"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("1", "2", "3"),
                    legend.title.txt = "Zone du\nclassement\n1/2/3", add = TRUE)}
      }
      
      # Cartographie de l'éligibilité aux dispositifs
      
      if(input$MapObj == "Dispositifs"){
        
        # L'investissement locatif
        
        if(input$Dispositif == "Aide aux investissements locatifs (Scellier, Duflot, Pinel)") {
          
          if(input$Changements == "Eligibilité au 31 décembre") {
            
            if (input$annee < 2009) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2003",
                    col = c("#a1d99b"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Oui"),
                    legend.title.txt = "Eligibilité\nau Robien", add = TRUE)}
        
            if(input$annee%in% 2009:2011) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2009",
                    col = c("#a1d99b", "lightgray"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                    legend.title.txt = "Eligibilité\nau Scellier", add = TRUE)}
        
            if(input$annee == 2012) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2012",
                    col = c("#a1d99b", "lightgray", "#fd8d3c"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                    legend.title.txt = "Eligibilité\nau Scellier", add = TRUE)}
        
            if(input$annee == 2013) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2013",
                    col = c("#a1d99b", "lightgray", "#fd8d3c"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                    legend.title.txt = "Eligibilité\nau Duflot", add = TRUE)}
        
            if(input$annee == 2014) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2014",
                    col = c("#a1d99b", "lightgray", "#fd8d3c"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                    legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
        
            if(input$annee == 2015) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2015",
                    col = c("#a1d99b", "lightgray", "#fd8d3c"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                    legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
        
            if(input$annee == 2016) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2016",
                    col = c("#a1d99b", "lightgray", "#fd8d3c"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                    legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
        
            if(input$annee == 2017) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2017",
                    col = c("#a1d99b", "lightgray", "#fd8d3c"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                    legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
        
            if(input$annee >= 2018) {
              typoLayer(x = fuaPa_DC, var = "ELIG_IL_2018",
                    col = c("#a1d99b", "lightgray"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                    legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
        } # Fin de la représentation par les éligibilité l'année donnée
        
          
          if(input$Changements == "Changement par rapport à l'année précédente") { 
             
            if(input$annee < 2009){
              plot(st_geometry(fuaPa_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                       title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                       categ = c("Pas de\nchangement"),
                       col = c("lightgray"),
                       nodata = F, frame = FALSE, symbol = "box")}
          
          
            if(input$annee == 2009) {
              typoLayer(x = fuaPa_DC, var = "CHG_ELIG_2009",
                      col = c("lightgray", "#fb6a4a"),
                      border = NA,
                      legend.pos = "topright",
                      legend.values.order = c("Pas de\nchangement", "Sorties\n(loi Scellier)"),
                      legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
          
            if(input$annee %in% 2010:2011) {
              plot(st_geometry(fuaPa_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                       title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                       categ = c("Pas de\nchangement"),
                       col = c("lightgray"),
                       nodata = F, frame = FALSE, symbol = "box")}

            if(input$annee == 2012) {
              typoLayer(x = fuaPa_DC, var = "CHG_ELIG_2012",
                      col = c("lightgray", "#74c476"),
                      border = NA,
                      legend.pos = "topright",
                      legend.values.order = c("Pas de\nchangement", "Entrées\n(agrément)"),
                      legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
          
           if(input$annee == 2013) {
              typoLayer(x = fuaPa_DC, var = "CHG_ELIG_2013",
                      col = c("lightgray", "#fdbf6f", "#fb6a4a", "#9e9ac8"),
                      border = NA,
                      legend.pos = "topright",
                      legend.values.order = c("Pas de\nchangement", "Sorties\n(fin agrément)", "Sorties\n(loi Duflot)", "Sorties\npar loi Duflot\net entrées\npar agrément"),
                      legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
          
            if(input$annee == 2014) {
              typoLayer(x = fuaPa_DC, var = "CHG_ELIG_2014",
                      col = c("lightgray", "#74a9cf", "#74c476", "#fd8d3c"),
                      border = NA,
                      legend.pos = "topright",
                      legend.values.order = c("Pas de\nchangement", "Entrées\n(zonage)", "Entrées\n(agrément)", "Sorties\n(zonage)"),
                      legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
          
          if(input$annee == 2015) {
            typoLayer(x = fuaPa_DC, var = "CHG_ELIG_2015",
                      col = c("lightgray", "#74c476"),
                      border = NA,
                      legend.pos = "topright",
                      legend.values.order = c("Pas de\nchangement", "Entrées\n(agrément)"),
                      legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
          
          if(input$annee == 2016) {
            plot(st_geometry(fuaPa_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
            plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
            plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
            legendTypo(pos = "topright", 
                       title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                       categ = c("Pas de\nchangement"),
                       col = c("lightgray"),
                       nodata = F, frame = FALSE, symbol = "box")}
          
          if(input$annee == 2017) {
            plot(st_geometry(fuaPa_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
            plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
            plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
            legendTypo(pos = "topright", 
                       title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                       categ = c("Pas de\nchangement"),
                       col = c("lightgray"),
                       nodata = F, frame = FALSE, symbol = "box")}
          
          if(input$annee == 2018) {
            typoLayer(x = fuaPa_DC, var = "CHG_ELIG_2018",
                      col = c("lightgray", "#fdbf6f"),
                      border = NA,
                      legend.pos = "topright",
                      legend.values.order = c("Pas de\nchangement", "Sorties\n(agrément)"),
                      legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
          
          if(input$annee == 2019) {
            plot(st_geometry(fuaPa_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
            plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
            plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
            legendTypo(pos = "topright", 
                       title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                       categ = c("Pas de\nchangement"),
                       col = c("lightgray"),
                       nodata = F, frame = FALSE, symbol = "box")}
          
            }
          
          } # Fin des investissements locatifs
        
        
        # La TVA réduite
        
        if(input$Dispositif == "TVA réduite"){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaPa_DC), col = NA, lwd = 0.4, border = "grey82", add = TRUE)
          
          if(input$annee %in% 2006: 2013) {plot(st_geometry(st_buffer(PNRU_Pa[(PNRU_Pa$date_sign <= input$annee & PNRU_Pa$elig_fin >= input$annee),], 500)), col = "orange1", border = NA, add = TRUE)}
          
          if(input$annee >= 2014){
            plot(st_geometry(st_buffer(PNRU_Pa[(PNRU_Pa$date_sign <= input$annee & PNRU_Pa$elig_fin >= input$annee),], 300)), col = "orange1", border = NA, add = TRUE)
          }
          
          if(input$annee >= 2015){
            plot(st_geometry(st_buffer(QPV_Pa, 300)), col = "#74a9cf", border = NA, add = TRUE)
          }
          
          if(input$annee >= 2017){
            plot(st_geometry(st_buffer(QPV_Pa[QPV_Pa$NPNRU == "OUI",], 500)), col = "#74a9cf", border = NA, add = TRUE)}
          
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = NA,
                     categ = c("Secteurs du\nPNRU" ,"Quartier prioritaire\ndes politiques de\nla ville"),
                     col = c("orange1", "#74a9cf"), 
                     nodata = F, frame = FALSE, symbol = "box")
        }
        
        
        # L'APL accession
        
        if(input$Dispositif == "APL accession" & input$annee < 2018){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaPa_DC), col = "#c2e699", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nà l'APL\naccession",
                     categ = c("Oui"),
                     col = c("#c2e699"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "APL accession" & input$annee >= 2018){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(filter(fuaPa_DC, ZONE_123_2003 == "3")), col = "#c2e699", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nà l'APL\naccession",
                     categ = c("Oui", "Non"),
                     col = c("#c2e699", "lightgray"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        
        # Le PTZ dans le secteur ancien
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee %in% 2005:2010){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaPa_DC), col = "#bf812d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui"),
                     col = c("#bf812d"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee == 2011){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaPa_DC), col = "gray92", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Non"),
                     col = c("gray92"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee %in% 2012:2017){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaPa_DC), col = "#dfc27d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui (si\nrénovations\nlourdes"),
                     col = c("#dfc27d"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee >= 2018){
          plot(st_geometry(fuaPa_DEP), col = "white", border = NA)
          plot(st_geometry(fuaPa_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(filter(fuaPa_DC, ZONE_ABC_2014 == "B2" | ZONE_ABC_2014 == "C")), col = "#dfc27d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui", "Non"),
                     col = c("#dfc27d", "gray92"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        # Denormandie
        
        if(input$Dispositif == "Aide à la rénovation (Denormandie)" & input$annee >= 2019) {
          typoLayer(x = fuaPa_DC, var = "ELIG_DEN",
                    col = c("#8856a7","#6baed6", "#fb6a4a", "lightgray"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("ACV et ORT", "ACV seul", "ORT seule", "Non"),
                    legend.title.txt = "Eligibilité\nau\nDenormandie", add = TRUE)}
        
        
      }
      
      plot(st_geometry(fuaPa_DC), col = NA, lwd = 0.4, border = "grey", add = TRUE)
      plot(st_geometry(fuaPa_DEP), col = NA, lwd = 1, border = "gray63", add = TRUE)
      
      plot(st_geometry(fuaPa_MET), col = NA, lwd = 1, border = "black", add = TRUE)
      
      if(input$show_NamesFinder == "Noms des communes les plus peuplées de\nchaque département (1 pour Paris,\n2 pour Lyon, 3 pour Avignon)") {
        labelLayer(fuaPa_Names,
                   txt = "LIBGEO_2019",
                   halo = T,
                   cex = 0.6,
                   overlap = F)}
      
      if(input$show_NamesFinder == "Noms (et code) des départements") {labelLayer(fuaPa_DEP,
                                                                                  txt = "NOM_DEP",
                                                                                  halo = T,
                                                                                  overlap = F)}
      
    }
    
    
    
    
    
    
    # Cartographie de Lyon
    
    if(input$MapArea == "Lyon"){
      plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
      plot(st_geometry(fuaLy_STUDY), col = "lightgray", border = NA, add = TRUE)
      
      if(input$MapObj == "Zonages"){
        
        # Cartographie des zonages A/B/C
        
        if(input$Zonage == "Classement A/B/C" & input$annee >= 2014){
          typoLayer(x = fuaLy_DC, var = "ZONE_ABC_2014",
                    col = c("#d95f0e", "#fe9929", "#fed98e", "#ffffd4"),
                    border = NA,
                    legend.pos = "topright",
                    legend.values.order = c("A", "B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2009:2013){
          typoLayer(x = fuaLy_DC, var = "ZONE_ABC_2009",
                    col = c("#fe9929", "#fed98e", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2006:2008){
          typoLayer(x = fuaLy_DC, var = "ZONE_ABC_2006",
                    col = c("#fe9929", "#fed98e", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2003:2005){
          typoLayer(x = fuaLy_DC, var = "ZONE_ABC_2003",
                    col = c("#feb24c", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B", "C"), 
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        # Cartographie des zonages 1/2/3
        
        if(input$Zonage == "Classement 1/2/3"){
          typoLayer(x = fuaLy_DC, var = "ZONE_123_2003",
                    col = c("#a8ddb5", "#e0f3db"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("2", "3"),
                    legend.title.txt = "Zone du\nclassement\n1/2/3", add = TRUE)}
      }
      
      # Cartographie de l'éligibilité aux dispositifs
      
      if(input$MapObj == "Dispositifs"){
        
        # L'investissement locatif
        
        if(input$Dispositif == "Aide aux investissements locatifs (Scellier, Duflot, Pinel)") {
          
          if(input$Changements == "Eligibilité au 31 décembre") {
            
            if (input$annee < 2009) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2003",
                        col = c("#a1d99b"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Oui"),
                        legend.title.txt = "Eligibilité\nau Robien", add = TRUE)}
            
            if(input$annee%in% 2009:2011) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2009",
                        col = c("#a1d99b", "lightgray"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                        legend.title.txt = "Eligibilité\nau Scellier", add = TRUE)}
            
            if(input$annee == 2012) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2012",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Scellier", add = TRUE)}
            
            if(input$annee == 2013) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2013",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot", add = TRUE)}
            
            if(input$annee == 2014) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2014",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee == 2015) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2015",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee == 2016) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2016",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee == 2017) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2017",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee >= 2018) {
              typoLayer(x = fuaLy_DC, var = "ELIG_IL_2018",
                        col = c("#a1d99b", "lightgray"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
          } # Fin de la représentation par les éligibilité l'année donnée
          
          
          if(input$Changements == "Changement par rapport à l'année précédente") { 
            
            if(input$annee < 2009){
              plot(st_geometry(fuaLy_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            
            if(input$annee == 2009) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2009",
                        col = c("lightgray", "#fb6a4a"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Sorties\n(loi Scellier)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee %in% 2010:2011) {
              plot(st_geometry(fuaLy_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            if(input$annee == 2012) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2012",
                        col = c("lightgray", "#74c476"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(agrément)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2013) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2013",
                        col = c("lightgray", "#fdbf6f", "#fb6a4a", "#9e9ac8"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Sorties\n(fin agrément)", "Sorties\n(loi Duflot)", "Sorties\npar loi Duflot\net entrées\npar agrément"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2014) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2014",
                        col = c("lightgray", "#74a9cf", "#74c476", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(zonage)", "Entrées\n(agrément)", "Sorties\n(zonage)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2015) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2015",
                        col = c("lightgray", "#74c476"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(agrément)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2016) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2016",
                        col = c("lightgray", "#74c476"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(agrément)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2017) {
              plot(st_geometry(fuaLy_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            if(input$annee == 2018) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2018",
                        col = c("lightgray", "#fdbf6f"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Sorties\n(agrément)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2019) {
              typoLayer(x = fuaLy_DC, var = "CHG_ELIG_2019",
                        col = c("lightgray", "#74a9cf"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(zonage)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
          }
          
        } # Fin des investissements locatifs
        
        
        
        # La TVA réduite
        
        if(input$Dispositif == "TVA réduite"){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaLy_DC), col = NA, lwd = 0.4, border = "grey82", add = TRUE)
          
          if(input$annee %in% 2006: 2013) {plot(st_geometry(st_buffer(PNRU_Ly[(PNRU_Ly$date_sign <= input$annee & PNRU_Ly$elig_fin >= input$annee),], 500)), col = "orange1", border = NA, add = TRUE)}
          
          if(input$annee >= 2014) {plot(st_geometry(st_buffer(PNRU_Ly[(PNRU_Ly$date_sign <= input$annee & PNRU_Ly$elig_fin >= input$annee),], 300)), col = "orange1", border = NA, add = TRUE)}
          
          if(input$annee >= 2015){
            plot(st_geometry(st_buffer(QPV_Ly, 300)), col = "#74a9cf", border = NA, add = TRUE)
          }
          
          if(input$annee >= 2017){
            plot(st_geometry(st_buffer(QPV_Ly[QPV_Ly$NPNRU == "OUI",], 500)), col = "#74a9cf", border = NA, add = TRUE)
          }
          
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = NA,
                     categ = c("Secteurs\ndu PNRU" ,"Quartiers prioritaires des\npolitiques de la ville"),
                     col = c("orange1", "#74a9cf"), 
                     nodata = F, frame = FALSE, symbol = "box")
        }
        
        
        # L'APL accession
        
        if(input$Dispositif == "APL accession" & input$annee < 2018){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaLy_DC), col = "#c2e699", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nà l'APL\naccession",
                     categ = c("Oui"),
                     col = c("#c2e699"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "APL accession" & input$annee >= 2018){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(filter(fuaLy_DC, ZONE_123_2003 == "3")), col = "#c2e699", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nà l'APL\naccession",
                     categ = c("Oui", "Non"),
                     col = c("#c2e699", "lightgray"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        
        # Le PTZ dans le secteur ancien
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee %in% 2005:2010){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaLy_DC), col = "#bf812d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui"),
                     col = c("#bf812d"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee == 2011){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaLy_DC), col = "gray92", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Non"),
                     col = c("gray92"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee %in% 2012:2017){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaLy_DC), col = "#dfc27d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui (si\nrénovation\nlourdes"),
                     col = c("#dfc27d"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee >= 2018){
          plot(st_geometry(fuaLy_DEP), col = "white", border = NA)
          plot(st_geometry(fuaLy_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(filter(fuaLy_DC, ZONE_ABC_2014 == "B2" | ZONE_ABC_2014 == "C")), col = "#dfc27d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui", "Non"),
                     col = c("#dfc27d", "gray92"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        # Denormandie
        
        if(input$Dispositif == "Aide à la rénovation (Denormandie)" & input$annee >= 2019) {
          typoLayer(x = fuaLy_DC, var = "ELIG_DEN",
                    col = c("#6baed6", "lightgray"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("ACV seul", "Non"),
                    legend.title.txt = "Eligibilité\nau\nDenormandie", add = TRUE)}
      }
      
      plot(st_geometry(fuaLy_DC), col = NA, lwd = 0.4, border = "grey", add = TRUE)
      plot(st_geometry(fuaLy_DEP), col = NA, lwd = 1, border = "gray63", add = TRUE)
      
      plot(st_geometry(fuaLy_MET), col = NA, lwd = 1, border = "black", add = TRUE)
      
      if(input$show_NamesFinder == "Noms des communes les plus peuplées de\nchaque département (1 pour Paris,\n2 pour Lyon, 3 pour Avignon)") {
        labelLayer(fuaLy_Names,
                   txt = "LIBGEO_2019",
                   halo = T,
                   overlap = F)
      }
      
      if(input$show_NamesFinder == "Noms (et code) des départements") {labelLayer(fuaLy_DEP,
                                                                                  txt = "NOM_DEP",
                                                                                  halo = T,
                                                                                  overlap = F)}
      
    }
    
    
    
    # Cartographie d'Avignon
    
    if(input$MapArea == "Avignon"){
      plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
      plot(st_geometry(fuaAv_STUDY), col = "lightgray", border = NA, add = TRUE)
      
      if(input$MapObj == "Zonages"){
        
        # Cartographie des zonages A/B/C
        
        if(input$Zonage == "Classement A/B/C" & input$annee >= 2014){
          typoLayer(x = fuaAv_DC, var = "ZONE_ABC_2014",
                    col = c("#fe9929", "#fed98e", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2009:2013){
          typoLayer(x = fuaAv_DC, var = "ZONE_ABC_2009",
                    col = c("#fe9929", "#fed98e", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B1", "B2", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2006:2008){
          typoLayer(x = fuaAv_DC, var = "ZONE_ABC_2006",
                    col = c("#fe9929", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B1", "C"),
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        if(input$Zonage == "Classement A/B/C" & input$annee%in%2003:2005){
          typoLayer(x = fuaAv_DC, var = "ZONE_ABC_2003",
                    col = c("#feb24c", "#ffffd4"),
                    border = NA, 
                    legend.pos = "topright",
                    legend.values.order = c("B", "C"), 
                    legend.title.txt = "Zone du\nclassement\nA/B/C", add = TRUE)}
        
        # Cartographie des zonages 1/2/3
        
        if(input$Zonage == "Classement 1/2/3"){
          typoLayer(x = fuaAv_DC, var = "ZONE_123_2003",
                    col = c("#a8ddb5", "#e0f3db"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("2", "3"),
                    legend.title.txt = "Zone du\nclassement\n1/2/3", add = TRUE)}
      }
      
      # Cartographie de l'éligibilité aux dispositifs
      
      if(input$MapObj == "Dispositifs"){
        
        # L'investissement locatif
        
        if(input$Dispositif == "Aide aux investissements locatifs (Scellier, Duflot, Pinel)") {
          
          if(input$Changements == "Eligibilité au 31 décembre") {
            
            if (input$annee < 2009) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2003",
                        col = c("#a1d99b"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Oui"),
                        legend.title.txt = "Eligibilité\nau Robien", add = TRUE)}
            
            if(input$annee%in% 2009:2012) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2009",
                        col = c("#a1d99b", "lightgray"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                        legend.title.txt = "Eligibilité\nau Scellier", add = TRUE)}
            
            if(input$annee == 2013) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2013",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot", add = TRUE)}
            
            if(input$annee == 2014) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2014",
                        col = c("#a1d99b", "lightgray"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee == 2015) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2015",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee == 2016) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2016",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee == 2017) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2017",
                        col = c("#a1d99b", "lightgray", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible", "Eligible\n(agrément)"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
            
            if(input$annee >= 2018) {
              typoLayer(x = fuaAv_DC, var = "ELIG_IL_2018",
                        col = c("#a1d99b", "lightgray"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Eligible\n(zonage)", "Non éligible"),
                        legend.title.txt = "Eligibilité\nau Duflot\n/ au Pinel", add = TRUE)}
          } # Fin de la représentation par les éligibilité l'année donnée
          
          
          if(input$Changements == "Changement par rapport à l'année précédente") { 
            
            if(input$annee < 2009){
              plot(st_geometry(fuaAv_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            
            if(input$annee == 2009) {
              typoLayer(x = fuaAv_DC, var = "CHG_ELIG_2009",
                        col = c("lightgray", "#fb6a4a"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Sorties\n(loi Scellier)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee %in% 2010:2012) {
              plot(st_geometry(fuaAv_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            if(input$annee == 2013) {
              typoLayer(x = fuaAv_DC, var = "CHG_ELIG_2013",
                        col = c("lightgray", "#fb6a4a", "#9e9ac8"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Sorties\n(loi Duflot)", "Sorties\npar loi Duflot\net entrées\npar agrément"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2014) {
              typoLayer(x = fuaAv_DC, var = "CHG_ELIG_2014",
                        col = c("lightgray", "#74a9cf", "#fd8d3c"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(zonage)", "Sorties\n(zonage)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2015) {
              typoLayer(x = fuaAv_DC, var = "CHG_ELIG_2015",
                        col = c("lightgray", "#74c476"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Entrées\n(agrément)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2016) {
              plot(st_geometry(fuaAv_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            if(input$annee == 2017) {
              plot(st_geometry(fuaAv_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
            if(input$annee == 2018) {
              typoLayer(x = fuaAv_DC, var = "CHG_ELIG_2018",
                        col = c("lightgray", "#fdbf6f"),
                        border = NA,
                        legend.pos = "topright",
                        legend.values.order = c("Pas de\nchangement", "Sorties\n(agrément)"),
                        legend.title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente", add = TRUE)}
            
            if(input$annee == 2019) {
              plot(st_geometry(fuaAv_DC), col = "lightgray", lwd = 0.4, border = "grey82", add = TRUE)
              plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
              plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
              legendTypo(pos = "topright", 
                         title.txt = "Entrées et sorties du\ndispositif par rapport\nà l'année précédente",
                         categ = c("Pas de\nchangement"),
                         col = c("lightgray"),
                         nodata = F, frame = FALSE, symbol = "box")}
            
          }
          
        } # Fin des investissements locatifs
        
        
        # La TVA réduite
        
        if(input$Dispositif == "TVA réduite"){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaAv_DC), col = NA, lwd = 0.4, border = "grey82", add = TRUE)
          
          if(input$annee %in% 2006: 2013) {plot(st_geometry(st_buffer(PNRU_Av[(PNRU_Av$date_sign <= input$annee & PNRU_Av$elig_fin >= input$annee),], 500)), col = "orange1", border = NA, add = TRUE)}
          
          if(input$annee >= 2014) {
            plot(st_geometry(st_buffer(PNRU_Av[(PNRU_Av$date_sign <= input$annee & PNRU_Av$elig_fin >= input$annee),], 300)), col = "orange1", border = NA, add = TRUE)
          }
          
          if(input$annee >= 2015){
            plot(st_geometry(st_buffer(QPV_Av, 300)), col = "#74a9cf", border = NA, add = TRUE)
          }
          
          if(input$annee >= 2017){
            plot(st_geometry(st_buffer(QPV_Av[QPV_Av$NPNRU == "OUI",], 500)), col = "#74a9cf", border = NA, add = TRUE)
          }
          
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = NA,
                     categ = c("Secteurs\ndu PNRU", "Quartiers prioritaires\nde la ville"),
                     col = c("orange1", "#74a9cf"), 
                     nodata = F, frame = FALSE, symbol = "box")
        }
        
        
        # L'APL accession
        
        if(input$Dispositif == "APL accession" & input$annee < 2018){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaAv_DC), col = "#c2e699", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nà l'APL\naccession",
                     categ = c("Oui"),
                     col = c("#c2e699"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "APL accession" & input$annee >= 2018){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(filter(fuaAv_DC, ZONE_123_2003 == "3")), col = "#c2e699", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nà l'APL\naccession",
                     categ = c("Oui", "Non"),
                     col = c("#c2e699", "lightgray"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        
        # Le PTZ dans le secteur ancien
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee %in% 2005:2010){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaAv_DC), col = "#bf812d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui"),
                     col = c("#bf812d"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee == 2011){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaAv_DC), col = "gray92", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Non"),
                     col = c("gray92"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee %in% 2012:2017){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(fuaAv_DC), col = "#dfc27d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui (si\nrénovations\nlourdes"),
                     col = c("#dfc27d"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        if(input$Dispositif == "PTZ dans le secteur ancien" & input$annee >= 2018){
          plot(st_geometry(fuaAv_DEP), col = "white", border = NA)
          plot(st_geometry(fuaAv_STUDY), col = "gray92", border = NA, add = TRUE)
          plot(st_geometry(filter(fuaAv_DC, ZONE_ABC_2014 == "B2" | ZONE_ABC_2014 == "C")), col = "#dfc27d", lwd = 0.4, border = "grey82", add = TRUE)
          plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "grey", add = TRUE)
          plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
          legendTypo(pos = "topright", 
                     title.txt = "Eligibilité\nau PTZ\ndans l'ancien",
                     categ = c("Oui", "Non"),
                     col = c("#dfc27d", "gray92"),
                     nodata = F, frame = FALSE, symbol = "box")}
        
        # Denormandie
        
        if(input$Dispositif == "Aide à la rénovation (Denormandie)" & input$annee >= 2019) {
          typoLayer(x = fuaAv_DC, var = "ELIG_DEN",
                    col = c("#6baed6", "lightgray"),
                    border = NA,
                    legend.pos =  "topright",
                    legend.values.order = c("ACV seul", "Non"),
                    legend.title.txt = "Eligibilité\nau\nDenormandie", add = TRUE)}
      }
      
      plot(st_geometry(fuaAv_DC), col = NA, lwd = 0.4, border = "grey", add = TRUE)
      plot(st_geometry(fuaAv_DEP), col = NA, lwd = 1, border = "gray63", add = TRUE)
      
      plot(st_geometry(fuaAv_MET), col = NA, lwd = 1, border = "black", add = TRUE)
      
      if(input$show_NamesFinder == "Noms des communes les plus peuplées de\nchaque département (1 pour Paris,\n2 pour Lyon, 3 pour Avignon)") {
        labelLayer(fuaAv_Names,
                   txt = "LIBGEO_2019",
                   halo = T,
                   overlap = F)
      }
      
      if(input$show_NamesFinder == "Noms (et code) des départements") {labelLayer(fuaAv_DEP,
                                                                                  txt = "NOM_DEP",
                                                                                  halo = T,
                                                                                  overlap = F)}
      
      
    }
    
    layoutLayer(title = paste0(ifelse(input$MapObj == "Zonages", input$Zonage, input$Dispositif),
                               " - FUA de ", input$MapArea,", ", input$annee),
                sources = "INSEE, Journal officiel",
                author = "ANR WIsDHoM - 2020 (P. Le Brun)",
                scale = 10, 
                frame = FALSE,
                theme = "red.pal")
  }
  
  
  output$MainMap <- renderPlot({
    
    plotInput()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$MapArea, input$MapObj, substr(input$annee, 3,4), ".svg", sep = "")
    },
    content = function(file) {
      
      svg(file)
      plotInput()
      dev.off()
    })
  

}


shinyApp(server = server, ui = ui)


