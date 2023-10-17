PARTIE GENERALE : MATIN

# EXHAUSTIVITE DES LIBRAIRIES

librairies_requises <- c( # On liste les librairies dont on a besoin
  "tidyverse", # Une série de packages pour faciliter la manipulation de données
  "readxl", # Pour lire les fichiers excel (Carvalho et al. 2018)
  "writexl", # Pour écrire des fichiers excel
  "cowplot", # Pour arranger des graphiques en illustrations composées
  "gt", # Pour des rendus graphiques harmonisés html et pdf/LaTeX
  "sf", # Pour faciliter la manipulation de données géographiques
  "wdpar", # Pour télécharger simplement la base d'aires protégées WDPA
  "webdriver", # requis pour installer phantomjs pour wdpar
  "tmap", # Pour produire de jolies cartes
  "geodata", # Pour télécharger simplement les frontières administratives
  "tidygeocoder", # pour obtenir les coordo GPS d'un point à partir de son nom
  "maptiles", # Pour télécharger des fonds de carte 
  # "mapme.biodiversity", # Acquisition et traitement des données du projet
  "plm", # Linear Models for Panel Data and robust covariance matrices
  "broom", # pour reformater simplement les rendus de tests statistiques
  "stargazer", # Reformater de manière plus lisible les résumé des régressions
  "MatchIt", # Pour le matching
  #"glm", # Modèles linéaires généralisés (pour le PSM)
  "optmatch", # Fonctions d'optimisation du matching
  "rgee",
  "did", # Méthode de double différence échelonnée de Callaway et Sant'Anna
  "cobalt", # Tables et graphs d'équilibre des groupes de matching
  "foreign") # Lire les informations des tables attributaires

# CHARGEMENT DES LIBRAIRIES 

library(tidyverse)
library(writexl) # Créer fichier Excel
library(readxl) # Lire fichier Excel 
library(lubridate)
library(sf)
library(tmap)
library(geodata)
library(cowplot)
library(wdpar)
library(gt)
library(foreign)

# IMPORTATION DES DONNEES VAHATRA 

# Le shapefile est composé d'une série de fichiers, (.shp, .dbf, .prj, .shx)
# qui doivent avoir le même nom et être au même endroits pour être ouverts en
# même temps. Comme souvent, ils sont compressés ensemble dans un fichier zip.

# On commence par dézipper (décompresser) ce fichier
#unzip("data/AP_Vahatra_geojson.zip", exdir = "data/Vahatra")
#unzip("data/AP_Vahatra_shp.zip", exdir = "data/Vahatra")

# On enregistre la table attributaire
AP_Vahatra <- read.excel("data/Vahatra/ch1_AP_Vahatra")

# On la convertit en fichier excel
write_xlsx(AP_Vahatra, path = "data/Vahatra/AP_Vahatra.xlsx")

# On travaille maintenant le fichier excel
AP_Vahatra <- read_xlsx("data/Vahatra/ch1_AP_Vahatra.xlsx")

# On fait apparaître le nom des colonnes 
colnames(AP_Vahatra)

# On fait apparaître le nom des différentes aires protégées et leur catégorie IUCN 
AP_Vahatra %>%
  select(nom, cat_iucn) %>%
  filter(row_number() <= 10)


# On fait apparaître le nom des parcs nationaux 
liste_PN <- AP_Vahatra %>%
  filter(cat_iucn == "II") %>%
  select(nom)

# Création d'une colonne pour la superficie en km² 
AP_Vahatra <- AP_Vahatra %>%
  mutate(superficie_km2 = hectares * 0.01)

# Tri des données de manière décroissante en fonction de la superficie en km²
AP_Vahatra <- AP_Vahatra %>%
  arrange(desc(superficie_km2))

# Obtenir les 3 plus grandes aires protégées 
AP_Vahatra %>% 
  slice_head(n = 3) %>%
  select(nom)

# Obtenir la superficie totale de toutes les aires protégées 
AP_Vahatra %>%
  summarise(sum(AP_Vahatra$superficie_km2))

# Résumé statistique des superficies en km² 
summary(AP_Vahatra$superficie_km2)

# Aires protégées de superficie supérieure ou égale au 3ème quartile
AP_Vahatra %>%
  filter(superficie_km2 >= 758.975) %>%
  select(nom) 

#à vérifier

# Aires protégées créées après 2000 et dont la gestion est assurée par l'Etat

#ERREUR DIMENSIONS AP_Vahatra$nom[AP_Vahatra$an_crtn > 2000 & AP_Vahatra$gest_2 == "MEEF", ]

subset(AP_Vahatra, an_crtn > 2000 & gest_2 == "MEEF")$nom

# Calcul des superficies totales pour chaque catégorie IUCN 

AP_Vahatra_icn <- AP_Vahatra %>%
  filter(!is.na(cat_icn)) %>%
  group_by(cat_icn) %>%
  summarise(superficie_totale = sum(superficie_km2))

# Production d'un joli tableau synthétique

gt(AP_Vahatra_icn) %>%
  cols_label(
    cat_icn = "Catégorie IUCN",
    superficie_totale = "Superficie totale (km²)"
  ) %>%
  tab_header(
    title = "Aires protégées de Madagascar : superficies par catégorie IUCN"
  ) %>%
  tab_source_note(
    "Source : données de l'association Vahatra"
  )

# Arrondir les km² 

## Production d'un joli graphique 
## On cherche à décrire la dynamique de création d'aires protégées 
## Notamment voir en termes de superficie

AP_superficie_annees <- AP_Vahatra %>%
  group_by(an_crtn) %>% # On regroupe d'abord les AP qui ont été créées la même année
  summarise(superficie_cumulée = sum(superficie_km2)) %>% # On fait la somme de ces superficies (par année de création)
  mutate(superficie_cumulée = cumsum(superficie_cumulée)) # Pour le rendu graphique (cumulatif), on accumule les superficies de chaque groupe (année de création X) avec le groupe précédent (X -1)

## Graphique en nuages de points 

ggplot(data = AP_superficie_annees, aes(x = an_crtn, y = superficie_cumulée)) +
  geom_point() +
  geom_line() +
  labs(x = "Année de création de l'aire protégée", y = "Superficie cumulée (km²)") +
  ggtitle("Superficie cumulée en fonction de l'année de création") +
  theme_minimal()

# IMPORTATION DES DONNEES WDPA 
WDPA_Mada <- wdpa_fetch("Madagascar", wait = TRUE,
                        download_dir = "data/WDPA")

#Créer une option pour ceux qui l'ont sur le PC

# Comparaison nombres d'aires protégées Vahatra et WDPA

AP_Vahatra %>%
  distinct(nom) %>%
  nrow()

WDPA_Mada %>%
  distinct(NAME) %>%
  nrow()

# Il existe une différence entre le nombre d'AP de la base Vahatra et de la base de données WDPA 

sum(AP_Vahatra$superficie_km2)
sum(WDPA_Mada$REP_AREA)

# Si on a le temps, pourquoi pas reprendre le script de Florent sur la comparaison
# Visualiser les différences

# Il existe une différence entre la superficie en km² des AP de la base Vahatra et de la base de données WDPA 



# PARTIE GEOSPATIALE : APRES-MIDI 

# On télécharge les communes depuis la base GADM

contour_mada <- gadm(country = "Madagascar", resolution = 1, level = 3,
                     path = "data/GADM") %>%
  st_as_sf()

# On enregistre contour_mada pour s'en servir par la suite
save(contour_mada, file = "data/contour_mada.rds")

# Option disque local 

# On veut connaître le nombre de communes 
nrow(contour_mada)

# On fait un point sur nos trois jeux de données 
st_crs(contour_mada) # WGS 84, EPSG:4326
st_crs(WDPA_Mada) # WGS 84, EPSG:4326 

AP_Vahatra <- st_read("data/Vahatra/AP_Vahatra.shp")
st_crs(AP_Vahatra) # WGS 84, EPSG:4326 

# On fait une carte pour visualiser la donnée géographique

tmap_mode("view") # En mode interactif
tm_shape(contour_mada) +
  tm_borders() +
  tm_shape(AP_Vahatra) + 
  tm_polygons(col = "cat_icn", alpha = 0.6, title = "Catégorie IUCN",
              id = "nom",
              popup.vars = c("Acte de création" = "creatin",
                             "Année de création" = "an_crtn",
                             "Surface (ha)" = "hectars",
                             "Nom" = "nom",
                             "Gestionnaire" = "gest_1")) +
  tmap_options(check.and.fix = TRUE)


# 1er constat : plusieurs zones de protection existent pour une même aire protégée
# Aggrégation : on va actualiser les informations attributaires en fonction de cette réalité spatiale 
# 2nd constat : les limites des aires protégées dépassent celles des communes 
# Intersection : on va garder uniquement les aires protégées qui se trouvent dans ces limites communales 
# Bien qu'on risque d'exclure certaines AP maritimes, on aura pour chaque commune une idée du % d'aire protégée

# Tout d'abord, on vérifie la géométrie des entités
st_is_valid(contour_mada)
st_is_valid(AP_Vahatra) 

# Bien faire l'assignation pour enregistrer la transformation des données
AP_Vahatra <- st_make_valid(AP_Vahatra)

# Suite au 1er constat, on divise les multi-polygones en polygones individuels
AP_terrestres <- AP_Vahatra %>%
  st_cast("POLYGON")

# Intersection avec contour_mada
AP_terrestres <- st_intersection(AP_terrestres, contour_mada)

# Actualiser les données attributaires d'intérêt (st_intersection ne le fait pas automatiquement)
## Calcul de la superficie en kilomètres carrés
AP_terrestres$superficie_km2 <- as.integer(st_area(AP_terrestres) / 1e6)

# Visualiser le recoupement en rechargeant la carte 

tm_shape(contour_mada) +
  tm_borders() +
  tm_shape(AP_terrestres) + 
  tm_polygons(col = "cat_icn", alpha = 0.6, title = "Catégorie IUCN",
              id = "nom",
              popup.vars = c("Acte de création" = "creatin",
                             "Année de création" = "an_crtn",
                             "Surface (ha)" = "superficie_km2",
                             "Nom" = "nom",
                             "Gestionnaire" = "gest_1",
                             "Commune" = "NAME_3")) +
  tmap_options(check.and.fix = TRUE)

# Objectif : connaître le pourcentage d'aires protégées par commune (km²)

# Agrégation spatiale des aires protégées sur la même commune 
AP_communes <- AP_terrestres %>% 
  group_by(NAME_3) %>% 
  summarize(superficie_km2 = sum(superficie_km2))

# Perte de données : nom de l'aire protégée (à rectifier)

# Voir le rendu cartographique 

tm_shape(contour_mada) +
  tm_borders() +
  tm_shape(AP_communes) + 
  tm_polygons(col = "NAME_3", alpha = 0.6, title = "test",
              popup.vars = c("Superficie totale" = "superficie_km2"))




