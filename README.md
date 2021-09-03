### Application shiny

Ce dépôt contient les éléments nécessaires au déploiement d'une application shiny permettant de suivre l'évolution des dispositifs sur les FUA de Lyon, Avignon et Paris. 

```
# Packages nécessaires à l'exécution de l'application
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(sf)
library(dplyr)
library(cartography)
library(readxl)

# Lancer l'application
shiny::runGitHub(repo = "riateStage/dispositif_lebrun_shiny", ref = "main")
```

Ce script utilise les données produites dans le dépôt [dispositif_lebrun](https://github.com/riateStage/dispositif_lebrun)