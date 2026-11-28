# EcoTS

Package R d'automatisation de l'econometrie des series temporelles (seuil 5%).

## Installation
```r
install.packages("remotes")
remotes::install_github("Jojo12End/EcoTS", force = TRUE)
```

## Utilisation
```r
library(EcoTS)
eco_guide("lprix", "lsal")
res <- eco_auto(lprix, lsal)
```
