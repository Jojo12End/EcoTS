# EcoTS

Package R d'automatisation de l'econometrie des series temporelles (seuil 5%).

## Installation
```r
install.packages("remotes")
remotes::install_github("Jojo12End/EcoTS")
```

## Utilisation
```r
library(EcoTS)
res <- eco_auto(data$D0, data$r_c)
eco_guide("D0", "r_c")
```
