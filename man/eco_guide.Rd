\name{eco_guide}
\alias{eco_guide}
\title{Aide-memoire syntaxique complet du Guide R (Examen)}
\usage{
eco_guide(var1 = "var1", var2 = "var2", output = c("viewer", "console"))
}
\arguments{
\item{var1}{Nom de la premiere variable en log (ex: lprix).}
\item{var2}{Nom de la deuxieme variable en log (ex: lsal).}
\item{output}{Mode d'affichage : "viewer" (panneau RStudio) ou "console".}
}
\value{Le texte du script genere (invisiblement).}
\description{Affiche dans le Viewer de RStudio ou dans la console toutes les commandes du pipeline d'examen (ts, ADF/KPSS, bulles, ARMA, previsions, VAR, Granger, IRF, projections locales, Johansen et VECM) en remplacant automatiquement les noms des variables par var1 et var2.}
