\name{eco_auto}
\alias{eco_auto}
\title{Analyse econometrique automatisee complete (seuil 5\%)}
\usage{
eco_auto(var1, var2 = NULL, name1 = NULL, name2 = NULL, max_lag = 8, run_bubbles = TRUE, plot = TRUE)
}
\arguments{
\item{var1}{Premiere serie temporelle (vecteur numerique, ts ou xts).}
\item{var2}{Deuxieme serie temporelle optionnelle pour l'analyse multivariee.}
\item{name1}{Nom personnalise pour var1.}
\item{name2}{Nom personnalise pour var2.}
\item{max_lag}{Nombre maximal de retards pour ADF et VARselect (defaut = 8).}
\item{run_bubbles}{Booleen : executer les tests de bulles SADF/GSADF (defaut = TRUE).}
\item{plot}{Booleen : afficher les graphiques (defaut = TRUE).}
}
\value{Une liste invisible contenant tous les objets estimes (stationarity, bubbles, arma, johansen, var, irf, lp).}
\description{Execute automatiquement toute la strategie d'analyse des series temporelles au seuil de 5\% : ADF sequentiel, KPSS, differenciation si I(1), bulles SADF/GSADF, ARMA + diagnostics, Johansen/VECM, VAR, causalite de Granger, IRF et projections locales.}
