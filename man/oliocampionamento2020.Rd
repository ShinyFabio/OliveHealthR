% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{oliocampionamento2020}
\alias{oliocampionamento2020}
\title{Schede del campionamento 2020 dell'olio}
\format{
Un dataset con 26 righe e 6 variabili:
\describe{
  \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
  \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
  \item{Data_campionamento}{la data del campionamento nel formato yyyy-mm-gg (nel file csv è gg/mm/yyyy)}
  \item{Olio}{fattoriale che indica se è stato fornito il campione di olio. (SI = fornito, NO = non fornito, NA = l'azienda non ha fornito nulla)}
  \item{Sansa}{fattoriale che indica se è stato fornito il campione di sansa. (SI = fornito, NO = non fornito, NA = l'azienda non ha fornito nulla)}
  \item{Note}{eventuali note riguardanti il campionamento}
}
}
\source{
\url{http://bioinfo.na.iac.cnr.it/olivehealth/File_csv.rar}
}
\usage{
oliocampionamento2020
}
\description{
Un dataset che contiene le schede del campionamento 2020 dell'olio. Non tutte le aziende hanno partecipato. Le variabili sono le seguenti:
}
\keyword{datasets}
