#' Schede del campionamento 2020 delle drupe
#'
#' Un dataset che contiene le schede del campionamento 2020 delle drupe. Le variabili sono le seguenti:
#'
#' @format Un dataset con 52 righe e 6 variabili:
#' \describe{
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
#'   \item{Data_campionamento}{la data del campionamento nel formato yyyy-mm-gg (nel file csv è gg/mm/yyyy)}
#'   \item{Fase_fenologica}{fase fenologica della pianta espresso come numero-factor (51, 55, 59, 61, 65, 69, 71, 75, 79, 81, 85, 89)}
#'   \item{Indice_maturazione}{il livello di maturazione delle drupe espresso come numero-factor (0, 1, 2, 3, 4, 5, 6, 7, 8)}
#'   \item{Note}{eventuali note riguardanti il campionamento}
#' }
#' @source \url{http://bioinfo.na.iac.cnr.it/olivehealth/File_csv.rar}
"drupecamp2020"



#' Schede del campionamento 2020 dell'olio
#'
#' Un dataset che contiene le schede del campionamento 2020 dell'olio. Non tutte le aziende hanno partecipato. Le variabili sono le seguenti:
#'
#' @format Un dataset con 26 righe e 6 variabili:
#' \describe{
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
#'   \item{Data_campionamento}{la data del campionamento nel formato yyyy-mm-gg (nel file csv è gg/mm/yyyy)}
#'   \item{Olio}{fattoriale che indica se è stato fornito il campione di olio. (SI = fornito, NO = non fornito, NA = l'azienda non ha fornito nulla)}
#'   \item{Sansa}{fattoriale che indica se è stato fornito il campione di sansa. (SI = fornito, NO = non fornito, NA = l'azienda non ha fornito nulla)}
#'   \item{Note}{eventuali note riguardanti il campionamento}
#' }
#' @source \url{http://bioinfo.na.iac.cnr.it/olivehealth/File_csv.rar}
"oliocampionamento2020"


#' Panel test degli oli
#'
#' Un dataset che contiene i risultati del panel test effettuato sugli oli. Non tutte le aziende hanno partecipato.
#'
#' @format Un dataset con 27 righe e 11 variabili:
#' \describe{
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{Anno}{l'anno del campionamento (es. 2020)}
#'   \item{Tipo_olio}{informazione addizionale sul tipo di olio. Alcune aziende hanno fornito diversi oli a diverse fasi di lavorazione.}
#'   \item{Categoria_olio}{categoria di appartenenza dell'olio (lampante, vergine o extravergine)}
#'   \item{Mediana_fruttato}{mediana del fruttato dell'olio (1.0 - 6.2)}
#'   \item{CVR_Fruttato}{il coefficiente di variazione robusta del fruttato. Se il valore è inferiore al limite di legge è riportato "ok", altrimenti "nok"}
#'   \item{Presenza_difetti}{presenza di difetti nell'olio (no, si)}
#'   \item{Mediana_amaro}{mediana dell'amaro dell'olio (1.9 - 4.5)}
#'   \item{CVR_amaro}{il coefficiente di variazione robusta dell'amaro. Se il valore è inferiore al limite di legge è riportato "ok", altrimenti "nok"}
#'   \item{Mediana_piccante}{mediana dell'amaro dell'olio (1.5 - 4.8)}
#'   \item{CVR_piccante}{il coefficiente di variazione piccante dell'amaro. Se il valore è inferiore al limite di legge è riportato "ok", altrimenti "nok"}
#'   
#' }
#' @source \url{http://bioinfo.na.iac.cnr.it/olivehealth/File_csv.rar}
"assaggi2020"



#' Morfometria di drupe, foglie ed endocarpo della stagione 2020
#'
#' Una lista che contiene 4 vettori contenenti i dati morfometrici di foglie, endocarpo, drupe e rapporti drupe-endocarpo della stagione 2020.
#'
#' @format Una lista con 4 dataset:
#' \itemize{
#' \item{\strong{Foglie}}
#' \describe{
#' Un dataset con 222 righe e 16 variabili:
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
#'   \item{Anno}{l'anno del campionamento (es. 2020)}
#'   \item{ID_foglia}{codice identificativo della foglia}
#'   \item{`Area_(cm3)`}{area della foglia in cm3}
#'   \item{`Lunghezza_(cm)`}{lunghezza della foglia in cm}
#'   \item{`Lunghezza_(IOC)`}{lunghezza della foglia secondo la classificazione dell'IOC (short, medium, long)}
#'   \item{`Larghezza_(cm)`}{larghezza della foglia in cm}
#'   \item{`Larghezza_(IOC)`}{larghezza della foglia secondo la classificazione dell'IOC (narrow, medium, broad)}
#'   \item{`Forma_(L/W)`}{rapporto tra lunghezza e larghezza}
#'   \item{`Shape (IOC)`}{forma della foglia secondo la classificazione dell'IOC (elliptic, medium, lanceolate)}
#'   \item{Circolarità}{valore di circolarità della foglia}
#'   \item{Rotondità}{valore di rotondità della foglia}
#'   \item{`Perimetro_(cm)`}{perimetro della foglia in cm}
#'   \item{Aspect_ratio}{aspect ratio della foglia}
#'   \item{Irregolarità_contorno}{irregolarità del contorno della foglia}
#'   }
#'  \item{\strong{Drupe}}
#'  \describe{
#'  Un dataset con 78 righe e 16 variabili:
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
#'   \item{Anno}{l'anno del campionamento (es. 2020)}
#'   \item{ID_oliva}{codice identificativo della drupa}
#'   \item{`Volume_(cm3)`}{volume della drupa in cm3}
#'   \item{`Area_superficie_(cm2)`}{area superficie della drupa in cm2}
#'   \item{`Lunghezza_(mm)`}{lunghezza della drupa in mm}
#'   \item{`Larghezza_(mm)`}{larghezza della drupa in mm}
#'   \item{`Forma_(L/W)`}{forma della drupa (rapporto tra lunghezza e larghezza)}
#'   \item{`Forma_(IOC)`}{forma della drupa secondo la classificazione dell'IOC (spherical, ovoid, elongated)}
#'   \item{Sfericità}{misura della sfericità della drupa}
#'   \item{`Volume_mesocarpo_(cm3)`}{volume del mesocarpo della drupa in cm3}
#'   \item{Coefficiente_asimmetria}{coefficiente di asimmetria della drupa}
#'   \item{`Asimmetria_(IOC)`}{coefficiente di asimmetria secondo la classificazione dell'IOC (symmetric, slightly asymmetric, asymmetric)}
#'   \item{`Posizione_MTD_(%L)`}{}
#'   \item{`Posizione_MTD_(IOC)`}{posizione .... secondo la classificazione dell'IOC (central, toward base)}
#'  }
#'  \item{\strong{Endocarpo}}
#'  \describe{
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
#'   \item{Anno}{l'anno del campionamento (es. 2020)}
#'   \item{ID_oliva}{codice identificativo dell'endocarpo}
#'   \item{`Volume_(cm3)`}{volume dell'endocarpo in cm3}
#'   \item{`Area_superficie_(cm2)`}{area superficie dell'endocarpo in cm2}
#'   \item{`Lunghezza_(mm)`}{lunghezza dell'endocarpo in mm}
#'   \item{`Larghezza_(mm)`}{larghezza dell'endocarpo in mm}
#'   \item{Sfericità}{misura della sfericità dell'endocarpo}
#'   \item{`Forma_(L/W)`}{forma dell'endocarpo (rapporto tra lunghezza e larghezza)}
#'  }
#'  \item{\strong{Rapporti}}
#'  \describe{
#'   \item{Codice_azienda}{codice identificativo dell'azienda (es. SA_01)}
#'   \item{N_campionamento}{codice utilizzato per distinguere il primo dal secondo campionamento (R1, R2)}
#'   \item{Anno}{l'anno del campionamento (es. 2020)}
#'   \item{ID_oliva}{codice identificativo dell'endocarpo}
#'   \item{Rapporto_volume_mesocarpo}{rapporto tra il volume del mesocarpo e dell'endocarpo}
#'   \item{Rapporto_lunghezza}{}
#'   \item{Rapporto_larghezza}{}
#'  }
#' }
#' @source \url{http://bioinfo.na.iac.cnr.it/olivehealth/File_csv.rar}
"morfometria2020"
