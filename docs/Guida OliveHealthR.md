
# 1. Cos'è OliveHealthR
OliveHealthR è un software che svolge analisi sui dati provenienti dal progetto OliveHealth. L'obiettivo principale del progetto è quello di identificare le componenti salutistiche (es. polifenoli) in prodotti della filiera olivicola (quali foglie, drupe e
olio) correlandole alla geo-localizzazione di ciascun appezzamento. Sulla base di queste informazioni
verranno prodotti dataset delle variabili rappresentative delle principali caratteristiche fenotipiche,
biochimiche e genetiche associate all’ulivo dalle quali nascerà un database messo a disposizione ai
produttori olivicoli campani.Per maggiori informazioni sul progetto clicca [qui](https://olivehealth.it).\

***





# 2. Come si installa
OliveHealthR è un pacchetto creato utilizzando le librerie shiny di R e, pertanto, per poter utilizzarlo bisogna installare i softwareR e Rstudio. Se li hai già installati puoi saltare questi passaggi e andare allo step 3.

## 2.1 Installazione dei software necessari
### I. Installare R

Scarica e installa R da uno di questi link. Scegli in base al sistema operativo utilizzato.

*   **Windows:** https://cran.r-project.org/bin/windows/base/
*   **OS:** https://cran.r-project.org/bin/macosx/
*   **Ubuntu:** in questo caso segui la procedura qui descritta https://www.r-bloggers.com/2013/03/download-and-install-r-in-ubuntu/


### II. Installa RStudio

Dopo aver installato R, installare RStudio. Apri il seguente link https://www.rstudio.com/products/rstudio/download/#download e scegliere la versione corrispondente al tuo sistema operativo.


### III. Operazioni aggiuntive in base al sistema operativo

Per poter far funzionare correttamente il software è necessario eseguire alcuni passaggi supplementari che cambiano in base al tuo sistema operativo:

* **Solo per utenti Windows.**\
Se sei un utente Windows è necessario installare anche Rtools utilizzando questo link: https://cran.r-project.org/bin/windows/Rtools.
Se il tuo computer non ha Windows puoi tranquillamente saltare questo passaggio.

* **Solo per utenti MacOS (da controllare).**\
Se sei un utente MacOS è necessario lanciare questa riga di comando dal Terminale del computer:

    ``` r
    brew install imagemagick@6
    ```

* **Solo per utenti Ubuntu (testato su 18.04).**\
Se sei un utente Ubuntu bisogna installare delle librerie aggiuntive lanciando dal terminale le seguenti linee di codice:\
  ```
  sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
  sudo apt-get install libcairo2-dev
  sudo apt-get install libxt-dev
  sudo apt install libudunits2-dev
  sudo apt install libgdal-dev
  sudo apt install libmagick++-dev
  sudo apt-get install libc6
  ```


## 2.2. Installazione dei pacchetti di R

Ora che hai installato tutti i software necessari, avvia il programma Rstudio. Al primo avvio la schermata iniziale è la seguente:


![rstudio](https://user-images.githubusercontent.com/78078351/159449079-84f48281-f1e1-49a1-9fb7-ef37e2d22640.png)


Tutti i codici di R che dovrai utilizzare nei passaggi seguenti dovranno essere copiati e incollati nella parte evidenziata nella figura precedente. Una volta incollati, per eseguire il comando basta premere il tasto Invio. 

Detto ciò, procediamo all'installazione del pacchetto OliveHealthR lanciando i codici qui sotto:

``` r
if(!requireNamespace("devtools"))install.packages("devtools")
devtools::install_github("ShinyFabio/OliveHealthR")

```
Per farlo seleziona semplicemente le due righe di codice, incollale in RStudio e premi Invio. Se esce un messaggio come questo nella figura sottostante cliccare su <strong>No</strong>.


<p align="center">
  <img src="https://user-images.githubusercontent.com/78078351/159449136-566233bd-1ab1-4730-8848-156e10330396.png">
</p>



Il processo di installazione richiede qualche minuto. Al termine OliveHealthR sarà pronto all'uso. Prima di avviare il software, puoi decidere di installare tutte le immagini accessorie (cromatogrammi, foto di drupe, foto di foglie etc.). Questo passaggio è facoltativo. Per farlo lancia il codice qui sotto riportato:

``` r
OliveHealthR::download_photo()
```

## 2.3. Avvio di OliveHealthR

Arrivati a questo punto sei pronto per lanciare OliveHealthR. Esegui semplicemente le due righe di codice qui riportate:

``` r
library(OliveHealthR)
OliveHealthR::run_OliveHealthR()
```

***


# 3. Guida all'uso

Appena viene lanciato il comando per far partire OliveHealthR, la prima schermata che viene visualizzata è una pagina di benvenuto contenente una breve descrizione del progetto e dei partner coinvolti. Cliccando sul tasto "VAI!" ci si ritrova nel software vero e proprio. Il software è suddiviso in menu (barra grigia a sinistra) e ogni menu può avere o meno dei sottomenu. La suddivisione è basata principalmente sulla tipologia di dati. Cinque sono i menu presenti:

* **File** in cui è possibile controllare e caricare tutti i dati
* **Azienda** contenente informazioni sulle aziende coinvolte
* **Campionamento azienda** dove sono analizzate le schede campionamento di drupe,foglie, olio e i panel test
* **Analisi laboratorio** contenente le analisi sui polifenoli e sulla morfometria
* **Integrazione dati** dove sono incrociate le varie tipologie di dati\




## 3.1. File
Il primo menu che si apre dopo aver cliccato il pulsante "VAI!" è il menu File. Qui è presente una panoramica di tutti i dati che sono stati raccolti dai vari partner e che sono stati già pre-elaborati e installati nel software. Una volta che ci si è assicurati che tutti i file siano presenti (spunte verdi in ogni casellina) si possono caricare i dati cliccando sul pulsante "Carica tutti i dati!".
Delle notifiche in basso a destra mostrano l'avanzamento del caricamento e l'avvenuto.


![schermata_dati](https://user-images.githubusercontent.com/78078351/159449178-5663294e-16ab-4273-9c1e-7db9f9d573fd.png)


Fatto ciò è possibile analizzare i dati cliccando sugli altri menu.


## 3.2. Azienda
Questo menu contiene tutte le informazioni relative alle aziende che hanno partecipato al progetto. Come si può vedere nella figura sottostante, il menu è suddiviso in tre schede "Tabella", "Cultivar" e "Mappa".


In **"Tabella"** è possibile cercare un'azienda semplicemente scrivendo una sua informazione (che può essere il nome, il codice o anche il comune) nella casella "Search" e cliccando il tasto Invio.


![tabella_aziende](https://user-images.githubusercontent.com/78078351/159449200-a4a694f2-aba5-4a13-bb64-974accde248e.png)


Accedendo alla scheda **“Cultivar”**è, inoltre, possibile visualizzare le cultivar principali del progetto tramite grafico a torta o grafico a barre. Questo, ma anche quasi tutti gli altri grafici, sono interattivi. Ciò significa che:


![cultivar](https://user-images.githubusercontent.com/78078351/159449235-ee3dd0e7-fe03-441f-93fb-1cbfb829bcf9.png)


* passando il mouse su una barra o un punto vengono visualizzate le relative informazioni
* cliccando una volta col tasto sinistro del mouse su un elemento della legenda (a destra del grafico) è possibile disabilitare/abilitare quell'elemento
* cliccando due volte velocemente col tasto sinistro del mouse su un elemento della legenda è possibile nascondere tutti gli elementi tranne quello cliccato. Per mostrare nuovamente tutti i dati, cliccare nuovamente due volte velocemente su un elemento della legenda. (se non funziona non sei stato abbastanza veloce. Con i touchpad dei portatili potrebbe essere più difficile).
* spostare, zoomare e scaricare il grafico tra le opzioni che escono in alto a destra del grafico quando si passa il mouse sullo stesso


In **"Dati meteo"** sono presenti le informazioni relative alle tre misure di precipitazioni che sono state prelevate dal progetto Copernicus. Nello specifico dai dati [ERA5-Land monthly averaged data from 1950 to present](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview) sono stati scaricati tre tipologie di misure: Precipitazioni totali, volume di acqua nello strato di suolo 7-28cm e volume di acqua nello strato di suolo 28-100cm. Per ulteriori informazioni riferirsi al link precedente. I dati si riferiscono ai mesi giugno-novembre sia dell'anno 2020 che 2021 e possono essere visualizzati sia in formato di mappa (sia statica che animata), che in forma di grafico. 


![meteo](https://user-images.githubusercontent.com/78078351/159449268-659a9c8e-3056-4adf-b5c0-8e49d802b5d7.png)

Nel grafico è possibile confrontare i dati meteo tra anni diversi e tra aziende. Nel caso del confronto tra aziende, è possibile scegliere l'anno (o gli anni), l'azienda (o le aziende) e la tipologia di grafico (statico o animato).


Nella scheda **“Mappa”** è, invece, possibile mostrare su mappa le aziende. Cliccando sul tasto “Carica mappa” verrà mostrata la mappa della regione Campania suddivisa in province in cui ogni punto si riferisce ad un’azienda (vedi figura sottostante). Dal menu a tendina è possibile selezionare la variabile in base alla quale colorare i punti delle aziende (es. per cultivar principale o per areale). 


![mappa_aziende](https://user-images.githubusercontent.com/78078351/159449290-339112e6-2863-48af-a221-6ebaf6c8fe39.png)


La mappa è anch'essa interattiva: ci si può spostare, zoommare, cambiare la mappa di base o nascondere un layer (tasto a sinistra sotto i pulsanti di zoom, le prime tre opzioni sono le mappe di base disponibili, shp e utmcoord23 sarebbero i layer utilizzati) e cliccando su un punto si apre un box con tutte le informazioni di quel punto.\
\
\




## 3.3. Campionamento azienda

In questo menu sono mostrati i dati relativi alle schede campionamento fornite da Aprol. Nello specifico, Aprol ha fornito tre tipologie di dati: le schede campionamento di drupe e foglie, le schede di campionamento dell’olio e le schede dei panel test sull’olio. Il menu è suddiviso in quattro sottomenu: “Drupe e foglie”, “Olio”, “Calendario campionamenti” e “Analisi sensoriali”. 

### Drupe e foglie

In Drupe e foglie sono mostrati tutti i dati relativi alle schede redatte dai tecnici al momento del campionamento di drupe e foglie. Per ogni azienda sono state redatte quattro schede di cui due relative alla fase fenologica e all’indice di maturazione del primo campionamento (drupe non ancora mature indicate con la sigla R1) e due relative al secondo campionamento (drupe pronte per il raccolto indicate con la sigla R2). Oltre a visualizzare questi dati in forma di tabella, vi è la possibilità di mapparli e graficarli. I grafici possibili sono lo scatterplot e il barplot. In entrambi i grafici è possibile selezionare l'informazione da utilzzare per l'asse X e per l'asse Y (a sinistra del grafico). Nel caso dello scatterplot è possibile scegliere l'anno e il numero del campionamento e l'informazione con la quale colorare i punti (in alto al grafico). Nel caso del barplot, invece, si possono comparare campionamenti diversi selezionando i campionamenti d'interesse dai box.


![scatt_fenologica](https://user-images.githubusercontent.com/78078351/159449335-0d0c18b2-a978-4f04-b913-97bebaa9d291.png)

Infine, nella scheda “Galleria” selezionando la riga dell’azienda è possibile mostrare le foto di foglie e drupe dei campioni raccolti. È possibile scegliere il numero di campionamento (primo o secondo) e l’anno della raccolta. 

### Olio

Analogamente agli altri menu, in Olio sono mostrate le schede campionamento dell’olio sia in forma di tabella che su mappa. Riferirsi ai menu precedenti.

### Calendario campionamenti

In Calendario campionamenti è presente un calendario che permette di visualizzare i giorni in cui sono stati effettuati i campionamenti sia di drupe e foglie che dell’olio. Si può decidere che tipo di dati mostrare, scegliere l’anno ed eventualmente mostrare solamente i dati di un’azienda.


![calendario](https://user-images.githubusercontent.com/78078351/159449364-26a53447-b4e5-4bbc-afe2-66829d9dae39.png)



### Analisi sensoriali
In Analisi sensoriali sono presenti i dati relativi ai panel test condotti sugli oli. È possibile visualizzare i dati in formato di tabella, creare tre tipi di grafici (scatterplot, barplot e spiderplot) in cui sono mostrati i valori di mediana fruttato, mediana amaro e mediana piccante, visionare i file originali forniti dagli assaggiatori e, infine, mappare i dati. Le uniche differenze rispetto ai grafici precedenti riguardano il barplot e lo spiderplot. Nel barplot è possibile selezionare due tipologie di grafico:

* Affiancato in cui per ogni azienda sono mostrate le tre barre relative alle tre misure, l'una accanto all'altra
* Impilato in cui le tre barre delle tre misure sono impilate l'una sopra l'altra. Ci sarà, dunque, una barra per ogni azienda.


![barplot_assaggi](https://user-images.githubusercontent.com/78078351/159449389-35bdc900-0e48-47f6-b8c5-6400d28f4549.png)


Nello spiderplot, selezionando un'azienda sono mostrate le tre misure. È possibile aggiungere una seconda azienda cliccando sulla relativa casella e poi scegliendo una seconda azienda.



## 3.4. Analisi laboratorio
In questo menu sono mostrati i dati sui polifenoli (sia totali che individuali) e sulla morfometria. Analisi laboratorio si suddivide in tre sottomenu: Polifenoli totali, Polifenoli individuali e Analisi morfometrica. 


### Polifenoli totali
Polifenoli totali contiene i dati relativi alla presenza totale dei polifenoli in drupe, foglie, olio, posa e sansa. La scelta della parte da analizzare è resa possibile dai pulsanti in blu posti sulla sinistra. Data la natura dei dati in questione (presenza di replicati tecnici) è stato aggiunto un tasto (vedi figura sottostante) che permetta di lavorare sia sui dati mediati (un valore per ogni azienda) che sui dati non mediati (più valori per ogni azienda).


![poltot_scatt](https://user-images.githubusercontent.com/78078351/159449420-90571421-d1aa-43c7-966a-9a626eadd033.png)


Questo tipo di scelta è presente anche nei polifenoli individuali e nella morfometria. Una volta scelto il tipo di dato, è possibile visualizzare una tabella, uno scatterplot, un barplot e la mappa. Al di sotto della tabella è presente anche un box informativo in cui viene effettuato un controllo sull’eventuale presenza di dati mancanti (cosìdetti “NA”). Qualora siano presenti, con un pulsante è possibile visualizzare due grafici che mostrano la loro distribuzione nel dataset. Questo tipo di controllo è presente in tutte le tabelle presenti nel menu Analisi laboratorio.


### Polifenoli individuali
Polifenoli individuali contiene, invece, i dati sui singoli polifenoli di drupe, foglie, olio e posa. 
Anche qui sono mostrati una tabella, la mappa, lo scatter plot e il barplot, ma a questi si aggiungono:

* una **Heatmap**
* un **Correlation plot**
* la possibilità di **visualizzare i cromatogrammi**
* la possibilità di **eseguire una PCA**

#### Heatmap
La Heatmap è un tipo di grafico in cui ogni cella assume un valore (ad esempio la concentrazione di un polifenolo) e viene colorata di conseguenza. L'heatmap presente in questo software si spinge oltre e presenta svariate funzionalità, come si può vedere dalla figura sottostante.


![heatmap](https://user-images.githubusercontent.com/78078351/159449437-cbb19b2f-624d-4275-b163-7209e5e5e6ce.png)


Tra le opzioni della Heatmap, oltre alla scelta del campionamento e dell'anno, è possibile:

* **Scala i dati.**Data la natura dei dati, scalare i dati si rivela molto utile per semplificare la visualizzazione dei dati, infatti i vari polifenoli hanno scale di valori anche molto diversi tra loro e, dunque, se si procede a generare l'Heatmap senza scalare i dati (opzione "No"), i polifenoli con concentrazione molto alta andranno a nascondere le informazioni relative ai polifenoli in concentrazione più bassa. In questo caso selezionando l'opzione "Per colonna" si effettua uno scaling lungo le aziende in modo da avere una scala di valori uniforme tra i vari polifenoli e vedere quale polifenolo è maggiormente presente in ogni azienda. Viceversa, se si è interessati maggiormente ad accentuare le differenze in polifenoli tra le aziende, si può scegliere l'opzione "Per riga" in cui si effettua uno scaling lungo i polifenoli in modo da avere una scala di valori uniforme tra le varie aziende. Questo opzione potrebbe essere utile per vedere quale azienda presenta i valori di polifenoli più alti.
* **Opzioni dendrogramma.** È possibile aggiungere altri elementi alla Heatmap tra cui la "Colonna annotazione" in cui è possibile selezionare una variabile da mostrare a lato della Heatmap e il clustering. Il clustering, in breve, è una tecnica di raggruppamento di elementi omogenei in un insieme di dati. Questo può essere effettuato sia su riga (raggruppamento delle aziende con valori di polifenoli più simili) che su colonna (raggruppamento dei polifenoli). È possibile, inoltre, scegliere con quanti cluster si vuole raggruppare i dati, la funzione di distanza e il metodo di clustering (è possibile lasciare i valori di default).
* **Personalizzazioni ulteriori.** Con l'Heatmap generata in OliveHealthR è possibile anche cliccare su una cella per visualizzare il singolo valore, o è anche possibile selezionare e zoommare un'area di interesse. Infine, in basso a sinistra della Heatmap ci sono altre opzioni tra cui quella di esportare il grafico come immagine.



#### Correlation Plot
Il Correlation Plot è la rappresentazione grafica della matrice di correlazione calcolata sui polifenoli. La correlazione può assumere valori compresi tra -1 e +1, dove per -1 si intende una forte correlazione negativa (ad esempio tra due polifenoli), per +1 si intende una forte correlazione positiva e per 0 si intende nessuna correlazione.


![corrplot](https://user-images.githubusercontent.com/78078351/159449470-69e11141-e0cd-4b01-b6f3-09d3d317344a.png)


#### PCA
Nella scheda PCA è possibile eseguire l'Analisi delle componenti principali (PCA), una tecnica di semplificazione dei dati che ha lo scopo di ridurre il numero più o meno elevato di variabili che descrivono un insieme di dati a un numero minore di variabili "artificiali" (dette componenti principali o PC), limitando il più possibile la perdita di informazioni. In questo caso il nostro dataset dei polifenoli individuali contiene 7 diversi polifenoli, ma se volessimo rappresentare graficamente queste informazioni non potremmo perchè il massimo delle dimensioni che è possibile graficare è 3 (grafico tridimensionale). La PCA permette, dunque, di ricavare un numero ridotto di componenti principali che possono essere più facilmente rappresentate su un grafico (da 7 a 2 dimensioni come nel caso del biplot). 

Dopo aver scelto se utilizzare i dati sintetizzati o quelli con i replicati, e aver selezionato il tipo di matrice su cui eseguirla (correlazione o covarianza), sono mostrati vari grafici informativi tra cui uno screeplot, il plot dei loadings (in cui si può scegliere il numero di componenti principali), il biplot (con la possibilità di scegliere sia la colonna di riempimento che una geometria) e il grafico tridimensionale con le prime tre componenti principali (anche qui è possibile scegliere la colonna  da usare come riempimento). Un esempio del biplot e del grafico tridimensionale è visionabile nella figura sottostante.


![pca](https://user-images.githubusercontent.com/78078351/159449533-7cb10506-d585-4fb2-9399-624b627c5d7a.png)


Plot e Biplot rappresentano i dati in uno spazio bidimensionale, utilizzando le prime due PC che spiegano il massimo della varianza. Sull'asse x si ha la prima Componente con la sua relativa varianza spiegata in percentuale, sull'asse y la seconda componente. Lo screeplot è un grafico che aiuta nella scelta del numero di PC da utilizzare (un valore di 90% sul secondo puntino indica che è possibile spiegare il 90% dei dati semplicemente utilizzando le prime due PC). Il plot dei loadings mostra, invece, la relazione tra le varie PC e le variabili originali. Il plot 3D è semplicemente un grafico tridimensionale delle prime tre PC.


### Analisi morfometrica 
Analisi morfometrica contiene i dati morfometrici relativi a foglie, drupe, endocarpo e rapporti e la scelta è data da quattro pulsanti posti a lato. Anche i dati morfometrici presentano un numero variabile di replicati per campione: ciò si traduce nella scelta di poter utilizzare sia i dati individuali che i dati sintetizzati (scegliendo, inoltre, la variabile attraverso la quale sintetizzare i dati). La parte dell’analisi morfometrica contiene funzionalità aggiuntive rispetto alle altre sezioni tra cui: un boxplot, un plot IOC, il clustering e vari test inferenziali. Inoltre, in “Galleria” è possibile visualizzare le foto di foglie o drupe che sono state misurate.

#### Tabella
Come già detto in precedenza, in analisi morfometrica è possibile scegliere la variabile per cui sintetizzare i dati. Per farlo, una volta abilitata l'opzione "Sintetizza i dati", selezionare una o più opzioni tra quelle proposte. Di default l'opzione selezionata è "Codice_azienda" dove i dati verranno sintetizzati per azienda (ogni azienda avrà un solo valore per misura); ma, se ad esempio si sceglie "Provincia", tutte le aziende appartenenti alla stessa provincia verranno sintentizzate e quindi si avranno solo cinque righe nella tabella (una misura per ogni provincia). La selezione multipla è leggermente più complessa: scegliere ad esempio "Cultivar_principale" e "Provincia" comporta il raggruppamento e la sintesi dei dati per queste due variabili e dunque, si avranno cinque valori per ogni cultivar, uno per provincia (es. Ravece-SA, Ravece-AV, Ravece-CE...). 


![tabella_morfo](https://user-images.githubusercontent.com/78078351/159449565-179762c8-ba61-41a4-b03e-381474183ccd.png)


Infine, data la tipologia di dati, è possibile scegliere anche quante cifre decimali visualizzare nella tabella attraverso l'opzione "Numero di cifre decimali".

#### Galleria
Il funzionamento della scheda Galleria è analogo agli altri, selezionando un’azienda dalla tabella è possibile visualizzare le foto che ritraggono le drupe o le foglie. Nel caso delle drupe sono presenti anche i modelli 3D ottenuti mediante microtomografie a raggi X come si può vedere nella figura sottostante.


![galleria_morfo](https://user-images.githubusercontent.com/78078351/159449615-beac552c-da02-47a3-a594-c65895c4d3d4.png)


#### Grafici
Relativamente ai grafici, sono presenti diversi tipi di grafici: boxplot, barplot, scatterplot, IOC, heatmap e correlation plot. Tra questi, l'unica differenza rispetto ai grafici dei polifenoli individuali è rappresentata dalla scheda **IOC**. Nei dati morfometrici sono state aggiunte delle variabili categoriche basate sulle classificazioni dell’International Olive Council (IOC). Questi dati sono mostrati nella relativa scheda, dove è possibile scegliere la misura IOC da visualizzare e il tipo di grafico tra grafico a torta e grafico a barre, Nel grafico a barre è possibile, inoltre, scegliere se usare la frequenza assoluta (essenzialmente sarebbe la conta delle classi per ogni azienda) o relativa (la stessa informazione ma in scala da 0 (0%) a 1 (100%). 



![ioc](https://user-images.githubusercontent.com/78078351/159449636-a20c5799-56ff-4875-a5c7-e6b26ab05371.png)


#### PCA
Nella scheda PCA è possibile eseguire una PCA sui dati morfometrici. Per ulteriori informazioni riferirsi alla PCA nei polifenoli individuali.


#### Clustering
Nella scheda Clustering è possibile eseguire un clustering sui dati morfometrici. Il clustering consiste in un insieme di metodi per raggruppare oggetti in classi omogenee. Un cluster è un insieme di oggetti che presentano tra loro delle similarità, ma che, per contro, presentano dissimilarità con oggetti in altri cluster. Gli algoritmi di clustering si dividono in due categorie principali: Algoritmi di clustering gerarchico e Algoritmi di clustering partizionale. I primi organizzano i dati in sequenze nidificate di gruppi che potremmo rappresentare in una struttura ad albero. Gli algoritmi di clustering partizionale, invece, determinano il partizionamento dei dati in cluster in modo da ridurre il più possibile la dispersione all’interno del singolo cluster e, viceversa, di aumentare la dispersione tra un cluster e un altro. La scelta dell'algoritmo da utilizzare è data dai due pulsanti in "Tipo di clustering" e, in base alla tipologia selezionata, sono mostrati grafici diversi quali gap statistic, silhouette plot, dendrogrammi, cluster plot. I grafici presenti nella scheda "Numero cluster" sono grafici che aiutano nella scelta del numero di cluster ottimale, mentre nella scheda Cluster plot vi sono i grafici con il clustering vero e proprio: dendrogramma nel caso del clustering gerarchico e cluster plot nel caso del clustering partizionale.



![clustering](https://user-images.githubusercontent.com/78078351/159449675-ec901411-5bce-4086-9416-bce3a27a37f0.png)


#### Test d'ipotesi
La scheda Test d’ipotesi contiene vari tipi di test inferenziali che è possibile eseguire sui dati morfometrici. Questi test sono stati suddivisi in quattro schede: 

* **Test correlazione**
* **Confronto tra due gruppi**
* **Confronto tra più gruppi**
* **Test d’indipendenza** 

In **Test di correlazione** è possibile confrontare due variabili diverse per vedere se vi è correlazione tramite il test di correlazione di Pearson. A questo è abbinato uno scatterplot con relativo modello lineare delle due variabili prese in considerazione. Ad esempio nella figura sottostante è mostrato il test di correlazione tra "Larghezza” e “Irregolarità_contorno”. Dai Shapiro-Wilk normality test emerge che entrambe le variabili presentano una distribuzione normale dei dati (p-value < 0.05), mentre il Pearson's product-moment correlation mostra che le due variabili sono abbastanza correlate positivamente (p-value < 0.05 e cor = 0.39). Oltre al test statistico si vede anche lo scatterplot con relativo modello lineare e intervallo di confidenza. 


![correlazione](https://user-images.githubusercontent.com/78078351/159449701-7d1669f1-13e7-4fe9-a893-0e03090f3ef3.png)


In **Confronto tra due gruppi** viene eseguito un T-test (o un Wilcoxon-Mann-Whitney test se una o più variabile non segue una distribuzione normale) tra una variabile dipendente e un fattore esplicativo in base ad una variabile numerica. Nell'immagine seguente è mostrato un esempio in cui, dopo aver scelto di fare il confronto utilizzando il Codice_azienda, si scelgono due aziende (SA_01 e BN_04) e le si confrontano in relazione alla variabile numerica (Rotondità). Dal Test sulla varianza le due aziende non hanno differenze significative in termini di varianza (p-value > 0.05), ma dal T-test (in Test statistico) emerge che esistono differenze significative tra le medie della Rotondità tra le due aziende (p-value < 0.05).


![ttest](https://user-images.githubusercontent.com/78078351/159449730-744fb855-053e-4ec2-8ecd-a1c1766d0bd7.png)


In **Confronto tra più gruppi** è possibile eseguire un ANOVA (one-way o two-way) o un Kruskal-Wallis. Se si sceglie l'opzione "One-way ANOVA", bisognerà scegliere una variabile numerica e una variabile categorica. Fatto ciò, oltre al classico test sulla normalità (se il p-value è minore di 0.05 scegliere "Kruskal-Wallis"), viene eseguito il test ANOVA (o Kruskal-Wallis). 

Nell'Anova one-way, un p-value significativo (< 0.05) indica che alcune delle medie dei gruppi sono differenti, ma non sappiamo quali gruppi sono differenti.Per sapere quali gruppi sono differenti si esegue un altro test chiamato post-hoc e ad esso è collegato un relativo grafico riassuntivo. Nell'immagine seguente è mostrato l'ANOVA condotta tra "Area" e "Cultivar_principale". In basso vi è il post-hoc (in questo caso Tukey HSD). Dal grafico del post-hoc emerge che, considerando l'Area, le cultivar Marinese e Frantoio hanno medie significativamente differenti.


![anova](https://user-images.githubusercontent.com/78078351/159449755-c30f5920-94bf-450e-a37f-2ebe76b5dbb5.png)


L'Anova two-way differisce dalla prima per la possibilità di aggiungere al modello una seconda variabile categorica. É possibile, inoltre, scegliere se utilizzare il modello additivo o il modello con interazione.

In **Test d’indipendenza** è possibile identificare la possibile associazione tra le categorie di due variabili qualitative (es. la Cultivar_principale e Lunghezza_IOC). In base ai tipi di dati si può scegliere se eseguire il test d’indipendenza Chi-quadro o il test esatto di Fisher. È possibile anche effettuare il test solo su una porzione di dati (ad esempio scegliendo solo alcune cultivar).



![chiquadro](https://user-images.githubusercontent.com/78078351/159449772-cf95f4e7-1c3d-45c1-92b8-ade79758eb3c.png)



## 3.5. Integrazione dati
Infine, nel menu Integrazione dati è possibile effettuare confronti tra misure diverse. Il menu si compone di tre schede: Tabella, Grafici e Test d'ipotesi. Alla sinistra di ogni scheda vi è una sezione in cui è possibile selezionare il tipo di confronto che si vuole effettuare: 

* scegliere **la prima variabile** di confronto (Polifenoli individuali o totali)
* scegliere **la seconda variabile** di confronto (Schede campionamento, Precipitazioni o Polifenoli individuali)
* scegliere **una misura** da confrontare tra Foglie, Drupe, Olio, Posa e Sansa (quest'ultimo solo in Polifenoli totali)
* scegliere **l'anno o gli anni**

Una volta fatta la selezione del confronto, nella scheda **Tabella** vi sarà riportata la tabella riassuntiva dei dati uniti e mediati. In basso è presente un tasto per poter scaricare i dati della tabella in formato excel.

Nella scheda **Grafici** sono riportati due tipi di grafici: uno scatterplot e un correlation plot. 
Lo scatterplot in questa parte è altamente personalizzabile. In "Dati da analizzare" è possibile scegliere che dati si vuole usare: scegliendo "Tutti" si usano tutti i dati che sono mostrati anche in tabella, se si sceglie "Filtra per cultivar" appare un'altra casella in cui è possibile una o più cultivar con le quali filtrare i dati. 
Al di sotto della scelta dei due assi, si ha un'altra parte chiamata "Altre opzioni" che mostra opzioni variabili in base alle scelte fatte. Nel caso in cui vengono selezionati più anni apparirà una casella in cui è possibile dividere il grafico in due parti (una parte per anno), se invece si decide di filtrare per cultivar, appariranno altre due caselle: "Density plot 2D", che aggiunge un density plot allo scatterplot, e "Fit" che aggiunge un modello di fit dei dati (è possibile scegliere tra "lm" ovvero un modello lineare e "loess" ovvero un modello polinomiale).

Per quanto riguarda la personalizzazione dello scatterplot, sopra di esso vi sono varie caselle in cui è possibile scegliere uno o più numeri di campionamento, una variabile con la quale colorare i punti, una per la dimensione dei punti (solo numerica) e una per la forma dei punti (solo categorica).

Inoltre, nella scheda seguente è possibile eseguire uno correlation plot (solo tra misure numeriche, il confronto con Schede campionamento è escluso da questo grafico).



Infine, in **Test d'Ipotesi** è possibile analizzare i confronti dal punto di vista statistico. Nella prima scheda vi è il test di correlazione (anche qui il confronto con Schede campionamento è escluso), nella seconda vi è il Confronto tra due gruppi (T-test) e nell'ultima è presente il confronto tra più gruppi (ANOVA). Per ulteriori informazioni sui test riferirsi al paragrafo della morfometria.


![conf_prec](https://user-images.githubusercontent.com/78078351/159449796-fae2fc66-2468-4a40-a093-6c080263fb0a.png)



***

# 4. Ringraziamenti
Si ringrazia la Regione Campania per aver aver finanziato questo progetto e tutti i partner coinvolti (Aprol Campania, CNR-IRET, CNR-IAC).

