#######Tesina Inferenza (Prima Parte)

## Introduzione dataset 

##Importo il Dataset
dati<-read.csv("atp_matches_2024.csv", header = TRUE, sep = ",")
attach(dati)

## Analisi marginale delle variabili 

#Iniziamo osservando la disribuzione marginale della variabile quantitativa relativa alla durata dell'incontro ("minutes"). Per farlo utilizziamo un diagramma a scatole e baffi accompagnato dal comando summary che ci permette di vedere con precisione quali valori assumono i quantili di base.

pdf("Boxplot_minutes.pdf")
boxplot(dati$minutes, boxwex = 0.3, ylab= "Durata (minuti)", main= "Diagramma a scatola e baffi di minutes") 
rug(dati$minutes, side=2)
dev.off()

summary(dati$minutes)

#Dalla visione congiunta di Boxplot e summary possiamo osservare come il 50% delle partite giocate nel 2024 abbia avuto una durata compresa tra gli 85 e i 139 minuti (rispettivamente primo e terzo quartile). Analizzando la mediana possiamo notare una sua leggera tendenza verso il lato inferiore della scatola evidenziando perciò una leggera asimmetria positiva della distribuzione di tale variabile. Grazie al rug() possiamo notare come la concentrazione delle osservazioni subisca una brusca variazione tra l'esterno e l'interno dell'intervallo 50-250 minuti (un numero limitatissimo di incontri si trovano al di fuori di tale intervallo di durata). Per quanto riguarda gli Outliers, mentre nella zona superiore ne troviamo parecchi, nella zona inferiore l'unico Outlier è 0 (partite vinte a tavolino a causa del ritiro dell'avversario).


#Adesso ritengo interessante osservare le distribuzioni di alcune coppie di variabili quantitative. Partiamo con le variabili w_ace e l_ace:
pdf("Boxplot w_ace e l_ace.pdf")
boxplot(dati[, c(28,37)], boxwex = 0.3, ylab = "Numero ace", main = "Diagramma a scatola e baffi di w_ace e l_ace") 
dev.off()

summary(dati$w_ace)
summary(dati$l_ace)

#Dalla visione congiunta dei boxplot risulta evidente come il boxplot dalla variabile l_ace sia più basso rispetto a quello della variabile w_ace. Questo risultato evidenzia come i vincitori facciano tendenzialmente più ace rispetto a chi perde. Entrambe le variabili presentano una distribuzione con una leggera asimmetria positiva. 

#Adesso vediamo i boxplot delle variabili w_df e l_df:
pdf("Boxplot w_df e l_df.pdf")
boxplot(dati[, c(29,38)], boxwex = 0.3, ylab = "Numero doppi falli", main = "diagramma a scatola e baffi di w_df e l_df")
dev.off()

summary(dati$w_df)
summary(dati$l_df)

#Questo risultato ci mostra come la variabile l_df abbia una distribuzione più spostata verso destra rispetto alla variabile w_df, essa infatti presenta una scatola più allungata rispetto a quella di w_df e una mediana più vicina al lato superiore rispetto che a quello inferiore, evidenziando perciò un'asimmetria negativa. In termini più pratici potremmo concludere dicendo che i vincitori tendono a commettere meno doppi falli e a fare un maggior numero di ace rispetto a chi perde.


#Un altro modo per rappresentare una variabile quantitativa è tramite il suo istogramma. Procedo allora col dividere le osservazioni della variabile "minutes" con un passo di 50 minuti, così da poter utilizzare tali classi per costruire il nostro istogramma:
table(cut(minutes, breaks = c(0, 50, 100, 150, 200, 250, 300, 350)))

pdf("istogramma_minutes.pdf")
hist(dati$minutes, breaks = c(0, 50, 100, 150, 200, 250, 300, 350), xlab = "Durata della partita (in minuti)", ylab = "Densità", main = "Istogramma di minutes", freq = FALSE)
dev.off()
#In un istogramma l'altezza dei rettangoli indica la densità di frequenza delle classi mentre la frequenza è rappresentata dall'area. In questo caso però abbiamo scelto classi di uguale ampiezza dunque l'altezza dei retangoli mi permette di confrontare sia le densità che le frequenze corrispondenti alle classi (se i rettangoli hanno tutti la stessa base ad un'altezza (densità) maggiore corrisponde anche un'area (frequenza) maggiore). Dalla visione dell'istogramma non possiamo che confermare ciò che abbiamo visto nell'analisi del Boxplot, la variabile infatti presenta una distribuzione asimmetrica positivamente e la maggior parte delle osservazioni ricadono nell'intervallo 50 - 150.

#Per avere ulteriori conferme circa la distribuzione della variabile calcoliamo gli indici di sintesi (Media, Deviazione Standard, Indice di Asimmetria e Curtosi):
install.packages("e1071")
library(e1071)

mean(dati$minutes, na.rm = TRUE)
var(dati$minutes, na.rm = TRUE)^(1/2)
skewness(dati$minutes, na.rm = TRUE)
kurtosis(dati$minutes, na.rm = TRUE)


#L'Indice di Asimmetria essendo positivo ci conferma ancora una volta un'asimmetria positiva della distribuzione, mentre, il Coefficiente di Curtosi evidenzia che si tratta di una distribuzione Leptocurtica, ovvero con code pesanti. In questo caso il valore discriminante è 0 e non 3 poiché uso la libreria "e1071".



## Analisi delle variabili qualitative

#Per quanto riguarda l'analisi di variabili qualitative reputo interessante porre l'attenzione sulla variabile w_continent che ho ottenuto convertendo i codici internazionali IOC della variabile winner_ioc nei rispettivi continenti. La variabile w_continent indica il continente dal quale provengono i vincitori.

#Creao la variabile e salvo tutto nel nuovo dataset "dati1"
#w_continent
install.packages("countrycode")
library(countrycode)
dati1<-dati
attach(dati1)
dati1$w_continent <- countrycode(winner_ioc, origin="ioc", destination="continent")
ambiguous_codes <- unique(dati1$winner_ioc[is.na(dati1$w_continent)])
print(ambiguous_codes)
dati1$w_continent[dati1$winner_ioc == "LIB"] <- "Asia"
dati1$w_continent[dati1$winner_ioc == "TGO"] <- "Africa"
table(dati1$w_continent)


#Grafico a barre
table(dati1$w_continent)
pdf("Grafico a Barre w_continent.pdf")
library(lattice)
tabella<- table(dati1$w_continent)
tabella_ord<- tabella[order(tabella, decreasing=T)]
barchart(tabella_ord, xlab = "Continente di provenienza dei vincitori", ylab = "Frequenza", main = "Grafico a barre w_continent", horizontal=F)
dev.off()
#Da questo grafico a barre si evince come nel 2024 gli Europei abbaino vinto il maggior numero di incontri seguiti dagli Americani, Oceaniani, Asiatici ed Africani. Questi dati però sono relativi alle vittorie cumulate nel corso dell'anno, un singolo giocatore può aver vinto un numero più o meno alto di incontri, dunque non possiamo concludere affermando che gli Europei siano più vincenti rispetto al resto del mondo perchè il numero di giocatori sui quali si distribuiscono le vittorie sarà diverso nei vari continenti. Reputo dunque interessante fare la stessa analisi contando il numero di giocatori vincenti e non il numero delle loro vittorie. In questo modo posso vedere su quanti giocatori si distribuiscono le vittorie per ciascun continente.

#Filtro il dataset (i vincitori vincenti compariranno una sola volta)
dati2<- dati1[!duplicated(dati1$winner_name), ]

#Creo il grafico a barre
table(dati2$w_continent)
pdf("Grafico a Barre continent_u_w.pdf")
tabella<- table(dati2$w_continent)
tabella_ord<- tabella[order(tabella, decreasing=T)]
barchart(tabella_ord, xlab = "Continente di provenienza dei vincitori (valori unici)", ylab = "Frequenza", main = "Grafico a barre continent_u_w", horizontal=F)
dev.off()
#In questo grafico a barre è interessante porre l'attenzione sull'Asia e Oceania. Possiamo notare come l'Asia superi l'Oceania alla quale corrisponde una frequenza molto simile a quella dell'Africa. Da questo possiamo dedurre che i giocatori Oceaniani siano più vincenti rispetto a quelli Asiatici perché le vittorie delli Oceaniani sono complessivamente maggiori di quelle degli Asiatici e sono distribuite su un minor numero di giocatori. Addirittura i giocatori Oceaniani che hanno vinto almento una partita sono stati 15, contro i 40 dell'Asia. Per quanto riguarda Europa, America e Africa poco cambia rispetto al grafico a barre visto in precedenza, i giocatori che hanno vinto almeno un incontro sono per la maggior parte Euroipei.

#Chiaramente questi risultati sono condizionati dal fatto che nei vari continenti abbiamo un numero diverso di tennisti che partecipano a tornei ufficiali a causa del fatto che il tennis è maggiormente diffuso in Europa (dove peraltro è nato) piuttosto che negli altri continenti.
#Per provare quanto detto creo e inserisco nel dataset la variabile continent_l (continente di provenienza dei perdenti) nello stesso modo con cui ho creato la variabile continent_w e filtro nuovamente il dataset in maniera tale che compaiano una sola volta i giocatori che hanno perso.


#l_continent
attach(dati1)
dati1$l_continent <- countrycode(loser_ioc, origin="ioc", destination="continent")
ambiguous_codes <- unique(dati1$loser_ioc[is.na(dati1$l_continent)])
print(ambiguous_codes)
dati1$l_continent[dati1$loser_ioc == "LIB"] <- "Asia"
dati1$l_continent[dati1$loser_ioc == "TGO"] <- "Africa"
table(dati1$l_continent)

dati3<- dati1[!duplicated(dati1$loser_name), ]

table(dati3$l_continent)

pdf("Grafico a Barre continent_u_w.pdf")
tabella<- table(dati3$l_continent)
tabella_ord<- tabella[order(tabella, decreasing=T)]
barchart(tabella_ord, xlab = "Frequenza", ylab = "Continente di provenienza dei perdenti unici", main = "Grafico a barre continent_u_l", horizontal=F)
dev.off()

#Dal confronto di quest'ultimo grafico a barre con quello precedente possiamo notare che l'Europa detiene sia il primato di singoli giocatori che hanno vinto nel 2024 sia quello di singoli giocatori che hanno perso. Mentre l'Africa è l'ultima sia per singoli giocatori che hanno vinto che per singoli giocatori che hanno perso. Questo evidenzia come una maggiore o minore presenza di giocatori provenienti da uno stesso continente influenzi fortemente i nostri risultati. Avessimo avuto lo stesso numero di giocatori nei vari continenti in quest'ultimo grafico a barre non avremmo dovuto avere l'Europa nuovamente prima e l'Africa ultima.

## Analisi esplorativa di coppie di variabili

#Adesso poniamo l'attenzione sulle due variabili quantitative w_ace (numero di ace realizzati dal vincitore) e winner_ht (Altezza del vincitore). Intuitivamente si potrebbe pensare ad una relazione positiva tra le due variabili poiché colpire la pallina ad un'altezza maggiore permette al giocatore di dare ad essa una traiettoria più verticale e di fargli raggiungere maggiore velocità. Per vedere se la nostra intuizione rispecchia la realtà useremo un diagramma di dispersione che riportiamo di seguito.
pdf("Diagramma di dispersione w_ace winner_ht.pdf")
plot(dati$winner_ht, dati$w_ace, ylab = "Numero di ace del vincitore", xlab = "Altezza del vincitore (in centimetri)", main = "Diagramma di dispersione")
dev.off()
#Dal diagramma possiamo notare come la nostra ituizione potrebbe essere corretta. All'aumentare dell'altezza vediamo aumentare il numero di ace realizzati dai giocatori. Essendo entrambe variabili quantitative discrete dal diagramma non riusciamo a vedere la frequenza relativa ai singoli punti, dunque calcoliamo l'indice di correlazione lineare per vedere se effettivamente esiste una relazione lineare tra le due variabili.
cor(dati$w_ace, dati$winner_ht, use = "complete.obs") 
#L'indice assume un valore positivo non molto alto, dunque è presente una leggera relazione lineare positiva.


#Adesso vediamo l'analisi di una variabile qualitativa ed una quantitativa ovvero tot_ace (Totale di ace) e surface (tipologia di superficie del campo). Data la tipologia di queste variabili è opportuno condurre l'analisi tramite un grafico a scatola e baffi condizionato.

#Creiamo la variabile tot_ace
dati4<-dati
dati4$tot_ace <- rowSums(cbind(dati4$w_ace, dati4$l_ace), na.rm = TRUE)

#Grafico a scotaloa e baffi condizionato
pdf("Diagramma a scatola e baffi di tot_ace condizionato a surface.pdf")
boxplot(dati4$tot_ace ~ dati4$surface, boxwex = 0.3, ylab = "Numero totale di ace per partita",xlab = "Superficie", main = "Diagramma a scatola e baffi di tot_ace condizionato a surface")
dev.off()
#Dal diagramma possiamo osservare come le partite giocate sul prato siano generalmente caratterizzate da un numero maggiore di ace. Non c'è da stupirsi da questo risultato, il prato infatti presenta un coefficiete di attrito inferiore rispetto al cemento o la terra battuta permettendo alla pallina di mantenere una velocità molto elevatta dopo il contatto col suolo. Inoltre il prato, proprio per la sua consistenza, limita il rimbalzo della pallina. Come risultato abbiamo palline che dopo aver toccato il suolo vanno ad una velocità maggiore rispetto a quando impattano con altre tipologie di supercici e tendono a rimanere più basse, combinazione perfetta per garantire un alto numero di ace.


#Adesso analizziamo le due variabili qualitative w_continent (continente dei vincitori) e l_continent (Continente dei perdenti), e per farlo useremo una tabella a doppia entrata.

tab<- table(dati1$w_continent, dati1$l_continent)
margin.table(tab,1)
margin.table(tab,2)
addmargins(tab)

#Per avere una rappresentazione più chiara usiamo un grafico a barre di continente_w condizionato alla variabile continent_l.

library(lattice)

barchart(table(dati1$w_continent, dati1$l_continent), ylab = "Continente giocatori vincitori", auto.key = list(title = "Continente giocatori perdenti", cex = 0.8))

#Dal grafico possiamo notare come i giocatori provenienti da qualsiasi continente, ad eccezione dell'Africa, abbiano vinto prevalentemente contro giocatori Europei. Questo chiaramente è determinato dal fatto che i giocatori europei siano molti di più rispetto ai giocatori provenienti da altri continenti. Per quanto riguarda i giocatori Africani essendo molto pochi è impossibile trarre informazioni dal grafico, dunque risulta più semplice guardare la tabella a doppia entrata.


##Analisi esplorativa di gruppi di variabili
#Adesso facciamo un'analisi incrociata tra 4 variabili quantitative attraverso una matrice di diagrammi a dispersione. Le variabili in questione sono winner_rank (Posizione del vincitore nel ranking al momento della partita), w_ace (Numero ace del vincitore), w_1stin (Prime battute valide del vincitore), e w_2ndWon (scambi in cui il vincitore ha sbagliato la prima battuta ma ha comunque vinto).

variabili <- names(dati)
data.frame(Posizione = seq_along(variabili), Variabile = variabili)

-------------------------------------------------------------------------------
##Idee aggiuntive
#(punti sulla prima battuta vincitore, punti sulla prima battuta perdente, durata)
dati5 <- dati[dati$draw_size == 128, ]
pairs(dati[, c(32,41,27)], main = "Scatter-plot matrix")

#(rank points vincitore, rank points perdente, durata)
dati5 <- dati[dati$draw_size == 128, ]
pairs(dati[, c(47,49,27)], main = "Scatter-plot matrix")
-------------------------------------------------------------------------------

pdf("Scatter-plot matrix.pdf")
pairs(dati[, c(46,28,31,33)], main = "Matrice di diagrammi a dispersione")
dev.off()

#winner_rank - w_ace: Possiamo vedere come giocatori che si trovano in posizioni basse del ranking riescano a fare pochi ace. Più ci si avvicina alle prime posizioni del ranking e più i giocatori vincitori sono in grado di fare ace.
#winner_rank - w_1stIn: qui possiamo notare come la nuvola dei punti sia prevalentemente schiacciata verso il basso. Giocatori con valori di ranking più bassi tendono a spaziare maggiormente nel numero di prime battute valide, mentre giocatori con valori più alti tendono a stabilirsi all'interno di un certo range (Il ranking è una scala decrescente, bassi valori di ranking equivalgono a giocatori più vincenti).
#winner_rank - w_2ndWon: anche qui abbiamo un grafico simile a quello appena visto. Il fatto che troviamo alti valori di w_2ndWon solo in corrispondenza di bassi valori di ranking può voler dire che giocatori più forti riescono a gestire meglio le situazioni con maggiore pressione ed anche dopo il fallimento della prima battuta riescono a vincere lo scambio.
#w_ace - w_1stIn: Dalla visione del grafico possiamo dire che non vi è una relazione lineare tra le due variabili e che all'aumentare del numero di prime battute valide non è detto che aumenti il numero di ace.
#w_ace - w_2ndWon: Anche qui non vi è una chiara relazione fra le variabili ma è interessante vedere come per alti valoro di w_ace non corrispondano bassi valori di w_2ndWon, questo può stare a significare che giocatori che fanno molti ace adottano una modalità di gioco più aggressiva che li porta a fare più ace ma allo stesso tempo a sbagliare magiormente la prima battuta recuperando con la seconda.
#w_1stIn - w_2ndWon: Qui invece troviamo una certa relazione lineare positiva. All'aumentare del numero di prime battute valide aumenta anche il numero di scambi vinti in seguito ad una seconda battuta. Questo significa che per quanto riguarda i giocatori vincitori più sono bravi con la prima battuta e più è probabile che sbagliandola comunque vinceranno lo scambio.


#Adesso condizioniamo la matrice alla varibiabile qualitativa minutes_cut (Variabile minutes divisa in tre classi, 0-100, 101-200, >200)
#Creo la variabile
dati5<-dati
dati5$minutes_cut <- with(dati5,
  ifelse(!is.na(dati5$minutes) & dati5$minutes >= 0 & dati5$minutes <= 100, 1,
    ifelse(!is.na(dati5$minutes) & dati5$minutes >= 101 & dati5$minutes <= 200,    	 2,
      ifelse(!is.na(dati5$minutes) & dati5$minutes > 200, 3, NA))))

#Matrice condizionata
pdf("Scatter plot matrix conditioned to minutes.pdf")
pairs(dati[, c(46,28,31,33)], pch = 21, bg = c("red", "blue", "grey")[as.factor(dati5$minutes_cut)], main = "Matrice di diagrammi a dispersione  (Red=0-100, Blue=101-200, grey>200)")
dev.off()
#Dalla matrice condizionata possiamo vedere qualcosa di molto intuitivo, ovvero che tutte le variabili scelte sono condizionate dal tempo, più è lungo l'incontro e più ace, prime battute valide e punti dopo una seconda battuta verranno fatti.

#Possiamo riassumere i risultati ottenuti con una matrice di correlazione.
cor(dati[, c(46,28,31,33)], use = "complete.obs") 


#Reputo interessante analizzare la dipendenza delle due variabili quantitative w_bpSavedRate (tasso di breakpoint vinti dal vincitore) e w_1stWonRate (tasso di scambi vinti dal vincitore dopo la prima battuta valida) al variare della variabile surface. Per avere una visione più chiara teniamo in considerazione solo i tornei con 128 iscritti (i più prestigiosi) e consideriamo il dataset ottenuto al netto degli incontri in cui il vincitore non ha affrontato breakpoint.

#Creo la variabile w_bpSavedRate e la variabile w_1stWonRate (consderando solo i grandi slam e togliendo i casi in cui la variabile bpFaced==0) 
dati6<- dati
dati6$w_bpSavedRate <- dati$w_bpSaved / dati$w_bpFaced
dati6 <- dati6[dati6$draw_size == 128, ]
dati6 <- dati6[dati6$w_bpFaced != 0 & !is.na(dati6$w_bpFaced), ]

dati6$w_1stWonRate <- dati6$w_1stWon / dati6$w_1stIn

#Scatter plot condizionato
library(lattice)
pdf("Scatter plot conditioned to surface.pdf")
xyplot(dati6$w_bpSavedRate ~ dati6$w_1stWonRate | dati6$surface, xlab = "w_1stWonRate", ylab = "w_bpSavedRate", main = "Diagramma di dispersione condizionato alla variabile surface")
dev.off()
#Possiamo notare come in tutti e tre i grafici la nuvola dei punti sia posta in alto a destra, questo significa che il suolo non influenza la dipendenza tra le due variabili quantitative e che in buona parte dei casi se al giocatore è associato un alto tasso di breakpoint vinti gli sarà associato anche un alto tasso di scambi vinti in seguito ad una prima battuta valida. Una possibile interpretazione potrebbe essere che la vittoria è data da una combinazione di aggressività (alto tasso di scambi vinti in seguito alla propria prima battuta) e buona gestione dei momenti delicati dell'incontro (alto tasso di breakpoint salvati).


#Adesso confrontiamo due variabili qualitative, continente del vincitore e continente del perdente, condizionate ad ua terza variabile, la superficie del campo.

table(dati1$w_continent, dati1$l_continen, dati1$surface)

library(lattice)
pdf("Matrice di diagrammi a nastro condizionata.pdf")
barchart(table(dati1$w_continent, dati1$l_continen, dati1$surface), ylab = "w_continent", auto.key = list(title = "surface", cex = 0.8))
dev.off()

#Quello che otteniamo sono tre tabelle a doppia entrata ed una loro rappresentazione grafica. 
#Per quanto riguarda il grafico, a causa della grande differenza tra giocatori Europei e giocatori provenienti da altri continenti, alcuni nastri risultano schiacciati, ma credo comunque che da esso si riescano ad ottenere informazioni interessanti. Potrebbe sembrare che gli Europei ed Americani siano più forti sul cemento ma se consideriamo che le partite disputate sul cemento sono quasi il doppio rispetto quelle disputate sulla terra battuta possiamo dire che sia Europei che Americani sono molto vincenti sull'erba.



#Adesso analizzaimo 4 variabili quantitative usando un grafico a stelle.
#Le variabili oggetto d'esame sono w_svpt (numero di punti fatti dal vincitore dopo aver servito), l_svpt (punti fatti dal perdente dopo aver servito), w_1stIn (Numero di prime battute valide del vincitore) e l_1stIn (Numero di prime battute valide del perdente). Per una migliore visualizzazione del grafico considereremo solo i tornei di medie dimensioni (draw_size=64) escludendo le partite vinte a tavolino (minutes=0).

dati7<- dati
dati7 <- dati7[dati7$draw_size == 64, ]
dati7 <- dati7[dati7$minutes != 0 & !is.na(dati7$minutes), ]
dati7$minutes_cut <- with(dati7,
  ifelse(!is.na(dati7$minutes) & dati7$minutes >= 0 & dati7$minutes <= 70, 1,
    ifelse(!is.na(dati7$minutes) & dati7$minutes >= 71 & dati7$minutes <= 140,    	 2,
      ifelse(!is.na(dati7$minutes) & dati7$minutes > 140, 3, NA))))
      
pdf("Diagramma a stelle non ordinato.pdf")
stars(dati7[, c(30,31,39,40)], key.loc = c(22,-2.5), mar = c(4, 0, 1, 0),main = "Diagramma a stelle", labels = rep("", nrow(dati7)))   
dev.off()
#Dal grafico possiamo notare come all'aumentare di una delle 4 variabili aumentino anche le altre (prismi più grandi), dunque reputo necessario ordinare in modo crescente una delle variabili così da ottenere prismi gradualmente sempre più grandi. Ordiniamo la variabile w_svpt.

dati7<- dati
dati7 <- dati7[dati7$draw_size == 64, ]
dati7 <- dati7[dati7$minutes != 0 & !is.na(dati7$minutes), ]
dati7 <- dati7[order(dati7$w_svpt), ] 
dati7$minutes_cut <- with(dati7,
  ifelse(!is.na(dati7$minutes) & dati7$minutes >= 0 & dati7$minutes <= 70, 1,
    ifelse(!is.na(dati7$minutes) & dati7$minutes >= 71 & dati7$minutes <= 140,    	 2,
      ifelse(!is.na(dati7$minutes) & dati7$minutes > 140, 3, NA))))  

pdf("Diagramma a stelle ordinato.pdf")
stars(dati7[, c(30,31,39,40)], key.loc = c(22,-2.5), mar = c(4, 0, 1, 0),main = "Diagramma a stelle", labels = rep("", nrow(dati7))) 
dev.off()
#Adesso il grafico risulta certamente più chiaro. Procediamo colorando i prismi in base alla fascia di durata degli incontri.
#Rosso:<70 minuti
#Blue: tra 71 e 140 minuti
#Arancione: >140 minuti

mappa_color <- c("1" = "red", "2" = "light blue", "3" = "orange")
colori_validi <- mappa_color[ dati7$minutes_cut ]

pdf("Diagramma a stelle colorato.pdf")
stars(dati7[, c(30,31,39,40)], key.loc = c(22,-2.5), mar = c(4, 0, 1, 0),main = "Diagramma a stelle", labels = rep("", nrow(dati7)), col.stars = colori_validi)
legend(x = 15, y = 3, col = c("green", "blue", "orange"), cex = 10, lwd = 3, bty = "n", legend = c("0-70", "71-140", ">140"))
dev.off()

#Possiamo notare come le 4 variabili aumentino all'aumentare della durata dell'incontro.


#Un altro modo per rappresentare graficmente gruppi di variabili quantitative è il diagramma a coordinate parallele. Proseguiamo con la rappresentazione delle stesse variabili così da trarre le stesse conclusioni attraverso una rappresentazione differente.

library(MASS)
parcoord(dati7[, c(30,31,39,40)], main = "Diagramma a coordinate parallele")

pdf("Diagramma a coordinate parallele condizionato.pdf")
custom_colors <- c("red", "light blue", "orange")
parcoord(dati7[, c(30,31,39,40)], main = "Diagramma a coordinate parallele", col = custom_colors[as.numeric(dati7$minutes_cut)])
legend(x = 2.3, y = 1, bty = "o", lty = 1, col = custom_colors, cex = 0.8, legend = c("<70 minuti", "tra 71 e 140 minuti", ">140 minuti"))
dev.off()
#Anche qui è evidente che all'aumentare della durata aumentano tutte le variabili.




###Rappresentazioni grafiche per serie temporali e spaziali
##Serie temporale

#Le serie temporali servono per visualizzare l'andamento di una variabile nel tempo. Nel nostro caso vedremo come varia la variabile w_monthly_ace_median (mediana mensile degli ace fatti dai vincitori).
#Consideriamo la mediana e non la media per avere un indice più stabile, non condizionato da outliers.

#Voglio creare una variabile w_monthly_ace_median in cui ho la mediana degli ace fatti per ciascun mese
dati8 <- dati

# Converte 'tourney_date' in formato Date; se tourney_date è numerico, lo trasformiamo in character
dati8$tourney_date <- as.Date(as.character(dati8$tourney_date), format = "%Y%m%d")

# Crea la nuova colonna 'tourney_month' estraendo il mese dalla data
dati8$tourney_month <- as.numeric(format(dati8$tourney_date, "%m"))

# Visualizza le prime righe per verificare il risultato
head(dati8[, c("tourney_date", "tourney_month")])

#Adesso creiamo una variabile w_monthly_ace_median in cui calcolo la mediana della variabile w_ace raggruppata per i mesi rappresentati dalla variabile tourney_month.

mediana_ace <- aggregate(w_ace ~ tourney_month, 
                       data = dati8[!is.na(dati8$w_ace), ], 
                       FUN = median)
names(mediana_ace)[names(mediana_ace) == "w_ace"] <- "w_monthly_ace_median"

dati8 <- merge(dati8, mediana_ace, by = "tourney_month", all.x = TRUE)

#Faccio la serie temporale
plot(dati8$tourney_month, 
     dati8$w_monthly_ace_median, 
     type = "o",              
     xlab = "Mese", 
     ylab = "Mediana Mensile di Ace", 
     main = "Serie Temporale: Mediana Mensile Ace fatti dai vincitori", 
     xaxt = "n")

# Imposto l'asse x con i mesi da 1 a 12
axis(1, at = 1:12)

#Poichè sappiamo che il numero di ace è influenzato dalla tipologia di superficie sulla quale si gioca (come abbiamo visto nel Diagramma a scatola e baffi di tot_ace), per analizzare questo grafico credo sia necessario osservare la frequenza con cui si è giocato nelle tre tipologie di superficie in ciascuno dei 12 mesi.

library(ggplot2)
library(scales)

pdf("Proporzioni tipologia superficie per ciascun mese.pdf")
ggplot(dati8, aes(x = factor(tourney_month), fill = surface)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = percent) +
  labs(x = "Mese", 
       y = "Proporzione degli incontri", 
       title = "Proporzione degli incontri per superficie in ciascun mese",
       fill = "Superficie") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#Dalla visione congiunta dei due grafici possiamo facilmente concludere che nei mesi di febbraio, marzo, aprile e maggio osserviamo delle basse mediane di ace realizzati dai vincitori e questo è causato dal fatto che in questi mesi si è giocato prevalentemente sulla terra battuta, superficie che limita fortemente il numero di ace.

library(ggplot2)
library(scales)

pdf("Proporzioni tipologia superficie per ciascun mese e mediana mensile ace vincitore.pdf")
scale_factor <- max(mediana_ace$w_monthly_ace_median, na.rm = TRUE)

# Costruiamo il grafico combinato
ggplot() +

  geom_bar(data = dati8, 
           mapping = aes(x = factor(tourney_month), fill = surface), 
           stat = "count", 
           position = "fill", 
           color = "black") +

  geom_line(data = mediana_ace, 
            mapping = aes(x = as.numeric(as.character(tourney_month)), 
                          y = w_monthly_ace_median / scale_factor, 
                          group = 1),
            color = "black", 
            size = 1.2) +
  geom_point(data = mediana_ace, 
             mapping = aes(x = as.numeric(as.character(tourney_month)), 
                           y = w_monthly_ace_median / scale_factor),
             color = "black", 
             size = 2) +

  scale_y_continuous(labels = percent,
                     sec.axis = sec_axis(~ . * scale_factor, name = "Mediana Mensile di Ace")) +
  scale_x_discrete(limits = as.character(1:12)) +
  labs(x = "Mese", 
       y = "Proporzione degli incontri", 
       title = "Proporzione degli incontri per superficie e Mediana di Ace dei vincitori per mese",
       fill = "Superficie") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3))
dev.off()
#In questo modo possiamo farci un'idea più chiara.


#Adesso rappresentiamo sempre la serie temporale della mediana degli ace mensili fatti dai vincitori ma questa volta la condizioniamo alla durata degli incontri.
#Consideriamo tre fasce di durata 1:<100 minuti, 2: tra 101 e 200 minuti, 3:>200 minuti ed otterremo una serie temporale con 3 spezzate.

dati9<-dati5

# Converte 'tourney_date' in formato Date; se tourney_date è numerico, lo trasformiamo in character
dati9$tourney_date <- as.Date(as.character(dati9$tourney_date), format = "%Y%m%d")

# Crea la nuova colonna 'tourney_month' estraendo il mese dalla data
dati9$tourney_month <- as.numeric(format(dati9$tourney_date, "%m"))

# Calcola la media di w_ace per ogni combinazione di tourney_month e surface,
# escludendo i casi in cui w_ace è NA
dati9 <- aggregate(w_ace ~ tourney_month + minutes_cut, 
                              data = dati9[!is.na(dati9$w_ace), ], 
                              FUN = median)

# Rinomina la colonna ottenuta
names(dati9)[names(dati9) == "w_ace"] <- "w_monthly_ace_median_duration"


dati9 <- dati9[order(dati9$tourney_month), ] 


# Estrai le modalità uniche della variabile surface
minutes_cut <- unique(dati9$minutes_cut)
# Definisci dei colori per le superfici (modifica se necessario)
colors <- c("orange", "light blue", "light green")

# Imposta il grafico vuoto con i range opportuni
pdf("Serie temporale mediana ace condizionata alla durata.pdf")
plot(range(dati9$tourney_month),
     range(dati9$w_monthly_ace_median_duration, na.rm = TRUE),
     type = "n", 
     xlab = "Mese del Torneo", 
     ylab = "Mediana Mensile di Ace", 
     main = "Numero mediano di Ace condizionato alla durata degli incontri")

# Per ogni modalità di surface, traccia i punti e la linea connessa
for(i in seq_along(minutes_cut)){
  subset_data <- dati9[dati9$minutes_cut == minutes_cut[i], ]
  lines(subset_data$tourney_month, 
        subset_data$w_monthly_ace_median_duration, 
        type = "o", 
        col = colors[i], 
        lwd = 2)
}

# Aggiungi una legenda che associa colori e superfici
legend("topright", legend = minutes_cut, col = colors, lty = 1, pch = 1)
dev.off()
#Anche qui possiamo notare come all'aumentare della durata dell'incontro tenda ad aumentare anche il numero di ace realizzati dai vincitori.



#Ora facciamo una serie temporale multivariata
#w_monthly_df_median
dati10 <- dati
dati10$tourney_date <- as.Date(as.character(dati10$tourney_date), format = "%Y%m%d")
dati10$tourney_month <- as.numeric(format(dati10$tourney_date, "%m"))


mediana_ace <- aggregate(w_ace ~ tourney_month, 
                       data = dati10[!is.na(dati10$w_ace), ], 
                       FUN = median)
names(mediana_ace)[names(mediana_ace) == "w_ace"] <- "w_monthly_ace_median"

dati10 <- merge(dati10, mediana_ace, by = "tourney_month", all.x = TRUE) 


mediana_df <- aggregate(w_df ~ tourney_month, 
                       data = dati10[!is.na(dati10$w_ace), ], 
                       FUN = median)
names(mediana_df)[names(mediana_df) == "w_df"] <- "w_monthly_df_median"
dati10 <- merge(dati10, mediana_df, by = "tourney_month", all.x = TRUE)


mediana_svpt <- aggregate(w_svpt ~ tourney_month, 
                       data = dati10[!is.na(dati10$w_svpt), ], 
                       FUN = median)
names(mediana_svpt)[names(mediana_svpt) == "w_svpt"] <- "w_monthly_svpt_median"
dati10 <- merge(dati10, mediana_svpt, by = "tourney_month", all.x = TRUE)


mediana_1stIn <- aggregate(w_1stIn ~ tourney_month, 
                       data = dati10[!is.na(dati10$w_1stIn), ], 
                       FUN = median)
names(mediana_1stIn)[names(mediana_1stIn) == "w_1stIn"] <- "w_monthly_1stIn_median"
dati10 <- merge(dati10, mediana_1stIn, by = "tourney_month", all.x = TRUE)


#Faccio le 4 serie storiche
library(lattice)
library(reshape2)

#Trasformo il dataset 'dati10' (che contiene le colonne indicate) da formato wide a formato long
dati10_long <- melt(dati10, id.vars = "tourney_month", measure.vars = c("w_monthly_ace_median", "w_monthly_df_median", "w_monthly_svpt_median", "w_monthly_1stIn_median"), variable.name = "metric", value.name = "value")

#Creo il grafico multivariato con xyplot: un pannello per ogni 'metric'
pdf("Serie temporale multivariata.pdf")
xyplot(value ~ tourney_month | metric,
       data = dati10_long,
       layout = c(1, 4), 
       xlab = "Mese",
       ylab = "Valore Mediano",
       main = "Serie Temporale multivariata",
       scales = list(x = list(at = 1:12, labels = as.character(1:12)),
                     y = list(relation = "free")),
       panel = function(x, y, ...) {
         panel.grid(h = -1, v = 0)          
         panel.lines(x, y, col = "light blue", lwd = 2)    
       },
       par.strip.text = list(cex = 1.2, font = 2))
dev.off()

#Possiamo notare come la mediana dei doppi falli effettuati dal vincitore non sembri essere influezata dal mese, mentre le altre 3 variabili sembrano seguire un andamento abbastanza simile.



##Serie Spaziale

#Per quanto riguarda l'analisi spaziale credo sia interessante rappresentare una mappa del mondo in cui ogni paese venga colorato in base al numero di partite vinte da giocatori provenienti da tale paese. Dunque non andiamo ad osservare per ciascun paese quanti giocatori hanno vinto almento un incontro nel 2024 ma vediamo per ciascun paese la somma delle partite vinte da parte di giocatori proveniente da tale paese (questo perchè ciascun giocatore può aver vinto più partite nel corso dell'anno).

library(dplyr)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(viridis)

# 1) prendo la mappa e rinomino adm0_a3 in iso_a3 (eliminando prima la colonna iso_a3 già esistente)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(-iso_a3) %>% 
  rename(iso_a3 = adm0_a3)

# 2) calcolo le frequenze per IOC → ISO-A3 usando recode()
freq_table <- dati %>%
  group_by(winner_ioc) %>%
  summarise(frequenza = n(), .groups="drop") %>%
  mutate(
    iso_a3 = recode(winner_ioc,
      "GER" = "DEU",
      "POR" = "PRT",
      "GRE" = "GRC",
      .default = winner_ioc
    )
  )

# 3) join “pulito”
map_data <- left_join(world, freq_table, by = "iso_a3")

# verifico che ora ci sia FRA
map_data %>% 
  filter(iso_a3 == "FRA") %>% 
  pull(frequenza)
# → restituisce il valore di frequenza (o NA se non ci sono dati)

# 4) coropletica 
ggplot(map_data) +
  geom_sf(aes(fill = frequenza), color = "gray80", size = 0.2) +
  scale_fill_viridis(option = "D", direction = -1, na.value = "white") +
  labs(fill = "Frequenza") +
  theme_minimal()



#Dalla mappa coropletica possiamo vedere come nel 2024 le partite siano state vinte principalmente da giocatori Italiani e Americani. In una fascia intermedia troviamo Argentini, Spagnoli, Francesi (anche se nella mappa non risulta), Tedeschi, Russi e Australiani ed a seguire il resto delle nazionalità. Tale mappa resta coerente con quanto abbiamo già visto nel corso della nostra analisi ovvero che il maggior numero di giocatori vincenti provenga dall'Europa. 



##Serie spazio temporale

#Adesso vediamo come è variato il numero di partite vinte dai giocatori di una stessa nazione nei 12 mesi del 2024.
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(viridis)
library(lubridate)

#1) preparo i dati con le date e il mese in formato "YYYY-MM"
data12 <- dati %>%
  mutate(
    tourney_date  = as.Date(as.character(tourney_date), "%Y%m%d"),
    tourney_month = format(tourney_date, "%Y-%m")
  )

#2) calcolo le frequenze mensili e ricodifica gli IOC in ISO3
freq_table <- data12 %>%
  count(tourney_month, winner_ioc, name = "frequenza") %>%
  mutate(
    iso_a3 = recode(winner_ioc,
      "GER" = "DEU",
      "POR" = "PRT",
      "GRE" = "GRC",
      .default = winner_ioc
    )
  )

#3) carico la mappa sf
world <- ne_countries(scale = "medium", returnclass = "sf")

#4) creo il vettore di tutti i mesi e lo suddivido in primi/ultimi sei mesi
all_months <- sort(unique(freq_table$tourney_month))
first_six <- all_months[1:6]
last_six  <- all_months[-(1:6)]

#5) cross-join fra ogni geometria e ciascun mese
months_df   <- data.frame(tourney_month = all_months)
world_months <- merge(world, months_df, by = NULL)

#6) unisco le frequenze usando il codice amministrativo adm0_a3 di Natural Earth
map_data <- left_join(
  world_months,
  freq_table,
  by = c("adm0_a3" = "iso_a3", "tourney_month")
)

#7) riempio di zero i NA e crea fill_val così che 0→NA (per avere bianco)
map_data <- map_data %>%
  replace_na(list(frequenza = 0)) %>%
  mutate(fill_val = ifelse(frequenza == 0, NA, frequenza))

#8) genero le coropletiche facet-wrap per i primi 6 mesi

map_plot_first <- map_data %>%
  filter(tourney_month %in% first_six) %>%
  ggplot() +
    geom_sf(aes(fill = fill_val), color = "gray80", size = 0.2) +
    scale_fill_viridis(option = "D", direction = -1, na.value = "white") +
    labs(fill = "Frequenza") +
    facet_wrap(~ tourney_month, ncol = 2) +
    theme_minimal(base_size = 16)

print(map_plot_first)


#9) e per gli ultimi 6 mesi
map_plot_last <- map_data %>%
  filter(tourney_month %in% last_six) %>%
  ggplot() +
    geom_sf(aes(fill = fill_val), color = "gray80", size = 0.2) +
    scale_fill_viridis(option = "D", direction = -1, na.value = "white") +
    labs(fill = "Frequenza") +
    facet_wrap(~ tourney_month, ncol = 2) +
    theme_minimal(base_size = 16)

print(map_plot_last)


#Da questa serie spazio-temporale possiamo notare come i giocatori americani abbiano vinto maggiormente nel mese di febbraio, luglio e agosto, mentre la Russia non presenta particolari picchi. L'Argentina invece presenta un solo picco ad aprile e L'Italia presenta frequenze molto alte nel periodo che va da aprile ad agosto. Dal grafico si evince anche che nel mese di dicembre si è disputato un numero molto limitato di partite, sicuramente a causa delle festività natalizie.



##Funzione di ripartizione empirica
#Adesso osserveremo la funzione di ripartizione empirica della variabile winner_age e l'accosteremo al suo istogramma così da osservare l'andamento della variabile da un diverso punto di vista. La funzione di ripartizione empirica, infatti, mostra per ogni valore della variabile la probabilità che la stessa assuma un valore minore o uguale ad esso.

plot(ecdf(dati$winner_age), do.points = F, verticals = T, xlab = "Età del vincitore", ylab = "Probabilità", main = "Funzione di ripartizione empirica di winner_age")
rug(dati$winner_age)


hist(dati$winner_age, xlab = "Età del vincitore", ylab = "Densità", main = "Istogramma di winner_age")
rug(dati$winner_age)


#Possiamo vedere che la maggior parte delle osservazioni le abbiamo tra i 20 e i 30 anni ma sono comunque presenti casi di partite vinte da giocatori con età intorno ai 45 anni.



##Massima verosimiglianza
#Prendiamo in esame la variabile winner_age. Facciamo finta che i dati che possediamo siano le osservazioni di un campione, campione estratto sotto ipotesi che la variabile winner_age si disponga normalmente. Le stime di massima verosimiglianza sono le seguent:

mean(dati$winner_age , na.rm = TRUE)
var(dati$winner_age , na.rm = TRUE)

#Adesso verifichiamo la validità del modello controllando i valori dei coefficienti campionari di asimmetria e curtosi (nel nostro caso per avere una distribuzione normale dovremmo ottenere entrambi i coefficienti pari a zero)

library(e1071)
skewness(dati$winner_age, na.rm = TRUE)
kurtosis(dati$winner_age, na.rm = TRUE)

#Abbiamo ottenuto entrambi i coefficienti positivi, dunque la variabile winner_age non si distribuisce normalmente ma la sua distribuzione presenta un'asimmetria positiva ed è leptocurtica (a code pesanti). Adesso vedremo anche una rappresentazione grafica di quanto detto attraverso un diagramma quantile-quantile. Con tale diagramma mettiamo a confronto le osservazioni standardizzate tramite la media e la varianza campionarie con i quantili della distribuzione normale standardizzata. 

qqnorm(dati$winner_age)
qqline(dati$winner_age)

#Avessimo avuto una distribuzione normale i punti si saebbero dovuti distribuire tutti lungo la retta, in questo caso invece vediamo che i punti si discostano da essa. Nella parte superiore possiamo notare come i punti di discostano maggiormente, ciò evidenzia che la coda di destra risulta più pesante e questo conferma l'asimmetria positiva che avevamo anticipato con l'indice di asimmetria. Possiamo notare però come anche nella parte inferiore i punti si discostano, seppur in maniera meno accentuata rispetto la parte superiore, evidenziando code pesanti che sono caratterizzanti di una distribuzione leptocurtica.




##Metodo dei minimi quadrati
#Adesso rappresentiamo la relazione che intercorre fra la variabile w_1stIn e w_2ndWon tramite un modello di regressione lineare. Chiaramente i parametri della retta di regressione li stimiamo con il metodo dei minimi quadrati ovvero minimizzando la somma degli scarti al quadrato dei valori osservati da quelli teorici della variabile di risposta. Tale relazione l'abbiamo già vista nella matrice di scatter plot fatta nelle prime pagine di questo elaborato ma adesso aggiungeremo la retta di regressione.

lm(dati$w_1stIn ~ dati$w_2ndWon)

cor(dati$w_1stIn, dati$w_2ndWon, use = "complete.obs")

#La nostra retta di regressione intercetta l'asse delle ordinate nel punto 24.264 ed ha un coeficiente angolare di 1.595. Tra le due variabili abbiamo dunque una relazione lineare positiva che però non risulta essere molto intensa dato che l'indice di correlazione lineare risulta essere circa 0.58. Vediamo graficamente.

pdf("Modello di regressione lineare.pdf")
plot(dati$w_1stIn ~ dati$w_2ndWon, xlab = "w_2ndWon", ylab = "w_1stIn", main = "Diagramma di dispersione")
abline(lm(dati$w_1stIn~ dati$w_2ndWon))
dev.off()

#Questo risultato lo si può interpretare nel seguente modo. Più un giocatore è bravo nella battuta (alto valore di w_1stIn) e maggiore è la probabilità che sbagliando la prima battuta il giocatore comunque riesca a battere correttamente la seconda pallina e a vincere lo scambio (alto valore di w_2ndWon).


##Regressione lineare locale
#I metodi di smorzamento vengono usati anche per la regressione lineare locale che, a differenza della semplice regressione lineare, dipende dal parametro h. In questa regressione più il parametro h è grande e più ci si avvicina alla retta di regressione ottenuta con il metodo dei minimi quadrati. Vedremo nuovamente la relazione tra le variabili w_1stIn e w_2ndWon ma questa volta concentrandoci sui tornei di medie dimensioni (64 partecipanti). Scegliamo tre valori del parametro h (h=3, 10, 30) e vediamo come cambia la nostra regressione locale.

library(sm)

plot(dati14$w_1stIn, dati14$w_2ndWon, xlab = "w_1stIn", ylab = "w_2ndWon", main = "Regressione lineare locale con diversi valori di h")

sm.regression(dati14$w_1stIn, dati14$w_2ndWon, h = 3, add = TRUE, col = "red")

sm.regression(dati14$w_1stIn, dati14$w_2ndWon, h = 10, add = TRUE, col = "blue")

sm.regression(dati14$w_1stIn, dati14$w_2ndWon, h = 30, add = TRUE, col = "green")

legend("topright", legend = c("h = 3", "h = 10", "h = 30"), col = c("red", "blue", "green"), lwd = 2)


#Possiamo vedere come all'aumentare di h otteniamo uno smorzamento maggiore della curva. Chiaramente anche qua più si smorza la curva e più informazioni perdiamo perché lo scopo di questa regressione è quello di cercare di spiegare la relazione tra le variabili con una curva anziché una retta proprio perchè una retta è troppo rigida e non può seguire localmente l'andamento della relazione fra le variabili. Inoltre tutte e tre le curve sono più instabili sulle code a causa del fatto che in quelle aree troviamo una minore densità di osservazioni e queste finiscono per influenzare localmente la nostra regressione (nella regressione lineare con il metodo dei minimi quadrati quei valori avrebbero influenzato l'intera retta). Per quanto riguarda la nostra analisi, tra le due variabili possiamo confermare la presenza di una relazione lineare positiva anche nel sottoinsieme dei tornei di medie dimensioni.

#Per concludere possiamo procedere al calcolo automatico del parametro di smorzamento h. I metodi che useremo saranno il metodo dei gradi di libertà approssimati (df), il metodo di cross-validation (cv) ed il metodo loess.


library(sm)

plot(dati14$w_1stIn, dati14$w_2ndWon, xlab = "w_1stIn", ylab = "w_2ndWon", main = "Regressione lineare locale con metodi df, cv e loess")

sm.regression(dati14$w_1stIn, dati14$w_2ndWon, method = "df", add = TRUE, col = "red", lwd = 2)

sm.regression(dati14$w_1stIn, dati14$w_2ndWon, method = "cv", add = TRUE, col = "green", lwd = 2)

od <- na.omit(dati14[order(dati14$w_1stIn), c("w_2ndWon", "w_1stIn")])

loess_model <- loess(w_2ndWon ~ w_1stIn, data = od, span = 0.5)

lines(od$w_1stIn, fitted(loess_model), col = "blue", lwd = 2)

legend("topright", legend = c("df", "cv", "loess"), col = c("red", "green", "blue"), lwd = 2)

#Possiamo vedere come con i metodi df e cv otteniamo più o meno la medesima regressione ed entrambe confermano un andamento grossomodo crescente. Con il metodo loess invece otteniamo una regressione che risulta crescente fino ad un certo punto per poi diventare decrescente.












------------------
variabili <- names(dati)
data.frame(Posizione = seq_along(variabili), Variabile = variabili)

datidef<- dati[, c(2,3,4,6,11,12,13,15,19,20,21,23,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,44,45,46,47)]
-------------------


