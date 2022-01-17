# R code complete - Telerilevamento Geo-Ecologico
#Chiara Teodoro

#------------------------------------------------

# Summary:

# 1. Remote sensing first code
# 2. R code time series 
# 3. R code Copernicus data
# 4. R code knitr
# 5. R code multivariate analysis
# 6. R code classification
# 7. R code ggplot2
# 8. R code vegetation indices
# 9. R code land cover
# 10. R code variability
# 11. R code spectral signatures


#------------------------------------------------

# 1. Remote sensing first code

# My frst code in R for remote sensing!!!
# Il mio primo codice in R per il telerilevamento!

# install.packages("raster") #per installare il pacchetto raster, lo scrivo tra “” perché lo prendo da fuori R
library(raster) #con library() richiamo i pacchetti che hai già installato


# setwd("C:/lab/lab_Brazil") # Windows,imposto la working directory cioè dico a R il percorso che deve fare per caricare le immagini, in questo caso è la cartella lab in C


p224r63_2011 <- brick("p224r63_2011_masked.grd") #brick importa tutte le bande dell’immagine, con la freccina creo il file, in questo caso l’immagine, con tutte le bande importate
p224r63_2011 #scrivo il nome del file per vedere tutte le sue informazioni

plot(p224r63_2011) # plotto l’immagine che vuol dire che la posso vedere, avendo l’immagine più bande ogni banda verrà plottata singolarmente

# colour change, cioè cambio i colori dei plot, creo una palette che chiamo cl e poi la applico al plot, scrivendo nella parentesi il nome dell’immagine e col= nome palette
cl <- colorRampPalette(c("black","grey","light grey")) (100) #100 è il numero di sfumature dei colori
plot(p224r63_2011, col=cl)



# colour change, provo a creare diverse palette e le applico al plot
cl <- colorRampPalette(c("blue","green","grey","red","magenta","yellow")) (100) 
plot(p224r63_2011, col=cl)

cls <- colorRampPalette(c("red","pink","orange","purple")) (200) #qui provo con 200 sfumature
plot(p224r63_2011, col=cls)                       

#### DAY 3
# Bande Landsat
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio
# B6: infrarosso termico
# B7: infrarosso medio

# dev.off will clean the current graph, nel senso che cancello il plot precedente
dev.off()

plot(p224r63_2011$B1_sre) #il simbolo $ serve a legare un elemento all’immagine, qui lego solo la banda B1 del blu

cls <- colorRampPalette(c("red","pink","orange","purple")) (200) #creo la palette che voglio applicare
plot(p224r63_2011$B1_sre, col=cls) #plotto immagine a cui ho legato la banda B1 con la color palette creata

dev.off()


plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)

# par è un comando per plottare due o più immagini, i numeri sono: il primo righe e il secondo colonne, in cui si disporranno le immagini, in questo caso avremo una riga con due immagini una accanto all’altra
par(mfrow=c(1,2))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)

# 2 row, 1 columns, provo 2 righe e una colonna
par(mfrow=c(2,1)) # se voglio mettere prima le colonne scrivo: par(mfcol....)
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)

# plotto le prime 4 bande legandole all’immagine
par(mfrow=c(4,1)) # 4 righe e 1 colonna così le avrò una sotto l’altra
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)
plot(p224r63_2011$B3_sre)
plot(p224r63_2011$B4_sre)

# a quadrat of bands, le plotto 2x2
par(mfrow=c(2,2))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)
plot(p224r63_2011$B3_sre)
plot(p224r63_2011$B4_sre)

# a quadrat of bands, le plotto sempre 2x2 ma per ogni banda creo una palette di colori e la applico
par(mfrow=c(2,2))
clb <- colorRampPalette(c("dark blue","blue","light blue")) (100) #nota che qui i colori sono sul blu e sotto li applico alla banda del blu, simile anche nelle altre bande
plot(p224r63_2011$B1_sre, col=clb)
clg <- colorRampPalette(c("dark green","green","light green")) (100)
plot(p224r63_2011$B2_sre, col=clg)
clr <- colorRampPalette(c("dark red","red","pink")) (100)
plot(p224r63_2011$B3_sre, col=clr)
clnir <- colorRampPalette(c("red","orange","yellow")) (100)
plot(p224r63_2011$B4_sre, col=clnir)

# Visualizing data by RGB plotting, RGB sta per red green blu plot, è un plot basato su questi tre layers combinati in modo che rappresentino il canale del rosso verde e blu, questa funzione può essere usata per fare immagini true o false color da landsat e altre immagini satellitari con più bande


# Bande Landsat
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio
# B6: infrarosso termico
# B7: infrarosso medio

#associo una componente dell’RGB a ogni banda di landsat, ad es. r è l’indice del canale del rosso, il valore va da 1 a n layers di “x” dove x è l’immagine caricata in RasterBrick o RasterStack
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") #stretch serve ad allungare in modo lineare i valori per aumentare il contrasto, r=3, g=2 e b=1 serve per dare un’idea dei colori reali dell’immagine
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") #provo altri indici
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") #la banda 4 è l’infrarosso
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

# creo multiframe con par con i diversi plotRGB
par(mfrow=c(2,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

pdf("il_mio_primo_pdf_con_R.pdf") #creo pdf con i risultati del plotRGB
par(mfrow=c(2,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")
dev.off() #il dev.off è importante, se no non ti salva il pdf

# Multitemporal set, uso un’altra immagine dello stesso posto, quindi stesse coordinate, ma del 1988
p224r63_1988 <- brick("p224r63_1988_masked.grd")
p224r63_1988

# Bande Landsat
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio
# B6: infrarosso termico
# B7: infrarosso medio

plot(p224r63_1988) #plotto tutte le bande
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin") #plotto in RGB truecolor
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") #plotto in RGB e controllo la vegetazione grazie all’infrarosso infatti e la vegetazione ha un’alta riflettanza nell’infrarosso

# hist, provo un altro tipo di stretch che si chiama histogram e lo plotto in un multiframe per vedere le differenza tra i due tipi di stretch
par(mfrow=c(2,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="hist")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="hist")



# par natural colours che sarebbe truecolors, flase colours, and false colours, sono diverse combinazioni, provale e guarda come vedi diversamente l’immagine
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="hist")

#------------------------------------------------

# 2. R code time series

# Time series analysis
# Greenland increase of temperature
# Data and code from Emanuela Cosma

# install.packages("raster") 
# install.packages("rasterVis") #devo installare questo che è nuovo
library(raster) #richiamo sempre i pacchetti installati che mi servono
library(rasterVis) 

# library(rgdal) 


# setwd("C:/lab/Greenland") # Windows #devi sempre settare la working directory, controllo come si chiama la cartella delle immagini che mi servono 


lst_2000 <- raster("lst_2000.tif") #con raster creo un unico file
lst_2005 <- raster("lst_2005.tif") #ripeto l’operazione precedente
lst_2010 <- raster("lst_2010.tif")
lst_2015 <- raster("lst_2015.tif")

# par, plotto le immagini caricate in un multiframe 2x2
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

# con questa funzione creo una lista
rlist <- list.files(pattern="lst") #con pattern indico quali file voglio inserire nella lista, ad esempio in questo caso voglio le immagini che contengono lst nel nome, lst è la temperatura superficiale
rlist

import <- lapply(rlist,raster) #applico la funzione raster agli elementi che ho inserito nella lista, infatti “lapply” serve ad applicare la stessa funzione a un gruppo di immagini
import #controllo il risultato
#impacchetto in un solo file con la funzione stack
TGr <- stack(import) 
Tgr #controllo il risultato
plot(TGr) #plotto il risultato

plotRGB(TGr, 1, 2, 3, stretch="Lin") #uso plotRGB e uso 3 layer diversi per vedere nei diversi anni quale colore è dominante e quindi come varia lst
plotRGB(TGr, 2, 3, 4, stretch="Lin") 
plotRGB(TGr, 4, 3, 2, stretch="Lin") 

levelplot(TGr) #levelplot è una funzione che disegna level plot e contour plot, puoi mettere più informazioni rispetto plot
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
levelplot(TGr, col.regions=cl)

levelplot(TGr,col.regions=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015")) #ho aggiunto degli attributi al plot

levelplot(TGr,col.regions=cl, main="LST variation in time",
          names.attr=c("July 2000","July 2005", "July 2010", "July 2015")) #ho aggiunto il nome, cioè il titolo dell’immagine che sto plottando

# Melt, creo una lista con i file che contengono melt nel nome
meltlist <- list.files(pattern="melt")
melt_import <- lapply(meltlist,raster) #applico la funzione raster a tutte le immagini grazie alla funzione lapply
melt <- stack(melt_import) #impacchetto in un file con la funzione stack
melt #controllo i risultati

levelplot(melt) #plotto i dati con leveplot

melt_amount <- melt$X2007annual_melt – melt$X1979annual_melt #faccio questa sottrazione così posso vedere dove c’è stato cambiamento, devo legare il layer del file con $, infatti sto facendo una sottrazione tra i due layers

clb <- colorRampPalette(c("blue","white","red"))(100) #cambio la palette di colori
plot(melt_amount, col=clb) #plotto

levelplot(melt_amount, col.regions=clb) #plotto con levelplot

#------------------------------------------------

# 3. R code Copernicus data

# R_code_copernicus.r
# Visualizing Copernicus data

install.packages("ncdf4") #devo installarlo 
library(raster)
library(ncdf4) #richiamo i pacchetti che mi servono

setwd("C:/lab/Copernicus/") #imposto la working directory
 

albedo <- raster("c_gls_ALBH_202006130000_GLOBE_PROBAV_V1.5.1.nc") #importo i dati

cl <- colorRampPalette(c('light blue','green','red','yellow'))(100) #cambio i colori
plot(albed0, col=cl) #plotto con la palette di colori che ho creato

# resampling, con la funzione aggregate faccio un ricampionamento, in questo caso diminuisco il numero di pixel alleggerendo il peso dell’immagine
albedores <- aggregate(albedo, fact=100)
plot(albedores, col=cl)



#------------------------------------------------

# 4. R code knitr



require(knitr) #il pacchetto knitr fornisce un tool per generare un report dinamici usando Literate Programming Techniques
library(knitr)
library(tinytex)
#tinytex::install_tinytex()
#serve per poter usare la funzione    
tinytex::tlmgr_update()
#Richiamo un codice per poter creare il template (il template serve a creare il “foglio bianco” su cui scrivi l’Rscript)
stitch("R_code_Greenland.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr")) #questo serve per creare un file di testo appunto un report in cui ci sono i risultati con gli script utilizzati. Stitch è una funzione per fare reporting automatico a piccola scala


stitch("~/Downloads/R_code_temp.r", template=system.file("misc", "knitr-template.Rnw", package="knitr")) #richiamo un codice già scritto per creare il template


#------------------------------------------------

# 5. R code multivariate analysis

# R_code_multivariate_analysis.r

library(raster)
library(RStoolbox)


setwd("C:/lab/lab_Brazil") # Windows


p224r63_2011 <- brick("p224r63_2011_masked.grd") #carico immagine con brick

plot(p224r63_2011) #plotto l’immagine

p224r63_2011 #guardo le informazioni dell’immagine

plot(p224r63_2011$B1_sre, p224r63_2011$B2_sre, col="red", pch=19, cex=2) #plotto la due bande B1 e B2 che ho legato all’immagine in un grafico x,y, attributi estetici: col= è colore, pch= è la forma dei simboli nel grafico, cex= è la dimensione dei simboli

pairs(p224r63_2011) #questa funzione fa tutte le correlazioni possibili, anche indice r che indica il grado di correlazione

# aggregate cells: ricampionamento, uso la funzione aggregate per ricampionare usando un numero minore i pixel
p224r63_2011res <- aggregate(p224r63_2011, fact=10)

par(mfrow=c(2,1)) #plotto le immagini una sotto l’altra
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="lin")
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch="lin")

p224r63_2011res_pca <- rasterPCA(p224r63_2011res) #faccio l’analisi PCA con la funzione rasterPCA, ovvero con la analisi delle componenti principali cerco quale componente ti dà più informazioni su quei dati

summary(p224r63_2011res_pca$model) # uso la funzione summary per vedere i risultati devo legare con $ model perché model è la parte del file dove fai l’operazione di analisiPCA. La funzione summary() ti fa vedere la deviazione standard e la Proporzione di VARIANZA, chi ha la varianza più alta è la componente che dà più informazioni, in pratica quanta varianza spiega ogni banda

dev.off() #così chiudo il par
plotRGB(p224r63_2011res_pca$map, r=1, g=2, b=3, stretch="lin") #plotto in RGB, c’è $map perché devo plottare la mappa, e il file ha 3 elementi (mappa, model e call)


#------------------------------------------------

# 6. R code classification

# R_code_classification.r

library(raster)
library(RStoolbox)


# setwd("C:/lab/solar_orbiter/") #setto la working directory


so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg") #carico l’immagine con brick
so

plotRGB(so, 1,2,3, stretch="lin") #plotto l’immagine per vederla

soc <- unsuperClass(so, nClasses=3) #unsupervised classification, viene effettutata dal software, è una classificazione non supervisionata automatica che divide l’immagine per il numero di classi che hai scelto (in questo caso 3). ogni colore avrà un valore diverso e quindi ogni pixel di un certo tipo di range di valori sarà raggruppato in una classe.
plot(soc$map) #devo legare $map perché la funzione ha creato diversi elementi in output

soc20 <- unsuperClass(so, nClasses=20) #provo unsuperclass con 20 classi
plot(soc20$map)

cl <- colorRampPalette(c('yellow','black','red'))(100) #cambio colori
plot(soc20$map,col=cl)


# Grand Canyon, ora uso immagini del Grand Canyon
# https://landsat.visibleearth.nasa.gov/view.php?id=80948

# When John Wesley Powell led an expedition down the Colorado River and through the Grand Canyon in 1869, he was confronted with a daunting landscape. At its highest point, the serpentine gorge plunged 1,829 meters (6,000 feet) from rim to river bottom, making it one of the deepest canyons in the United States. In just 6 million years, water had carved through rock layers that collectively represented more than 2 billion years of geological history, nearly half of the time Earth has existed.

gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg") #carico l’immagine

#plotto l’immagine in RGB con due tipi di stretch
plotRGB(gc, r=1, g=2, b=3, stretch="lin") 
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

gcc2 <- unsuperClass(gc, nClasses=2) #faccio unsupervised classification con 2 classi
gcc2
plot(gcc2$map)

gcc4 <- unsuperClass(gc, nClasses=4) #rifaccio la classificazione con 2 classi
plot(gcc4$map)

#------------------------------------------------

# 7. R code ggplot2

library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

setwd("~/lab/lab_Brazil") #sistemo working directory

p224r63 <- brick("p224r63_2011_masked.grd") #carico l’immagine

ggRGB(p224r63,3,2,1, stretch="lin") #plotto con ggRGB
ggRGB(p224r63,4,3,2, stretch="lin")

#prima di poter inserire le immagini in una finestra devo creare file p1 per la prima immagine e file p2 con la seconda immagine
p1 <- ggRGB(p224r63,3,2,1, stretch="lin") 
p2 <- ggRGB(p224r63,4,3,2, stretch="lin")

grid.arrange(p1, p2, nrow = 2) #con ggRGB uso grid.arrange e non par

#------------------------------------------------

# 8. R code vegetation indices

# R_code_vegetation_indices.r

library(raster) 
library(RStoolbox) # for vegetation indices calculation 
install.packages("rasterdiv")
library(rasterdiv) # for the worldwide NDVI
install.packages("rasterVis")
library(rasterVis)


setwd("C:/lab/deforestazione/") #faccio working directory


defor1 <- brick("defor1.jpg") #carico le immagini
defor2 <- brick("defor2.jpg")

# b1 = NIR, b2 = red, b3 = green

par(mfrow=c(2,1)) #plotto le immagini
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")


# difference vegetation index: DVI, devo fare la differenza tra banda dell’infrarosso e banda del rosso

defor1 #controllo le informazioni e le bande

# time 1, lego all’immagine la banda dell’infrarosso (defor1$defor1.1) e la banda del rosso (defor1$defor1.2 e faccio la sottrazione
dvi1 <- defor1$defor1.1 - defor1$defor1.2

dev.off() #devo eliminare il par di prima
plot(dvi1)

cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme

plot(dvi1, col=cl, main="DVI at time 1") #plotto dvi1

# time 2, faccio le stesse operazioni per il secondo periodo di tempo
dvi2 <- defor2$defor2.1 - defor2$defor2.2

plot(dvi2, col=cl, main="DVI at time 2")

par(mfrow=c(2,1)) #plotto l’indice DVI nei due periodi considerati
plot(dvi1, col=cl, main="DVI at time 1")
plot(dvi2, col=cl, main="DVI at time 2")

difdvi <- dvi1 – dvi2 #faccio la differenza tra DVI dei due periodi

# dev.off() #così elimino il par
cld <- colorRampPalette(c('blue','white','red'))(100) #creo palette da applicare
plot(difdvi, col=cld) #plotto la differenza tra dvi con la color palette creata


# ndvi
# (NIR-RED) / (NIR+RED) #è l’indice dvi normalizzato

#calcolo indice NDVI per la prima immagine
ndvi1 <- (defor1$defor1.1 - defor1$defor1.2) / (defor1$defor1.1 + defor1$defor1.2)
plot(ndvi1, col=cl)

#faccio le stesse operazioni per la seconda immagine
ndvi2 <- (defor2$defor2.1 - defor2$defor2.2) / (defor2$defor2.1 + defor2$defor2.2)
plot(ndvi2, col=cl)


difndvi <- ndvi1 – ndvi2 #faccio la differenza tra ndvi del primo periodo (prima immagine) e secondo periodo (seconda immagine)

# dev.off()
cld <- colorRampPalette(c('blue','white','red'))(100) #creo palette da applicare
plot(difndvi, col=cld) #plotto la differenza di ndvi con la palette appena creata


# Rstoolbox::spectralIndices questa è una funzione del pacchetto Rstoolbox, serve a calcolare automaticamente tutti gli indici possibili per quell’immagine

vi1 <- spectralIndices(defor1, green = 3, red = 2, nir = 1)
plot(vi1, col=cl) #plotto il risultato

vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1) #faccio le stesse operazioni per la seconda immagine
plot(vi2, col=cl)

# worldwide NDVI
plot(copNDVI)


# Pixels with values 253, 254 and 255 (water) will be set as NA’s.
# la funzione reclassify serve a riclassificare, in questo caso sto eliminando il contributo dell’acqua
copNDVI <- reclassify(copNDVI, cbind(253:255, NA)) #cbind cambia i valori dei pixel con valori 253, 254 e 255
plot(copNDVI)

# rasterVis package needed:nel senso che questa funzione ha bisogno del pacchetto rasterVIS
levelplot(copNDVI)

#------------------------------------------------

# 9. R code land cover

# R_code_land_cover.r

library(raster)
library(RStoolbox) 
# install.packages("ggplot2")
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra) # for grid.arrange plotting


# setwd("C:/lab/deforestazione/") # setto la working directory


# NIR 1, RED 2, GREEN 3

defor1 <- brick("defor1.jpg") #carico l’immagine
plotRGB(defor1, r=1, g=2, b=3, stretch="lin") #plotto l’immagine in RGB
ggRGB(defor1, r=1, g=2, b=3, stretch="lin") #uso ggRGB

#ripeto le operazioni per la seconda immagine
defor2 <- brick("defor2.jpg")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
ggRGB(defor2, r=1, g=2, b=3, stretch="lin")

par(mfrow=c(1,2)) #visualizzo i due plotRGB in un solo frame
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# multiframe with ggplot2 and gridExtra, con ggRGB non posso usare par quindi creo file per ogni immagine e poi uso grid.arrange
p1 <- ggRGB(defor1, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(defor2, r=1, g=2, b=3, stretch="lin")
grid.arrange(p1, p2, nrow=2)

# faccio l’unsupervised classification con due classi, una classe per la foresta e una classe per la zona agricola
d1c <- unsuperClass(defor1, nClasses=2)
plot(d1c$map)
# class 1: forest
# class 2: agriculture

#ripeto per operazioni per la seconda immagine
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map)
# class 1: agriculture
# class 2: forest

#per la seconda immagine rifaccio l’operazione con 3 classi
d2c3 <- unsuperClass(defor2, nClasses=3)
plot(d2c3$map)

# frequencies, cioè devo ottenere la frequenza delle classi nelle immagini
freq(d1c$map) #scrivo questo e sotto mi usciranno le frequenze nella prima immagine
#   value  count
# [1,]     1 306583
# [2,]     2  34709

s1 <- 306583 + 34709 #questo è il numero totale di pixel

#ora che ho tutti i dati calcolo le proporzioni per la prima immagine
prop1 <- freq(d1c$map) / s1
# prop forest: 0.8983012
# prop agriculture: 0.1016988

#ripeto le operazioni per la seconda immagine
s2 <- 342726
prop2 <- freq(d2c$map) / s2
# prop forest: 0.5206958
# prop agriculture: 0.4793042

# costruisco il data set
cover <- c("Forest","Agriculture")
percent_1992 <- c(89.83, 10.16) #questi dati li hai ottenuti con le operazioni precedenti
percent_2006 <- c(52.06, 47.93)

percentages <- data.frame(cover, percent_1992, percent_2006) # conla funzione data.frame costruisco il data set
percentages #lo scrivo per controllare il data set appena creato

# plotto le percentuali con ggplot
ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")

ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

#con grid.arrange unisco il due plot in un’unica immagine
p1 <- ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")
p2 <- ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

grid.arrange(p1, p2, nrow=1)

#------------------------------------------------

# 10. R code variability

# R_code_variability.r

#richiamo i pacchetti da utilizzare
library(raster)
library(RStoolbox)
# install.packages("RStoolbox")
library(ggplot2) # for ggplot plotting
library(gridExtra) # for plotting ggplots together
# install.packages("viridis")
library(viridis) # for ggplot colouring



# setwd("C:/lab/similaun") # preparo working directory


sent <- brick("sentinel.png") #carico l’immagine con tutte le bande

# NIR 1, RED 2, GREEN 3
# r=1, g=2, b=3
plotRGB(sent, stretch="lin") #plotto in RGB

plotRGB(sent, r=2, g=1, b=3, stretch="lin") 


nir <- sent$sentinel.1 #creo immagine con legata$ solo la banda dell’infrarosso
red <- sent$sentinel.2 #creo immagine con banda del rosso

ndvi <- (nir-red) / (nir+red) #calcolo l’indice NDVI
plot(ndvi)
cl <- colorRampPalette(c('black','white','red','magenta','green'))(100) 
plot(ndvi,col=cl) #plotto ndvi con i colori della palette cl

# uso una moving window 3x3 e calcolo la deviazione standard
ndvisd3 <- focal(ndvi, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # creo palette da utilizzare
plot(ndvisd3, col=clsd) #plotto la deviazione standard con palette clsd

# uso una moving window sempre 3x3 per calcolare la media
ndvimean3 <- focal(ndvi, w=matrix(1/9, nrow=3, ncol=3), fun=mean)
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
plot(ndvimean3, col=clsd)

# caloclo la deviazione standard con una griglia 13x13
ndvisd13 <- focal(ndvi, w=matrix(1/169, nrow=13, ncol=13), fun=sd)
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
plot(ndvisd13, col=clsd) #plotto il risultato

# uso una griglia 5x5 per calcolare deviazione standard
ndvisd5 <- focal(ndvi, w=matrix(1/25, nrow=5, ncol=5), fun=sd)
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
plot(ndvisd5, col=clsd) #plotto il risultato

# calcolo la PCA
sentpca <- rasterPCA(sent) 
plot(sentpca$map)  

summary(sentpca$model) #così vedo le informazioni

#la prima PC contiene il 67.36804 dell'informazione originale

pc1 <- sentpca$map$PC1 #raccolgo la prima componente PC1 e la mappa

#calcolo la deviazione standard con una griglia 5x5
pc1sd5 <- focal(pc1, w=matrix(1/25, nrow=5, ncol=5), fun=sd)
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
plot(pc1sd5, col=clsd)

# With the source function you can upload code from outside, posso caricare uno script
source("source_test_lezione.r")
source("source_ggplot.r") #visualizzo lo script 

# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# The package contains eight color scales: “viridis”, the primary choice, and five alternatives with similar properties - “magma”, “plasma”, “inferno”, “civids”, “mako”, and “rocket” -, and a rainbow color map - “turbo”.

p1 <- ggplot() + #carico una finestra vuota poi con + posso aggiungere quello che mi interessa
geom_raster(pc1sd5, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis() #questa è la colorRampPalette di viridis +
ggtitle("Standard deviation of PC1 by viridis colour scale")

#ora faccio la stessa cosa ma con ColorRampPalette magma
p2 <- ggplot() +
geom_raster(pc1sd5, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option = "magma")  +
ggtitle("Standard deviation of PC1 by magma colour scale")

#stessa operazione con ColorRampPalette turbo
p3 <- ggplot() +
geom_raster(pc1sd5, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option = "turbo")  +
ggtitle("Standard deviation of PC1 by turbo colour scale")

grid.arrange(p1, p2, p3, nrow = 1) #unisco i tre plot in un’unica immagine

#------------------------------------------------

# 11. R code spectral signatures

# R_code_spectral_signatures.r

library(raster)
library(rgdal)
library(ggplot2)

setwd("~/lab/deforestazione")


defor2 <- brick("defor2.jpg") #carico l’immagine con tutte le bande

# defor2.1, defor2.2, defor2.3 
# NIR, red, green

#plotto l’immagine con i due tipi di strecth
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="hist")

click(defor2, id=T, xy=T, cell=T, type="p", pch=16, cex=4, col="yellow")
#con la funzione click posso cliccare su un punto della mappa e ottenere le informazioni di quel punto

# results: questi saranno diversi in base al punto in cui clicchi
#      x     y  cell defor2.1 defor2.2 defor2.3
# 1 178.5 435.5 30293      206        6       19
#      x     y   cell defor2.1 defor2.2 defor2.3
# 1 571.5 245.5 166916       40       99      139

# define the columns of the dataset: cioè sto creando un data.frame con tre colonne
band <- c(1,2,3)
forest <- c(206,6,19)
water <- c(40,99,139)

# creo il dataframe
spectrals <- data.frame(band, forest, water)

# plotto con ggplot
ggplot(spectrals, aes(x=band)) +
 geom_line(aes(y=forest), color="green") +
 geom_line(aes(y=water), color="blue") +
 labs(x="band",y="reflectance")

############### Multitemporal, analisi multitemporale

defor1 <- brick("defor1.jpg")

plotRGB(defor1, r=1, g=2, b=3, stretch="lin")

# spectral signatures defor1, uso la funzione click
click(defor1, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

#     x     y  cell defor1.1 defor1.2 defor1.3 #questi valori sono diversi in base a dove clicchi nell’immagine
# 1 89.5 339.5 98622      223       11       33
#     x     y   cell defor1.1 defor1.2 defor1.3
# 1 42.5 336.5 100717      218       16       38
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 64.5 341.5 97169      213       36       46
#      x     y   cell defor1.1 defor1.2 defor1.3
# 1 80.5 326.5 107895      208        2       22
#     x     y  cell defor1.1 defor1.2 defor1.3
# 1 76.5 374.5 73619      224       21       41

# clicco la stessa zona nella seconda immagine
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
click(defor2, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

#      x     y  cell defor2.1 defor2.2 defor2.3
# 1 86.5 339.5 99033      197      163      151
#      x     y  cell defor2.1 defor2.2 defor2.3
# 1 104.5 338.5 99768      149      157      133
#      x     y  cell defor2.1 defor2.2 defor2.3
# 1 110.5 354.5 88302      197      132      128
#     x     y   cell defor2.1 defor2.2 defor2.3
# 1 90.5 320.5 112660      169      166      149
#    x     y   cell defor2.1 defor2.2 defor2.3
# 1 97.5 309.5 120554      150      137      129

# creo i file per i due intervalli temporali per creare il data.frame, i valori tra parentesi sono presi dai risultati della funzione click
band <- c(1,2,3)
time1 <- c(223,11,33)
time1p2 <- c(218,16,38)
time2 <- c(197,163,151)
time2p2 <- c(149,157,133)

spectralst <- data.frame(band, time1, time2, time1p2, time2p2) #creo data.frame


# plotto con gg plot le risposte spettrali
ggplot(spectralst, aes(x=band)) +
 geom_line(aes(y=time1), color="red", linetype="dotted") +
 geom_line(aes(y=time1p2), color="red", linetype="dotted") +
 geom_line(aes(y=time2), linetype="dotted") +
 geom_line(aes(y=time2p2), linetype="dotted") +
 labs(x="band",y="reflectance")

# ripeto le operazioni su una nuova immagine

eo <- brick("june_puzzler.jpg") #carico nuova immagine
plotRGB(eo, 1,2,3, stretch="hist")
click(eo, id=T, xy=T, cell=T, type="p", pch=16, cex=4, col="yellow")

# output
#     x     y  cell june_puzzler.1 june_puzzler.2 june_puzzler.3
# 1 93.5 373.5 76414            187            163             11
#      x     y   cell june_puzzler.1 june_puzzler.2 june_puzzler.3
# 1 219.5 285.5 139900             11            140              0
#     x     y   cell june_puzzler.1 june_puzzler.2 june_puzzler.3
# 1 184.5 315.5 118265             41             40             20


# define the columns of the dataset: sto creando i file delle colonne da inserire nel data frame
band <- c(1,2,3)
stratum1 <- c(187,163,11)
stratum2 <- c(11,140,0)
stratum3 <- c(41,40,20)

spectralsg <- data.frame(band, stratum1, stratum2, stratum3)

# uso ggplot per plottare le risposte spettrali
ggplot(spectralsg, aes(x=band)) +
 geom_line(aes(y=stratum1), color="yellow") +
 geom_line(aes(y=stratum2), color="green") +
 geom_line(aes(y=stratum3), color="blue") +
 labs(x="band",y="reflectance")

#------------------------------------------------
#R_code_exam.r
# installo i pacchetti
install.packages("raster")
install.packages("rgdal")
install.packages("sp")
install.packages("ggplot2")
install.packages("RStoolbox")

# indico la cartella che sarà la working directory del progetto
setwd("C:/foggia/")

# richiamo i pacchetti installati
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(RStoolbox)

# carico l'immagine del 2021
sentinel2_2021 <- raster('S2B_2021.tif')

# carico l'immagine del 2017
sentinel2_2017 <- raster('S2B_2017.tif')

# richiamo i file creati per controllare le informazioni principali
sentinel2_2021
sentinel2_2017

# apro una finestra con più pannelli dove inserire le immagini
par(mfrow=c(2,1))

# importo l'immagine del 2021 con tutte le sue bande e poi la plotto utilizzando plotRGB in truecolor
sentinel2_2021 <- brick("S2B_2021.tif")
plotRGB(sentinel2_2021, r = 3, g = 2, b = 1, stretch = "lin")

# importo l'immagine del 2017 con tutte le sue bande e poi la plotto utilizzando plotRGB in truecolor
sentinel2_2017 <- brick("S2B_2017.tif")
plotRGB(sentinel2_2017, r = 3, g = 2, b = 1, stretch = "lin")

# plotto e visualizzo tutte le bande dell'immagine 
plot(sentinel2_2021)

# plotto e visualizzo tutte le bande dell'immagine
plot(sentinel2_2017)

# creo due file per il 2021 a cui lego rispettivamente la banda dell'infrarosso (8) e la banda del rosso (4)
NIR2021 <- sentinel2_2021$S2B_2021.8
RED2021 <- sentinel2_2021$S2B_2021.4

# creo due file  per il 2017 a cui lego rispettivamente la banda dell'infrarosso (8) e la banda del rosso (4)
NIR2017 <- sentinel2_2017$S2B_2017.8
RED2017 <- sentinel2_2017$S2B_2017.4

# calcolo l'indice NDVI per l'immagine del 2021
ndvi2021 <- (NIR2021 - RED2021) / (NIR2021 + RED2021)

# calcolo l'indice NDVI per l'immagine del 2017
ndvi2017 <- (NIR2017 - RED2017) / (NIR2017 + RED2017)

# creo file png del plot dell'indice ndvi per l'anno 2021
png('ndviplot_2021.png', width = 4, height = 4, units = "in", res = 300)
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
plot(ndvi2021, col = cl, main = 'Sentinel 2, Foggia-NDVI2021')
dev.off()

# creo file png dell'istogramma della distribuzione ndvi2021
png('ndvi_2021_hist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi2021,
     main = "Distribution of NDVI 2021 values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "black",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()

# creo file png del plot dell'indice ndvi per l'anno 2017
png('ndviplot_2017.png', width = 4, height = 4, units = "in", res = 300)
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
plot(ndvi2017, col = cl, main = 'Sentinel 2, Foggia-NDVI2017')
dev.off()

# creo file png dell'istogramma della distribuzione ndvi2017
png('ndvi_2017_hist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi2017,
     main = "Distribution of NDVI 2017 values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "black",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()

# faccio la differenza tra gli ndvi dei diversi anni
difndvi <- ndvi2021 - ndvi2017

# creo file png della differnza di ndvi
png('ndvi_dif2020_2017.png', width = 4, height = 4, units = "in", res = 300)
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(difndvi, col=cld, main = 'differenza_ndvi_2020_2017')
dev.off()

# faccio una unsupervised classification con tre classi, una per la differenza positiva, una per la differenza neutra, una per la differenza negativa
uc <- unsuperClass(difndvi, nClas=3)

# plotto il risultato
plot(uc$map)

# controllo il numero di pixel dove ci sono i diversi cambiamenti ndvi

 freq(uc$map)
   #  value   count
#[1,]     1 5582494 
#[2,]     2  268377 (miglioramento)
#[3,]     3  298160 (peggioramento)

# creo file png dell'unsupervised classifcation
png('uc.png', width = 4, height = 4, units = "in", res = 300)
plot(uc$map, main = 'unsupervised_classification_3class')
dev.off()

# faccio la proporzione dei pixel in cui è avvenuto miglioramento e peggioramento rispetto al numero di pixel delle classi 2 e 3
s1 <- 268377 + 298160
prop1 <- freq(uc$map)/s1

#controllo i risultati
prop1
          #  value     count
#[1,] 1.765110e-06 9.8537148
#[2,] 3.530220e-06 0.4737149
#[3,] 5.295329e-06 0.5262851

# creo un data set con le percentuali di miglioramento e peggioramento
variazione <- c("Miglioramento","Peggioramento")
percent <- c(47.37,52.63)
percentages <- data.frame(variazione,percent)

# controllo i risulati
percentages

# plotto il risultato con ggplot così creo un istogramma
ggplot(percentages,aes(x=variazione,y=percent,color=variazione)) + geom_bar(stat="identity",fill="red")

# creo file png dell'istogramma
png('istomp.png', width = 4, height = 4, units = "in", res = 300)
ggplot(percentages,aes(x=variazione,y=percent,color=variazione)) + geom_bar(stat="identity",fill="red")
dev.off()

