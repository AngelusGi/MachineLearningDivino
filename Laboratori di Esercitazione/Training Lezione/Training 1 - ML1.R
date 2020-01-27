
#Simulazione di modello di Regressione lineare
# MODELLO : Y=(B0+B1X)+E
# n : dimensione training set
# m : dimensione test set
# X : reddito ( tra 1000 e 2000 )
# Y : consumi
# A :

B0<-250 #Costi fissi
B1<-0.7 #70% del reddito va in consumi

n<-80 #Su 100 dati 80 verranno usati per il training
m<-20 #Il resto verrano usati per il test
set.seed(1) #Fissiamo un seed comune per la generazione della stessa randomizzazione
X<-rnorm(100, 1500, 100) #
#Y<-B0+B1*x
E<-rnorm(100,0,50)
Y<-B0+B1*X+E #Modello

A<-matrix(c(X,Y),100,2, byrow = F) #Immetto i dati pseudo-casuali in una matrice
A<-as.data.frame(A) #ne faccio un data set
colnames(A)<-c("reddito","consumi") #rinomino le colonne
#plot(X,Y)
A.training<-A[1:80,] #definisco le prime 80 come training
            #la virgola non � sbagliata, per sintassi sto definendo le righe 
            #che ho scelto e non avendo messo parametri dopo la virgola 
            #indico che sto prendendo tutte le colonne
A.test<-A[81:100,] #le restanti come tes

output.lm<-lm(consumi~reddito, data = A.training) #metto in output il risultato del linearmodel(lm)
summary(output.lm) #resoconto output

plot(A.training) 
abline(output.lm) #aggiungo al plot del grafico la linea lm che rappresenta la mia
                  #regressione lineare

names(output.lm)

is.list(output.lm) #chiedo se output.lm � una lista

output.lm$coefficients #stampa dei coefficienti

res<-output.lm$residuals #metto i residui dentro res

plot(res) #plot dei residui
abline(h=0) 
hist(res) #istogramma
#per valutare se un training � buono, si pu� fare l'analisi dei residui
#pi� i residui sono simmetrici pi� il modello � buono


#------------------------Parte di test----------------------#

A.test
plot(A.test)
abline(output.lm)

#mi calcolo i valori predetti dalla retta
#utilizzo output.lm per dargli il modello calcolato con il training
#ed i nuovi dati su cui testare la predizzione

Y<-predict.lm(output.lm, A.test) #valori predetti

Z<-A.test$consumi #valori osservati dei consumi

X<-A.test$reddito

plot(X,Y)
abline(output.lm)

sum((Z-Y)^2) #Valore utile a stabilire se una procedura � migliore di un'altra

plot(Z,Y)
abline(coef = c(0,1))

#---------------Capitolo 3 del libro--------------------#
#Andare nei package e spuntare la libreria MASS

data() #visualizzo tutti i dataset disponibili
attach(Boston) #scelgo il dataset Boston
#force("Boston") #se non lo ha caricatoBos
#scrivere solo Boston per controllare se viusalizza tutti i dati

#lm(formula = medv~. , data =Boston)



output.lm<-lm(formula = medv ~ . , data = Boston )
summary(output.lm)


#----------------Esercizio--------------------------------#0

#Dataset Boston
#Primi 400 records come training
#106 records per il test
#utilizzando al massimo 5 variabili, trovare il miglior modello
#per la scelta delle variabili, prestare attenzion alle ultime due colonne
#prendo il

#togliere i valori con  il Pr(>|t|) pi� altro e senza stelline accanto

output.lm<-lm(formula = medv ~ . -age-indus-chas-zn, data = Boston)
summary(output.lm)

#funzione di correlazione per sapere se due variabili sono dipendenti tra loro
cor(Boston)
#genera una matrice la quale mostra la correlazone tra le variabili della mtrice 
