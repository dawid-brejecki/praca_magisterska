### wczytanie danych
dane=read.csv("bazadanych.csv", header=TRUE, sep=';')

### wczytanie bibliotek
library(MASS)
library(car)
library(ggcorrplot)
library(caret)
library(VIF)
library(class)
library(MASS)
library(klaR)
library(biotools)
library(SpatialEpi)
library(lmtest)
library(e1071)
### rzut oka na dane
scatterplotMatrix(dane)


### losowanie indeks闚 wierszy zbioru ucz鉍ego
zbior.uczacy = sample(1:nrow(dane), nrow(dane)/1.5, F)
zbior.uczacy


### naiwny bayes

model=naiveBayes(factor(kaliber)~odl+czas, data = dane, subset = zbior.uczacy)
oceny=predict(model, newdata = dane[-zbior.uczacy,2:3], type = c("class"))
ct=table(oceny, empiryczne = dane[-zbior.uczacy,1])
ct
diag=(prop.table(ct,1))
sum(diag(prop.table(ct)))
partimat(factor(dane$kaliber)~dane$czas+dane$odl, data = dane, subset=zbior.uczacy
         ,method = "naiveBayes", plot.matrix = FALSE, 
         imageplot=TRUE, image.colors=c('cyan', 'deepskyblue','blue','darkblue'),
         col.correct='black', col.wrong='red', main="Klasyfikacja przypadk闚",
         prec=300, name=c('czas trwania (ms)','odleg這 czujnik闚 (m)'), 
         print.err=0)


### metoda wektor轑 noych

svmfit=svm(factor(kaliber)~czas+odl, data = dane, subset = zbior.uczacy, kernel='linear',
           cost=10)
svmfit=svm(factor(kaliber)~czas+odl, data = dane, subset = zbior.uczacy, kernel='radial', 
           cost=10)
print(svmfit)
summary(svmfit)
oceny=predict(svmfit, newdata = dane[-zbior.uczacy,2:3], type = c("class"))
oceny
ct=table(empiryczne = dane[-zbior.uczacy,1], oceny)
ct
diag=(prop.table(ct,1))
sum(diag(prop.table(ct)))
plot(svmfit, dane, subset = -zbior.uczacy, col=c('darkorange', 'yellow','green','brown'))

### liniowa analiza dyskryminacyjna
klasyfikatorLDA = lda(dane[,2:3], grouping = dane[,1], subset=zbior.uczacy)
print(klasyfikatorLDA)
klasyfikatorLDA$scaling

install.packages('DiscriMiner')
library(DiscriMiner)
trein=cbind(dane$czas,dane$odl)
trein
da12=linDA(trein,c(dane$kaliber))
da12
### wyietlenie wykres闚 funkcji klasyfikuj鉍ych
plot(klasyfikatorLDA, col='blue', cex=1.1)
title ("Wartoi funkcji kanonicznych")
dane$kaliber=as.factor(dane$kaliber)
partimat(factor(dane$kaliber)~dane$czas+dane$odl, data = dane, subset=zbior.uczacy
         ,method = "naiveBayes", plot.matrix = FALSE, 
         imageplot=TRUE, image.colors=c('darkorange', 'yellow','green','brown'),
         col.correct='black', col.wrong='red', main=" ",
         prec=300, name=c('czas trwania (ms)','odleg這 czujnik闚 (m)'), 
         print.err=0)

### tabela klasyfikacyjna po u篡ciu zbioru testowego
oceny = predict(klasyfikatorLDA, newdata=dane[-zbior.uczacy,2:3])
ct=table(teoretyczne=oceny$class, empiryczne = dane[-zbior.uczacy,1])
ct

### wyietlenie procentu poprawnej klasyfikacji
diag=(prop.table(ct,1))
sum(diag(prop.table(ct)))

### badanie wsp馧liniowoi zmiennych
vif(dane$czas,dane$odl)

### wyietlenie rozk豉du zmiennych
hist(dane[,2])
hist(dane[,3])

### test rozk豉du normalnego
shapiro.test(dane[,1])
shapiro.test(dane[,2])
shapiro.test(dane[,3])

### obliczenie macierzy kowariancji
dane1=dane[1:194,2:3]
dane2=dane[195:204,2:3]
dane3=dane[205:236,2:3]
dane4=dane[237:278,2:3]

kowar1=cov(dane1)
kowar2=cov(dane2)
kowar3=cov(dane3)
kowar4=cov(dane4)

print(kowar1)
print(kowar2)
print(kowar3)
print(kowar4)

### test M-Boxa
res=boxM(dane[,2:3],dane[,1])
res

### wyietlenie macierzy korelacji
korelacja1=round(cor(dane1),1)
ggcorrplot(korelacja1,lab = TRUE, title = "7,62 mm",col=c('red','darkblue','cyan'))
korelacja2=round(cor(dane2),1)
ggcorrplot(korelacja2,lab = TRUE, title = "5,56 mm", col=c('red','darkblue','cyan'))
korelacja3=round(cor(dane3),1)
ggcorrplot(korelacja3,lab = TRUE, title = "23 mm",col=c('red','darkblue','cyan'))
korelacja4=round(cor(dane4),1)
ggcorrplot(korelacja4,lab = TRUE, title = "35 mm",col=c('red','darkblue','cyan'))

### kwadratowa analiza dyskryminacyjna
klasyfikatorQDA = qda(dane[,2:3], grouping = dane[,1], subset=zbior.uczacy)
print(klasyfikatorQDA)
oceny = predict(klasyfikatorQDA, newdata=dane[-zbior.uczacy,2:3])
ct=table(teoretyczne=oceny$class, empiryczne = dane[-zbior.uczacy,1])
ct
diag=(prop.table(ct,1))
sum(diag(prop.table(ct)))
dane$kaliber=as.factor(dane$kaliber)
partimat(factor(dane$kaliber)~dane$czas+dane$odl, data = dane, subset=zbior.uczacy
         ,method = "qda", plot.matrix = FALSE, 
         imageplot=TRUE, image.colors=c('cyan', 'deepskyblue','blue','darkblue'),
         col.correct='black', col.wrong='red', main="Klasyfikacja przypadk闚",
         prec=300, name=c('czas trwania (ms)','odleg這 czujnik闚 (m)'), 
         print.err=0)

### regresja logistyczna
dane762=read.csv("C:\\Users\\user\\Desktop\\baza762.csv", header=TRUE, sep=';')
glm.fit=glm(kaliber~odl+czas, data=dane762, subset = zbior.uczacy, family = binomial)
summary(glm.fit)
### test istotnoi modelu
lrtest(glm.fit)
### walidacja na zbiorze testowym
glm.probs=predict(glm.fit, newdata = dane762[-zbior.uczacy,], type = "response")
Teoretyczne=ifelse(glm.probs>0.5, "1","0")
Empiryczne=dane762$kaliber[-zbior.uczacy]
table(Teoretyczne, Empiryczne)
### procent poprawnej klasyfikacji
mean(Teoretyczne == Empiryczne)

### KNN
train.X=cbind(dane$odl, dane$czas)[zbior.uczacy,]
test.X=cbind(dane$odl, dane$czas)[-zbior.uczacy,]
train.kaliber=dane$kaliber[zbior.uczacy]
EmpiryczneKNN=c(dane$kaliber[-zbior.uczacy])
set.seed(1)
TeoretyczneKNN=knn(train.X, test.X, train.kaliber, k=3)
table(TeoretyczneKNN,EmpiryczneKNN)
mean(TeoretyczneKNN==EmpiryczneKNN)
contour(train.X, test.X, train.kaliber, levels=0.5, labels="", xlab="", ylab="", main= "15-nearest neighbour", axes=FALSE)
