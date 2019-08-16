library(e1071)
library(caret)

porcentaje<-0.7

datos<-perrosCompleto
set.seed(1234)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

plot(train$Age,train$Breed1)
modeloLineal<-lm(train$Age~train$Breed1)
abline(modeloLineal)

datos$AdoptionSpeed <- as.factor(datos$AdoptionSpeed)

#debido a lo pesado que es se recurre al resultado
#modeloSVM<-svm(AdoptionSpeed~., data=train, cost=2^1, gamma=2^-1, type = 'C')
modeloSVM<-load("C:/Users/Ana Lucia Diaz Leppe/Documents/Hojadetrabajo8/Ana/modeloSVM.RData") 
as.factor(modeloSVM)
as.numeric(modeloSVM)
prediccion<-predict(modeloSVM,newdata=test)
confusionMatrix(test$AdoptionSpeed,prediccion)
#segundo dato

porcentaje<-0.7
datos<-perrosCompleto
set.seed(1234)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]


plot(train$Breed2,train$Breed1)
modeloLineal<-lm(train$Breed2~train$Breed1)
abline(modeloLineal)

datos$AdoptionSpeed <- as.factor(datos$AdoptionSpeed)
modeloSVM<-svm(AdoptionSpeed~., data=train, cost=2^1, gamma=2^-1, type = 'C')
prediccion<-predict(modeloSVM,newdata=test)

confusionMatrix(test$AdoptionSpeed,prediccion)

install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("corrplot")
library(rela)
library(psych)
library(FactoMineR)

library(corrplot)

datos<-read.csv("filasennumeros.csv",stringsAsFactors = F)

#Se debe analizar si se puede usar el análisis factorial 
#para formar las combinaciones lineales de las variables
pafDatos<-paf(as.matrix(datos[,1:38]))
pafDatos$KMO 
pafDatos$Bartlett 
summary(pafDatos)

#Pero hay que ver el nivel de significación de la prueba
cortest.bartlett(datos[,-1])


#se muestra la matriz de correlación
cor(datos[,-1],use = "pairwise.complete.obs")



#Esta función normaliza los datos de una vez
compPrinc<-prcomp(datos[,1:10], scale = T)
compPrinc


summary(compPrinc)

compPrincPCA<-PCA(datos[,-1],ncp=ncol(datos[,-1]), scale.unit = T)

summary(compPrincPCA)

#Se obtiene el scree plot de las componentes principales.

fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))

# En la siguiente gráfica se ilustra la calidad de la representación de los componentes en las dos primeras dimensiones.
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

pafdatos <- paf(datos[,1:18])
pafdatos$KMO #La adecuaci�n muestral no es buena

datosPCA <- PCA(datos[,1:18])
summary(datosPCA)


#Scree Plot
fviz_eig(datosPCA, addlabels = TRUE, ylim = c(0, 80))
#Representaci�n de las variables en cada componente
fviz_pca_var(datosPCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Representaci�n de cada variable en cada componente
var<-get_pca_var(datosPCA)
corrplot(var$cos2, is.corr = F)

