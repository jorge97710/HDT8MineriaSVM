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

