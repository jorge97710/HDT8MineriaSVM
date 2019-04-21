library(e1071)

library(caret)

porcentaje<-0.7

datos<-perrosCompleto
set.seed(123)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

plot(train$Age,train$Breed1)
modeloLineal<-lm(train$Age~train$Breed1)
abline(modeloLineal)


modeloSVM<-svm(RescuerID~., data=train, cost=2^5, gamma=2^-5)
prediccion<-predict(modeloSVM,newdata=test)


confusionMatrix(test$RescuerID,prediccion)


