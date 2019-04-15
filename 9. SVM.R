#Support Vector Machine

library(e1071)
library(caret)
porcentaje<-0.7
datos<-iris
set.seed(123)


corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

plot(train$Sepal.Length,train$Petal.Length)
modeloLineal<-lm(train$Sepal.Length~train$Petal.Length)
abline(modeloLineal)

modeloSVM<-svm(Species~., data=train, cost=2^5, gamma=2^-5)
prediccion<-predict(modeloSVM,newdata=test)

confusionMatrix(test$Species,prediccion)


