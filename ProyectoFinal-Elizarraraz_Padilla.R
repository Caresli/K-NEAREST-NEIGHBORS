#Código proyecto final
#Estefany Elizarraraz Cardoso
#Alondra Padilla Ramírez 

iris
library(caret)

#n= 150 =número de dator
#k= 20 =pruebas = elemento con lo que se compara su distancia con los otros
#p= 130 = todos los demás elementos =train


#Dividir los datos entre los registros que se van a clasificar y los que se usarán para la clasificación
set.seed(20)
test<-sample(1:150,20,F) ## crear una variable llamada test, utilizando la función sample, nos devuelve 20 números aleatorios
test
m_test<-iris[test,]  ##para la muestra referente a test, se crea un data frame rescatando de Iris aquellos datos en el número de fila, según los indices que nos devolvio test
m_test
m_train<-iris[-test,]  ##para la muestra train, se crea un data frame en el cual se eliminan aquellos datos que se referenciaron en m_test.
m_train



#se crea un data frame de veinte columnas, donde se agregara la distancia entre los datos de test y los 130 de train (130 columnas)
Eu_D<-data.frame(test_1=rep(x=NA,times=130), test_2=rep(x=NA,times=130),test_3=rep(x=NA,times=130),tes_4=rep(x=NA,times=130),test_5=rep(x=NA,times=130),test_6=rep(x=NA,times=130),test_7=rep(x=NA,times=130),test_8=rep(x=NA,times=130),test_9=rep(x=NA,times=130),test_10=rep(x=NA,times=130),test_11=rep(x=NA,times=130),test_12=rep(x=NA,times=130),test_13=rep(x=NA,times=130),test_14=rep(x=NA,times=130),test_15=rep(x=NA,times=130),test_16=rep(x=NA,times=130),test_17=rep(x=NA,times=130),test_18=rep(x=NA,times=130),test_19=rep(x=NA,times=130),test_20=rep(x=NA,times=130))
Eu_D

#se crea un for en el que i (representando los numeros de test) y k (representando los indices de train), van iterando en la formula para calcular la distancia
for (i in 1:20) {
  for (k in 1:130) {
    euDist<-sqrt(sum((m_test[i,1:4]-m_train[k,1:4])^2))
    Eu_D[k,i]<-euDist   #las distancias irán llenando los espacios en el data frame creado
  }
}
Eu_D
#guardar el data frame en otro para conservar el orden original de las distancias#
Eu_D1<- data.frame(test_1=Eu_D[,1], test_2=Eu_D[,2],test_3=Eu_D[,3],tes_4=Eu_D[,4],test_5=Eu_D[,5],test_6=Eu_D[,6],test_7=Eu_D[,7],test_8=Eu_D[,8],test_9=Eu_D[,9],test_10=Eu_D[,10],test_11=Eu_D[,11],test_12=Eu_D[,12],test_13=Eu_D[,13],test_14=Eu_D[,14],test_15=Eu_D[,15],test_16=Eu_D[,16],test_17=Eu_D[,17],test_18=Eu_D[,18],test_19=Eu_D[,19],test_20=Eu_D[,20])
Eu_D1


#se ordenan los datos dentro de las columnas de menor  a mayor
for (i in 1:20) {
  Eu_D[,i]<-sort(Eu_D[,i],decreasing = F)
  Eu_D
}
Eu_D

#se dejan solo las 21 distancias más cortas para cada elemento de test
if(nrow(Eu_D)>=21){
  Eu_D<-Eu_D[- c(22,23:130),]
}
Eu_D


##Para recuperar los indices de las distancias 
#se crea un data frame para que se guarden los indices
Ind<-data.frame(test_1=rep(x=NA,times=20), test_2=rep(x=NA,times=20),test_3=rep(x=NA,times=20),tes_4=rep(x=NA,times=20),test_5=rep(x=NA,times=20),test_6=rep(x=NA,times=20),test_7=rep(x=NA,times=20),test_8=rep(x=NA,times=20),test_9=rep(x=NA,times=20),test_10=rep(x=NA,times=20),test_11=rep(x=NA,times=20),test_12=rep(x=NA,times=20),test_13=rep(x=NA,times=20),test_14=rep(x=NA,times=20),test_15=rep(x=NA,times=20),test_16=rep(x=NA,times=20),test_17=rep(x=NA,times=20),test_18=rep(x=NA,times=20),test_19=rep(x=NA,times=20),test_20=rep(x=NA,times=20))
Ind
#####agregar filas extras, para desempates, se agregan hasta 21 porque es el desempate del numero de k más alto#
for(i in 1:20){   #i indica las columnas
  for(k in 1:21){  #k indica las filas de Eu_D
    ind_k<-which(Eu_D[k,i]==Eu_D1[,i])
    if(length(x=ind_k)>=2){     #se pone la condición que si ind_k nos da más de un valor, sólo se escogera el primero, esto no afecta al codigo porque son de la misma especie
      ind_k<-ind_k[1]
    }else{
      ind_k
    }
    Ind[k,i]<-ind_k
  }
}
Ind


#Para clasificar en que clase están nuestros valores de test desde K=20 hasta k=1...
  #Crear una función que busque la especie de cada observación de acuerdo a sus vecinos
w_class<- function(I,K){ #siendo I las observaciones de test y K el número  de vecinos en comparar la distancia para asignar la especie
  t<-m_train[Ind[1:K,I],5]  
  a<-length(which(t=="virginica"))
  b<-length(which(t=="setosa"))
  c<-length(which(t=="versicolor"))
  if(a==b | a==c |c==b){ #se agrega en los números pares para el desempate#
    t<-m_train[Ind[1:K+1,I],5]
  }else{
    t
  }
  if(a>b & a>c){
    test<-"virginica"
  }else{
    if(b>a & b>c){
      test<-"setosa"
    }else{
      test<-"versicolor"
    }
  }
  test
}

#k=20
predictions_K20 <- c()
for(i in 1:20){
  predictions_K20[i] <- w_class(I=i,K=20)
}
predictions_K20
  #matriz de confusión
k_20<-confusionMatrix(m_test[,5],factor(predictions_K20))
k_20
Accuracy_k20 <- k_20[[3]][1]
Accuracy_k20

#k=19
predictions_K19 <- c()
for(i in 1:20){
  predictions_K19[i] <- w_class(I=i,K=19)
}
predictions_K19
  #matriz de confusión
k_19<-confusionMatrix(m_test[,5],factor(predictions_K19))
k_19
Accuracy_k19 <- k_19[[3]][1]
Accuracy_k19

#k=18
predictions_K18 <- c()
for(i in 1:20){
  predictions_K18[i] <- w_class(I=i,K=18)
}
predictions_K18
  #matriz de confusión
k_18<-confusionMatrix(m_test[,5],factor(predictions_K18))
k_18
Accuracy_k18 <- k_18[[3]][1]
Accuracy_k18


#k=17
predictions_K17 <- c()
for(i in 1:20){
  predictions_K17[i] <- w_class(I=i,K=17)
}
predictions_K17
  #matriz de confusión
k_17<-confusionMatrix(m_test[,5],factor(predictions_K17))
k_17
Accuracy_k17 <- k_17[[3]][1]
Accuracy_k17


#k=16
predictions_K16 <- c()
for(i in 1:20){
  predictions_K16[i] <- w_class(I=i,K=16)
}
predictions_K16
  #matriz de confusión
k_16<-confusionMatrix(m_test[,5],factor(predictions_K16))
k_16
Accuracy_k16 <- k_16[[3]][1]
Accuracy_k16


#k=15
predictions_K15 <- c()
for(i in 1:20){
  predictions_K15[i] <- w_class(I=i,K=15)
}
predictions_K15
  #matriz de confusión
k_15<-confusionMatrix(m_test[,5],factor(predictions_K15))
k_15
Accuracy_k15 <- k_15[[3]][1]
Accuracy_k15
  

#k=14
predictions_K14 <- c()
for(i in 1:20){
  predictions_K14[i] <- w_class(I=i,K=14)
}
predictions_K14
#matriz de confusión#
k_14<-confusionMatrix(m_test[,5],factor(predictions_K14))
k_14
Accuracy_k14 <- k_14[[3]][1]
Accuracy_k14


#k=13
predictions_K13 <- c()
for(i in 1:20){
  predictions_K13[i] <- w_class(I=i,K=13)
}
predictions_K13
  #matriz de confusión
k_13<-confusionMatrix(m_test[,5],factor(predictions_K13))
k_13
Accuracy_k13 <- k_13[[3]][1]
Accuracy_k13


#k=12
predictions_K12 <- c()
for(i in 1:20){
  predictions_K12[i] <- w_class(I=i,K=12)
}
predictions_K12
  #matriz de confusión
k_12<-confusionMatrix(m_test[,5],factor(predictions_K12))
k_12
Accuracy_k12 <- k_12[[3]][1]
Accuracy_k12


#k=11
predictions_K11 <- c()
for(i in 1:20){
  predictions_K11[i] <- w_class(I=i,K=11)
}
predictions_K11
  #matriz de confusión
k_11<-confusionMatrix(m_test[,5],factor(predictions_K11))
k_11
Accuracy_k11 <- k_11[[3]][1]
Accuracy_k11

#k=10
predictions_K10 <- c()
for(i in 1:20){
  predictions_K10[i] <- w_class(I=i,K=10)
}
predictions_K10
  #matriz de confusión
k_10<-confusionMatrix(m_test[,5],factor(predictions_K10))
k_10
Accuracy_k10 <- k_10[[3]][1]
Accuracy_k10


#k=9
predictions_K9 <- c()
for(i in 1:20){
  predictions_K9[i] <- w_class(I=i,K=9)
}
predictions_K9
  #matriz de confusión
k_9<-confusionMatrix(m_test[,5],factor(predictions_K9))
k_9
Accuracy_k9 <- k_9[[3]][1]
Accuracy_k9


##k=8
predictions_K8 <- c()
for(i in 1:20){
  predictions_K8[i] <- w_class(I=i,K=8)
}
predictions_K8
  #matriz de confusión
k_8<-confusionMatrix(m_test[,5],factor(predictions_K8))
k_8
Accuracy_k8 <- k_8[[3]][1]
Accuracy_k8


#k=7
predictions_K7 <- c()
for(i in 1:20){
  predictions_K7[i] <- w_class(I=i,K=7)
}
predictions_K7
  #matriz de confusión
k_7<-confusionMatrix(m_test[,5],factor(predictions_K7))
k_7
Accuracy_k7 <- k_7[[3]][1]
Accuracy_k7


#k=6
predictions_K6 <- c()
for(i in 1:20){
  predictions_K6[i] <- w_class(I=i,K=6)
}
predictions_K6
  #matriz de confusión
k_6<-confusionMatrix(m_test[,5],factor(predictions_K6))
k_6
Accuracy_k6 <- k_6[[3]][1]
Accuracy_k6

##k=5
predictions_K5 <- c()
for(i in 1:20){
  predictions_K5[i] <- w_class(I=i,K=5)
}
predictions_K5
  #matriz de confusión#
k_5<-confusionMatrix(m_test[,5],factor(predictions_K5))
k_5
Accuracy_k5 <- k_5[[3]][1]
Accuracy_k5


#k=4
predictions_K4 <- c()
for(i in 1:20){
  predictions_K4[i] <- w_class(I=i,K=4)
}
predictions_K4
  #matriz de confusión#
k_4<-confusionMatrix(m_test[,5],factor(predictions_K4))
k_4
Accuracy_k4 <- k_4[[3]][1]
Accuracy_k4


#k=3
predictions_K3 <- c()
for(i in 1:20){
  predictions_K3[i] <- w_class(I=i,K=3)
}
predictions_K3
  #matriz de confusión
k_3<-confusionMatrix(m_test[,5],factor(predictions_K3))
k_3
Accuracy_k3 <- k_3[[3]][1]
Accuracy_k3


#k=2
predictions_K2 <- c()
for(i in 1:20){
  predictions_K2[i] <- w_class(I=i,K=2)
}
predictions_K2
  #matriz de confusión
k_2<-confusionMatrix(m_test[,5],factor(predictions_K2))
k_2
Accuracy_k2 <- k_2[[3]][1]
Accuracy_k2

#k=1
predictions_K1 <- c()
for(i in 1:20){
  predictions_K1[i] <- w_class(I=i,K=1)
}
predictions_K1
  #matriz de confusión
k_1<-confusionMatrix(m_test[,5],factor(predictions_K1))
k_1
Accuracy_k1 <- k_1[[3]][1]
Accuracy_k1





#####Realizar la grafica para cada K con su accuracy####
X <- c(1:20) #número de k en el eje x#
X
Y <- c(Accuracy_k1,Accuracy_k2,Accuracy_k3,Accuracy_k4,Accuracy_k5,Accuracy_k6,Accuracy_k7,Accuracy_k8,Accuracy_k9,Accuracy_k10,Accuracy_k11,Accuracy_k12,Accuracy_k13,Accuracy_k14,Accuracy_k15,Accuracy_k16,Accuracy_k17,Accuracy_k18,Accuracy_k19,Accuracy_k20)
Y

plot(X,Y,main= "KxAccuracy",xlab="k vecinos",ylab="Accuracy",col="aquamarine4",pch=19)

