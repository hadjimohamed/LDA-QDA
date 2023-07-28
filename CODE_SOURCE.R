#la description du tableau de données 
dim(heart)
colnames(heart)
attributes(heart)
summary(heart)

#faire quelques plots
plot(heart$age,heart$chol,xlab = "Age",ylab = "Chol",col="purple",cex=1.2,main ="Varation du cholesterol par rapport a l'age")
hist(heart$trtbps[1:200],xlab="Tot de trtbps",ylab ="Frenquence",col="pink",main = "Histogramme du tot de trtbps")
plot(density(heart$thalachh),main = "Density du thalachh",col="blue") 
x=nrow(heart[heart$age>=29 & heart$age<=35,])
y=nrow(heart[heart$age>=36 & heart$age<=55,])
z=nrow(heart[heart$age>=56 & heart$age<=65,])
e=nrow(heart[heart$age>=66 & heart$age<=70,])
slices=c(x,y,z,e)
lbls <- c("Jeunes", "Adultes", "Aines", "Vieux")
pie3D(slices, labels = lbls, main="Pie des tranches d'ages")

#Dévision du tableau de données en apprentissage/test:
#1 apprentissage :
tr <- sample(nrow(heart), nrow(heart)*0.8)
tr=sort(tr)
tr
train <-heart[tr,]
train

#2 test :
test <-heart[-tr,]
test

#Application de l'analyse discriminante sur l'échantillon d'apprentissage : 
#1.1 appliquer LDA sur le train  : 
res_train_lda=lda(output~age+sex+cp+trtbps+chol+fbs+restecg+thalachh+exng+oldpeak+slp+caa+thall,data=train)
res_train_lda

#1.1.2 creer la variables discriminante pour LDA : 
res_train_lda$scaling
u_lda=res_train_lda$scaling
u_lda=as.matrix(u_lda)
g=colMeans(train[-14])
g=as.matrix(g)
x=as.matrix(train[-14])
t_lda=x%*%u_lda
t2_lda=t(u_lda)%*%g
v1_lda=rep(t2_lda,dim(train)[1])
v1_lda=as.matrix(v1_lda)
s_lda=t_lda-v1_lda
s_lda
couleur=c(rep('red',135),rep('blue',107))
plot(s_lda,col=couleur)

#1.2 appliquer QDA sur le train  : 
res_train_qda=qda(output~age+sex+cp+trtbps+chol+fbs+restecg+thalachh+exng+oldpeak+slp+caa+thall,data=train)
res_train_qda

#Prédiction des classes pour l'échantillon test : 

#1.1 Prediciton pour LDA: 
res_test_lda=predict(res_train_lda,newdata=test)
res_test_lda

#1.2 Prediciton pour QDA
res_test_qda=predict(res_train_qda,newdata=test)
res_test_qda

#Evaluation des résultats obtenus sur l'échantillon test : 

#1 pour LDA :
#1.1 matrice de confusion : 
matrice_confusion_test_lda=table(test[,14],res_test_lda$class)
matrice_confusion_test_lda
#1.2 accuracy :
acc_test_lda=sum(diag(matrice_confusion_test_lda))/sum(matrice_confusion_test_lda)
acc_test_lda
#1.3 le pourcentage d'erreur
error_test_lda=1-acc_test_lda
error_test_lda

#2 pour QDA :
#2.1 matrice de confusion : 
matrice_confusion_test_qda=table(test[,14],res_test_qda$class)
matrice_confusion_test_qda
#1.2 accuracy :
acc_test_qda=sum(diag(matrice_confusion_test_qda))/sum(matrice_confusion_test_qda)
acc_test_qda
#2.3 le pourcentage d'erreur
error_test_qda=1-acc_test_qda
error_test_qda


