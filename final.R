library(e1071)
library(nnet)
data0<-read.table("st.csv",sep=",", header = T)
colnames(data0)[6]<-'y'
data0
loop<-50 #隨機分割次數
pred.res_mat<-matrix(0,loop,4) #儲存預測力結果
loss.res_mat<-matrix(0,loop,4) #儲存解釋力結果
i <- 1
for (i in 1:loop)
{
  train<-sample(c(1:168),112)
  x_train<-data0[train,1:4]
  y_train<-data0$y[train]
  train_data<-data.frame(x=x_train,y=as.factor(y_train))
  x=x_train
  x
  tune.res<-tune(svm,y~.,data=train_data,kernel="linear",
                 ranges=list(cost=c(0.01,0.1,1,10,100)))
  svm.res<-svm(y~.,data=train_data,kernel="linear",
               cost=tune.res$best.parameter,scale=F)
  #利用testing data預測
  x_test<-data0[-train,1:4]
  names(x_test)<-names(train_data)[1:4]
  y_test<-data0$y[-train]
  y_test
  pred_test.res<-predict(svm.res,x_test) #哪里错了
  pred.res_mat[i,1]<-sum(y_test==pred_test.res) 
  names(x_train)<-names(train_data)[1:4]
  pred_train.res<-predict(svm.res)
  loss.res_mat[i,1]<-sum(y_train==pred_train.res) 
  data_train<-data0[train,]
  data_test<-data0[-train,]
  glm.res<-multinom(y~X+X.1,data=data_train)
  pred.logistic<-predict(glm.res, newdata = data_test, type = "c")
  pred.res_mat[i,2]<-sum(data_test$y==pred.logistic)
  loss.logistic<-predict(glm.res, newdata = data_train, type = "c")
  loss.res_mat[i,2]<-sum(data_train$y==loss.logistic)
}
apply(loss.res_mat,4,mean)
apply(pred.res_mat,4,mean)

