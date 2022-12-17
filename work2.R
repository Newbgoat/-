ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
ucla$admit <- factor(ucla$admit)


n <- nrow(ucla)
i <- 1:n

train_n <- sample(i,round(0.6*n))
test_n<- setdiff(i,train_n)
train <- ucla[train_n,]
test <- ucla[test_n,]






library(caret)

library(rpart)
rp<- rpart(admit~.,data = train)


rp_p<- predict(rp,test,type='class')
rp_t <- table(rp_p,test$admit)
rp_t




library(randomForest)
rf50 <- randomForest(admit~.,data=train,ntree=50)

rf50_p <- predict(rf50,test,type='class')
rf50_t <- table(rf50_p,test$admit)
rf50_t


rf100 <- randomForest(admit~.,data=train,ntree=100)

rf100_p <- predict(rf100,test,type='class')
rf100_t <- table(rf100_p,test$admit)
rf100_t


library(e1071)
svrb <- svm(admit~.,data = train)

svrb_p <- predict(svrb,test)
svrb_t <- table(svrb_p,test$admit)
svrb_t


svpm <- svm(admit~.,data=train,kernel='polynomial')


svpm_p <- predict(svpm,test)
svpm_t <- table(svpm_p,test$admit)
svpm_t


library(class)

k <- knn(train[,2:4],test[,2:4],train$admit,5)
k_t <- table(k,test$admit)
k_t


acc <- data.frame(name = c("tree","forest_50","forest_100","SVM-radia","svm-poly","knn"),acc=c(0))
tables <- list(rp_t,rf50_t,rf100_t,svrb_t,svpm_t,k_t)


for(i in 1:length(tables)){
  tb <- tables[[i]]
  acc$acc[i] <- ((tb[1,1]+tb[2,2])/sum(tb) *100)
}





 