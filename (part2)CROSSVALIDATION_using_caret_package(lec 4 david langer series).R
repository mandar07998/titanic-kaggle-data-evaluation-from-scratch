#create a randoforest out out test data set and check if our model is prone to overfitting on new data (i.e test data)
#you can even submit it on to kaggle so that they can verify using their own cross validation/test data set 

#Subset on test records using features we selected("Pclass,title,family.size")and check the error rate 
#if error rate is more =then our model suits well only for training dataset and not the new testing data.
test.submit.df<-data.combined[892:1309,c("Pclass","title","family.size")]
#this will give an error coz there is no "survived"data available in our test set.So we will have to submit it to kaggle to check our nhypothesis accuracy 
rf.5.test<-randomForest(x=rf.train.5,y=rf.label[892:1309],importance=TRUE,ntree = 1000)
varImpPlot(rf.5.test)#So ignore this 2 lines of code as we will get error




#Now we make predictions using test.submit.df as input and rf.5 as checking our testing
rf.5.preds<-predict(rf.5,test.submit.df)

#Nowtranform our predictions into csv file using R
#create a data.frame using 2 variables passengerid and rf.5.preds
submit.df<-data.frame(PassengerId=rep(892:1309),Survived=rf.5.preds)

#Now convert this data frame into a .csv file
write.csv(submit.df,file="RF_SUB_20200419_1.csv",row.names=FALSE)


#AFter submitting this to kaggel my accuracy score on test data set was 79.24%.this test data can be similiar to new data which is used for checking my model accuracy.
#But our model accuracy score on training data set was 81%(18.18%error).This gives us a difference of 2%
#The only way to minimize this error of 2% is to introduce to cross validation dataset using packages like caret 

install.packages("caret")
install.packages("doSNOW")


##10 fold cross validation using R package named "Caret"
##what is 10 fold cross-validation?
##ans:
##step1:Split the data into 10 logical chunks 10 parts of one massive data sets(like sub data)
##step 2:Out of 9 ,use 1 as test data set .Train model using the remaining 9 data sets and run it .Compare the predicted result with the test data 
##step3:Now use different chunk as the test set and store the result
##step4:Repeat step 2 again 8 times 
##step 5:noW do not chunk or divide or data(use entire data set as one)and train it .Test ur findings o0n a completely unseen data set(cross validation) and check the accuracy



set.seed(2348)
cv.10.folds<-createMultiFolds(rf.label,k=10,times = 10)

table(rf.label)
342/549=0.6229
##for reference only do not use this as a code
table(rf.label[cv.10.folds[[36]]])
308/494

##similary do this with any fold and we will egt the same answer i.e 62 percent
table(rf.label[cv.10.folds[[33]]])
307/494
##

##Now that I made a output set(y=rf.label and used mutifolds on it) ,now lets do the same for trainig data(mutifold it and then train)
##for that we use the the below:
##set up train control object as per above
ctrl.1<-trainControl(method="repeatedcv",number=10,repeats = 10,index=cv.10.folds)


#we can now setup muticore trainig with doSNOW package(By default r uses single core processor to do default training)
#doing this is helpful as we are going to use a lot of trees for the training

cl<-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)


set.seed(34324)
rf.5.cv.1<-train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,ntree=1000,trControl=ctrl.1)

stopCluster(cl)

#now by the result of rf.5.cv.1 accuracy score(mtry=2 is 81.14)this is somewhat less than what actual rf.5 OBB error rate was(18.18 ,so accuracy was 81.82)so cv error is more than trainig set error.
##This is true coz in cv i ttrained with 90% of data which resulted in overfitting of trainig set and caused error after comparing with rf.label.which was used in 1st part (trainig set)as well as cross validation 
#THis is still optimistic as accuracy of cv(81.14 )is more than accuracy of test.submit.df submitted to kaggle(79.426).this is 2 points higher 

#to over come the error of 81.82-81.11 we train a the same cross validation with less fold i.e half(5 folds)
set.seed(5983)
cv.5.folds<-createMultiFolds(rf.label,k=5,times = 10)
ctrl.2<-trainControl(method = "repeatedcv",number = 5,repeats=10,index = cv.5.folds)
cl2<-makeCluster(6,type = "SOCK")
registerDoSNOW(cl2)
set.seed(89472)
rf.5.cv.2<-train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,ntree=1000,trControl=ctrl.2)
#after getting the result for rf.5.cv.2 we get the accuracy score of 81.23434 whixh has improved a bit (from 81.11 to 81.2343)




# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)
cl3 <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl3)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3


#question :why i suppose 3 fold validation worked better than 10 fold or 5 fold 
#answer:dividing a training data by 3 parts(2/3rd for trainig and 1/3rd for testing)where model is formed using 2/3rd training dataset and tested on 1/3rd testing set.This is exactly the same as the "ORIGINAL TRAIN AND TEST SET where 2/3 was the training set and 1/3 was testing set"
#possible doubt:Then why do rf.5.cv.3 gives somewhat similiar accuracy as to the daat trained on original "train" and tested on oroginal "test" of "data.combined"
#answer:(1)the structure of data.combined orfinal is 2/3 train and 1/3 test
#       (2)the structure of rf.5.cv.3 is also 2/3 of trainind data as train set and 1/3rd left out as test set
#so the structure matches and this is possible reason for high accuracy rate of rf.5.cv.3 and cv.2 and cv.1 






#conclusions:(1)use rf.5.cv.3  for further exploratory analysis and do not use rf.5 ,   rf.5.cv.1,   rf.5.cv.2
#            (2)acuuracy score of rf.5.cv.1=81.14
#            (3)accuracy score of rf.5.cv.2=81.23
#             (4)accuracy score of rf.5.cv.3=81.39




#as we are doing exploratory analysis further our modelling will be furthen on cv3 fold 3 repeated 10 times other than using rf.5 alone
#the problem with rf.5 was it might have overfitted the data and did not yeild good accuracy on new kaggle set(79.42)
#this errro is resolved by cross validation set with an accuracy of 81.39 which is >79.42 .,So continue with rf.5.cv.3 further
