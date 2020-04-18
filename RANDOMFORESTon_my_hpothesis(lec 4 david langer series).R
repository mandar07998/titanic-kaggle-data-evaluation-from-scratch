#install and load the basic random forest 
install.packages("randomForest")
library(randomForest)

    #train a random forest using Pclass and title variables as they are very important and pivotalThey are predictive in building the visual model of the data.
    #lets try building a model with these 2 things and check the result of our model.Grab first 891 rows as they are the ones with labels
rf.train.1<-data.combined[1:891,c("Pclass","title")]


      #create a label(survived or not) data frame from the traing data set
rf.label<-as.factor(train$Survived)
levels(rf.label)

#Random forest are random(not surprisingly).That means they will produce somewhat different outputs each time I run it.So if we want the reproducibility i.e evaluating each time we run the algorithm we need to set a pateern that is set.seed(1234)
#"random sampling with replacement"it means<-n=100 times I am going to select rows and columsn(matrix)from the spreadsheet and I am OK if I select any rows or columns twice.This also means certain number or rows and columns will never get selected and would be OUTSIDE OF BAG(oob).THE stuff that i didnt select can be used to test the trained tree(this is called cross validation set)
rf.1<-randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
varImpPlot(rf.1)

#Now lets just plt another random forest with oen more variable("SibSp")added
rf.train.2<-data.combined[1:891,c("Pclass","title","SibSp")]
rf.2<-randomForest(x=rf.train.2,y=rf.label,importance=TRUE,ntree=1000)
varImpPlot(rf.2)
#here when added sibsp error rate of those who survived decreased by nearly 17% which is what we want since our oibjective is to find factors influencing the durvival or deaths of passengers sailing on titanic boat 


#Now,instead of SibSp,lets use Parch variable and see what difference does it makes on confusion matrix.
rf.train.3<-data.combined[1:891,c("Pclass","title","Parch")]
rf.3<-randomForest(x=rf.train.3,y=rf.label,importance=TRUE,ntree=1000)
varImpPlot(rf.3)



#Now lets use all 4 of them at once 
rf.train.4<-data.combined[1:891,c("Pclass","title","SibSp","Parch")]
rf.4<-randomForest(x=rf.train.4,y=rf.label,importance = TRUE,ntree = 1000)
varImpPlot(rf.4)
#I think this is by far the best model with lowest error rate 18.74%


rf.train.5<-data.combined[1:891,c("Pclass","title","Sex")]
rf.5<-randomForest(x=rf.train.5,y=rf.label,importance = TRUE,ntree = 1000)
varImpPlot(rf.5)

#I think i will use rF.5 FOR FURTHER MODELLING AND TWEAKING AS IT HAS THE MOST ACCURACY SCORE 