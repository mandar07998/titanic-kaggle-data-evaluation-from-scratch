#in last rscript we obtained a cross validation data set with an accuracy of 83.4% 
#Now in here lets check on the test data set using the same features that we used before.

test.submit.df<-data.combined[892:1309,features]

#now make predictions
rpart.3.preds<-predict(rpart.3.cv.1$finalModel,test.submit.df,type="class")
table(rpart.3.preds)
#when submitted to kaggle we get an accuracy score of 80% which is not bad as rpart.3.cv.1 is around  83.43% 

#As we used rpart last time we use randomforest this time with improved set of features from rpart decision tree.
#so basicaaly we use the features that we got from rpart.3.cv.1 tree(new.title,Pclass,ticket.party.size,avg.fare)so 4 features.And now instead of trainig one whole tree(cart tree)we train random forest trees on it and avg it just like we did it with parch and sibsp earlier.


features<-c("Pclass","new.title","ticket.party.size","avg.fare")
rf.train.temp<-data.combined[1:891,features]


set.seed(1234)
rf.temp<-randomForest(rf.train.temp,rf.label,ntree=1000)
rf.temp

#FROM THE CONFUSION MATRIX GENERATED FOR THE RANDOM FOREST IT IS CLEAR THAT i PREDICTED PEOPLE WHO PERISHED FAR MORE ACCURATELY THAN FOLKS WHO I PREDICTED TO SURVIVE.
#Make Predictions
rf.preds<-predict(rf.temp,test.submit.df)
table(rf.preds)

#here 266 predicted have perished and 152 predicted as survived 
#wheres in table(rpart.3.preds)  267 predicted perished 151 predicted survived

install.packages("infotheo")
library(infotheo)




mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$Sibsp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$Cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


#MUTINFORMATION GIVES US THE relation between the two variables i.e how dependent they are on each other /how one variable is affected by the other 
#from the numbers we get we find that new.title is the most important and related closely with rf.label
#so mutinformation is a good way to find that your engineered feature is a good variable to find your data to be predicitve or not of your label.


#as we know from the latest cart tree anybody who doesnt have the new.title="Mr."our tree does good job of prediciting with less error
#Now we have to worry about Mr. as the predictions vs error is 451 predicted with title mr. to perish and 86 predicted falsely to survive but theyactually perished.
#so 86/86+451= 0.16 percent i.e 16%is the error rate.



install.packages("Rtsne")
library(Rtsne)

most.correct<-data.combined[data.combined$new.title !="Mr.",] #AS Mr. IS THE most error causing term.
indexes<-which(most.correct$Survived !="None")


tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")













# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "Pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]