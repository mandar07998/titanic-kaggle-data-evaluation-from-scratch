#decision trees do not give in depth analysis of our data.we get deeper understanding of data with CART algorithm/tree
#from rf.5 we got that "title" variable is most impportant in deciding the survibility of people on titanic followed by "Pclass","familysize"
#however its not always informative in finding out what's going on inside the model coz we want mode detailed understandingof what's going on inside the data,find patterns etc.
#thats why we use plain decison tree to look at features adn the way they are mainfested in the tree.,

#we use cart which is in rpart package and plot it using package rpart.plot

install.packages("rpart")
install.packages("rpart.plot")


##we rf.5.cv.3(3 fold repeated 10 times cv)aS we got the highest accuracy score wuing it 81.39%


#from previous script wse remember that we made 3 clusters(cl1,cl2,cl3)with rf.5.cv.1,   rf.5.cv.2,    rf.5.cv.3
#This we will have to do for cart algo as well as we did for random forests.
#So what I do is create a function so that we don't have to repeath 3 times .Just use different folds and different clusters 



#creating utility function
rpart.cv<-function(seed,training,labels,ctrl){
  cl<-makeCluster(6,"SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  #do the trainig as we did with rf.5.cv1 or 2 or 3 but this time do not use method=randomforest use rpart(cart algorithm)
  rpart.cv<-train(x=training,y=labels,method="rpart",tuneLength=30,trControl=ctrl)
  
  stopCluster(cl)
  
  return(rpart.cv)
}


#now that we have written generalized function 
#extract the imp features that we want 
features<-c("Pclass","title","family.size")
#now as we did for rf training for ie. rf.5 
rpart.train.1<-data.combined[1:891,features]

#now pass this training set to make cross valaidation set out of it using cart algorith 

rpart.1.cv.1<-rpart.cv(94622,rpart.train.1,rf.label,ctrl.3)
##where ctrl.3 is ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,index = cv.3.folds)
#and cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)


#after running the above model for rpart and storing the reuslt of cross validation in rpart.1.cv.1 we get highest accuracry so far of 82.10999
#this is coz in cart algo we use one tree instead of many decison tress which results in less overfitting of data 
#this increases the accuracy by more than a percent 
#accurarcy is 82.1099



##plot this using rpart.plot
prp(rpart.1.cv.1$finalModel,type=0,extra=1,under=TRUE)

#the plot brings about this 4 main conclusions:

#1.ttiles of "Mr" and "Other" are predicted to perish with overall accuracy score of 83.2%
#so this need some more data analysis 
#2.Titles of"Master","Miss","Mrs" not in 3rd classs are predicted to survive with accuracy score of 168/177 wich is 94.9%
#3.Title sof"Master","Miss","Mrs" in 3rd class with family size equal to 5,6,8,11 are predicted to perish with 100%accuracy.
#4.titles of "master","miss","mrs" in 3rd classs with family size not equal to 5,6,8,11 are predicted to survive with 55/55+83 i.e 59.6%accuracy.





# Both rpart and rf confirm that title is important, let's investigate further
table(data.combined$title)

data.combined[1:25,"Name"]

name.splits<-strsplit(as.character(data.combined$Name),",")

last.names<-sapply(name.splits,"[",1)
last.names[1:10]

#add last names to data frame in case we need it later 
data.combined$last.names<-last.names


#Now do the same thing for titles 

name.splits<-strsplit(sapply(name.splits,"[",2)," ")
titles<-sapply(name.splits,"[",2)
unique(titles)


#lets check what's up with the title "the"

data.combined[which(titles=="the"),]

#remapt the titleswto be more exact.call them "Lady."for "the" and "Dona"
titles[titles %in% c("Dona","the")]<-"Lady."
titles[titles %in% c("Ms.","Mlle.")]<-"Miss."
titles[titles=="Mme."]<-"Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)


#now add this variable with modified titles in  a variable new.title in data.combined
data.combined$new.title<-as.factor(titles)

#now plot this 
library(ggplot2)
ggplot(data.combined[1:891,],aes(x=as.character(new.title),fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival rates for new.title by Pclass")

#we now have reduced number of titles 


#based on visual analysis we conclude that
#1.Based on visual graph when Pclass=3 then there are no "Dr.","Lady.","Officer.","Rev.","Sir." This means that our assumption and common sense is right that these category of people only buy expensive tickets(of first class only,since there are no Sir or Lady in 2nd and 3rd class)
#2.Female nobility survived entirely in first class("Lady.").But for male nobility("Sir.")in first class has pretty abysmal survival rates.
#3.sir.can be put into Mr.   and    Lady can be put into Mrs.



#so collapsing titles based on visual analysis as mentioned in point 3 above 
indexes<-which(data.combined$new.title=="Lady.")
data.combined$new.title[indexes]<-"Mrs."


indexes<-which(data.combined$new.title=="Dr." |
               data.combined$new.title=="Sir."|
               data.combined$new.title=="Rev."|
               data.combined$new.title=="Officer.")
data.combined$new.title[indexes]<-"Mr."


#now plot this new collapsed&reduced features with ggplot 
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")

#now check the accuracy by creating a new cart tree
#grab the features 0
features<-c("Pclass","new.title","family.size")
rpart.train.2<-data.combined[1:891,features]

rpart.2.cv.1<-rpart.cv(94622,rpart.train.2,rf.label,ctrl.3)

##plot this using rpart.plot of for new cv
prp(rpart.2.cv.1$finalModel,type=0,extra=1,under=TRUE)


#For rpart.2.cv.1 ::from the cart graph visualization we can see that new.title=Mr. and "officer"in first class the no of predicted perished cases is 451 and 87 were precicted wrong(i.e they survived)
#this figure has improved as earlier from rpart.1.cv.1(451,91)this is now 451,87



#So now lets dive in in first class Mr. a little more:
indexes.first.mr<-which(data.combined$new.title=="Mr." & data.combined$Pclass=="1")
first.mr.df<-data.combined[indexes.first.mr,]
summary(first.mr.df)


#from the summary it is interesting to note that we made an error above with our human bias in assuming that title="Dr."must be a male at the time of titanic as women weren't as progressive as they are now back then.
#so we grouped Dr. along with "Sir","Rev","Officer"in Mr. category and went on with our cart tree which resulted in this error here 
#so by fixing this error we may improve our accuracy score somehow from 82.6262(rpart.2.cv.2)to something slightly better.
#her name is Dr.Alice farnham

first.mr.df[first.mr.df$Sex=="female",]
#this can be easily fixed below 
indexes<-which(data.combined$new.title=="Mr."&
               data.combined$Sex=="female")

data.combined$new.title[indexes]<-"Mrs."
#Check if there are any other gender check up /slip outs as well
length(which(data.combined$sex == "female" & 
               (data.combined$new.title == "Master." |
                  data.combined$new.title == "Mr.")))


#we will have to refresh our data frame as well
indexes.first.mr<-which(data.combined$new.title=="Mr."& data.combined$Pclass=="1")
first.mr.df<-data.combined[indexes.first.mr,]


#lets look at surviving first class Mr.
summary(first.mr.df[first.mr.df$Survived=="1",])
View(first.mr.df[first.mr.df$Survived=="1",])

#we can see from first.mr.df that most expensive fare"fare" 512 pounds have same "ticket" name
#How can many people have same ticket number and name.
#the only logical answer is that maybe they are all related people who shared the same ticket and the fare that they paid is also divided and not 512 per person

indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")

View(data.combined[indexes,])


#we can see that ticket with number"PC 17755" has 4  people.Out of those two,2 HAVE last name Cardeza(a female and a male).My hypothesis is Mrs.james Cardeza(age is 58)has a Son named Mr.Thomas Cardeza(age 36)
#They cannot be siblings as SibSp variable is empty in both of them and Parch is 1 and their family size is 2(which means they themself and their son/mother)
#Also with ticket no mentioned above there are 2 people with no family .
#So why would they stay with cardeza(or why would Cardezas rather allow them to stay with them)if they are not the part of the family.
#The only logical explanation is is they are their personal servants/maid/body guard etc .This makes sense coz they are very rich



#Now let's visualize the survival rates of first class "Mr."by fare 
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


#Now based on a which "Ticket"lets find out how many people are actually travelling on that particlar ticket and what is the average fare of all the people travelling on that ticket .

#so lets engineer some features based on all the passengers with the same ticket.
ticket.party.size<-rep(0,nrow(data.combined))
avg.fare<-rep(0.0,nrow(data.combined))
tickets<-unique(data.combined$Ticket)


for(i in 1:length(tickets)){                    #this loop runs 929 times since length(tickets=929)
  current.ticket<-tickets[i]                    #tickets[5] crams the fifth ticket into current.ticket
  party.indexes<-which(data.combined$Ticket==current.ticket)  #give me the indexes of 5th ticket[which is 373450] in data.combined.There can be instances
  current.avg.fare<-data.combined[party.indexes[1],"Fare"]/length(party.indexes)

  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  } 
}
data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare


# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)


# Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


#Now in both these plots ,one is redundant and implies the same thing as other.because higher avg fare means ticket.party.size is also very high 
summary(data.combined$avg.fare)
#we can see that there is one missingrecord i.e na value.

#So we do first spot which is that na value 
data.combined[is.na(data.combined$avg.fare),]

#what we can do here is compute a machine learning model to deduce the avg.fare but that would be just waste of time to do for calculating just one avg.fare
#so what we do instead is find similiar data which matches the characteristics of Mr.Thomas who has a third class tickets who is a "Mr."with family size=1
#search for all records to find similiar traits 


indexes <- with(data.combined, which(Pclass == "3" & title == "Mr." & family.size == 1 &
                                       Ticket != "3701"))
similiar.na.passengers<-data.combined[indexes,]
#thus we stored similiar attributed males like Mr.Thomas in a new dataframe similiar.na.passengers
summary(similiar.na.passengers)

#from the summary we got that Mr.Thomas avg.fare might be 7-8 pounds.to be more precise but not accurate take the value of median of avg fare wihich is 7.840 pounds .So Mr.Thomas might have paid that much as his fair.
#so lets just update Mr thomas age so that there is no NA value in entire avg.fare
data.combined[is.na(avg.fare),"avg.fare"]<-7.840

#now that we have all the data needed to find the co-relation between avg.fare and people travelling on a ticket(ticket.party.size)
preproc.data.combined<-data.combined[,c("ticket.party.size","avg.fare")]
preProc<-preProcess(preproc.data.combined,method = c("center","scale"))

#we use cor()to find the corelation between two distict variables so that we can decide wether to use them as two different variables or combine them into same variable.
postproc.data.combined<-predict(preProc,preproc.data.combined)

cor(as.numeric(data.combined$ticket.party.size),as.numeric(data.combined$avg.fare))
#the answer of correlation is 0.0942 
#here if answer is -1 then it is negatively corelated.1 then it is positively corelated.0 means it is not corelated.
#our answer is 0.09 which is close to 0 which means ticket.party.size and avg.fare are highly un-corelated and so they can be considered as two distict features instead of removing one as it seemed redundant to us in first place.


#now check the same thing with only first class people

indexes<-which(data.combined$Pclass=="1")
cor(postproc.data.combined$ticket.party.size[indexes],postproc.data.combined$avg.fare[indexes])


#this give a corelation of 0.25 which is again close to 0.257 which is very close tom 0 so they are somewhat related but very less.So use them as two different variales for further manipulation.



#Now we can train new rpart tree with these two added features 
features<-c("Pclass","new.title","family.size","ticket.party.size","avg.fare")
rpart.train.3<-data.combined[1:891,features]


#run cross validation and check your result 
rpart.3.cv.1<-rpart.cv(94622,rpart.train.3,rf.label,ctrl.3)
rpart.3.cv.1
#here we get accuracy score 83.4% which is the highest accuracy.

#here we get an accuracy score which is the highest of 0.834 which is 83 percent
prp(rpart.3.cv.1$finalModel,type=0,extra = 1,under=TRUE)


#WE Can see in all single tree algoriths(in this case cart algo)executes strong feature selection.Wheres in rf where there are mutiple trees which feature should be selected is infered .
#so what I mean in random forest we get a relative priority of which features are useful and which are not and so its on us which one to select and reject.
#In in cart algorith features are already selected we get what operations are to be performed on those features.

#So maybe this model best fits the unseen dataset on kaggle.So rpart.3.cv.1 is the best  cross validation dataset so far we got!

