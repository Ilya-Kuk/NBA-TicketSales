## Importing Packages and Setting Seed

install.packages("tree")
require(tree)
install.packages('randomForest')
library(randomForest)

set.seed(1)

## Data Preprocessing

#Reading in and storing dataset from file
nbatkts=read.csv("nbaTKTS4teams0412_sample.csv",header=T, sep=",")
nbatktsdf=data.frame(nbatkts)

#Creating subset that my computer can handle
 #match(nbatkts$post_tickets_flag,1)
 #dataset <- nbatktsdf[c(1:100,601:700),]
 ## Otherwise, uncomment next line
dataset <- nbatktsdf
rm(nbatktsdf,nbatkts) #cleaning up environment space

#Removing unwanted variables, as per Data Dictionary
data_dictionary <- read.csv("Copy_of_Ticket_Development_Sample_Data_Dictionary2.csv", header=TRUE, sep=",")
select.Variable <- data_dictionary[,c(2,3)] #matrix of two rows, the first is 1 if selected 0 otherwise, the second is the name of the variable
variables_used <- select.Variable[select.Variable[,1]==1,] #taking only the variables selected in the data dictionary
variables_used <- na.omit(variables_used)
variables_used <- variables_used[,2]
variables_used <- variables_used[-c(3,4,5,6,7,8,9,10,11,59)] #Removing tenure variables, as requested
variables_used <- variables_used[-c(6:23,81:98)] #Removing re-coded variables (SECOND PART WAS 63:80, the data dictionary is not consistent with dataset)
variables_used <- as.character(variables_used)
#Creating new dataset using only the useful variables, as per Data Dictionary
Not_Variables <- c()
Are_Variables <- c()
all_variables <- colnames(dataset)
first_col <- rep(NA, times=nrow(dataset))
new_dataset <- data.frame(first_col) #initializing dataframe with null values, to be removed later
for(i in 1:length(variables_used)){  #adding column to new dataset if select==1, and recording the variables used and the variables in data dictionary that aren't in dataset
  V <- variables_used[i]
  J <- match(V,all_variables)
  ifelse(
    is.na(J),
    Not_Variables <- c(Not_Variables,V),
    new_dataset <- cbind(new_dataset,dataset[,J])
  )
  if(is.na(J)==FALSE) Are_Variables <- c(Are_Variables,V)
}
new_dataset <- new_dataset[,-1] #removing first (null) column
if(ncol(new_dataset)==length(Are_Variables)) colnames(new_dataset) <- Are_Variables #adjusting column names
rm(data_dictionary,dataset,select.Variable,all_variables,Are_Variables,Not_Variables,i,J,V,variables_used,first_col) #cleaning up environment space

#Removing other response variables besides post_tickets_flag
colnames(new_dataset)
new_dataset <- new_dataset[,-(33:42)]
colnames(new_dataset) #looks good!
attach(new_dataset)

## Growing Trees

#Creating Pure Classification Response Vector
y <- post_tickets_flag
Ticket <- ifelse(y<0.5,"No","Yes")
colnames(new_dataset)[33]
new_dataset[,33] <- Ticket
new_dataset[,33] <- factor(new_dataset[,33])
attach(new_dataset)
rm(y,Ticket) #cleaning up environment space

#Growing Preliminary Classification Tree
 #tree.nba <- tree(post_tickets_flag~., new_dataset)
 #new_dataset[is.na(new_dataset)] <- 0
 #attach(new_dataset)
tree.nba <- tree(post_tickets_flag~., new_dataset)
plot(tree.nba)
text(tree.nba,pretty=0)
#Training Set and Test Set
train <- sample(1:nrow(new_dataset), nrow(new_dataset)*(.8))
new_dataset_train <- new_dataset[train,] #train X
new_dataset_test <- new_dataset[-train,] #test X
ptf_test <- post_tickets_flag[-train] #test y
#Estimate Error Rate
tree.nba_train <- tree(post_tickets_flag~., new_dataset, subset=train)
plot(tree.nba_train)
text(tree.nba_train,pretty=0)
######## What's going on here?
tree.nba_pred <- predict(tree.nba_train, ptf_test, type="class")
length(tree.nba_pred)
length(ptf_test)
##
tree.nba_pred <- predict(tree.nba_train, post_tickets_flag[-train], type="class")
length(tree.nba_pred)
length(post_tickets_flag[-train])
##
tree.nba_pred <- predict(tree.nba_train, post_tickets_flag[train], type="class")
length(tree.nba_pred)
length(post_tickets_flag[train])
##
tree.nba_pred <- predict(tree.nba_train, post_tickets_flag, type="class")
length(tree.nba_pred)
length(post_tickets_flag)
########
table(tree.nba_pred, post_tickets_flag)
M <- table(tree.nba_pred, post_tickets_flag)
print(paste("The <TEST> error rate is", ((M[1,2]+M[2,1])/(sum(M)))))
#Pruning Tree
cv.nba_train <- cv.tree(tree.nba_train, FUN=prune.misclass)
cv.nba_train
b <- which.min(cv.nba_train$dev)
print(paste("The best trees seem to be of size",cv.nba_train$size[b],"with cross-validation error of",cv.nba_train$dev[b],"."))
prune.nba_train <- prune.misclass(tree.nba_train, best=11)
plot(prune.nba_train)
text(prune.nba_train,pretty=0)
prune.nba_pred <- predict(prune.nba_train, ptf_test, type="class")
########
length(prune.nba_pred)
########
M_prune <- table(prune.nba_pred, post_tickets_flag)
M_prune
print(paste("The <TEST> error rate is", ((M_prune[1,2]+M_prune[2,1])/(sum(M_prune)))))
print("Not much better at all...")

#Creating Bagged Tree
new_dataset0 <- new_dataset
new_dataset0[is.na(new_dataset0)] <- 0
attach(new_dataset0)
bag.nba_train <- randomForest(post_tickets_flag~., data=new_dataset0, subset=train, mtry=ncol(new_dataset0)-1, importance=TRUE)
######## forces to use entire response variable, not test response variable
bag.nba_pred <- predict(bag.nba_train, post_tickets_flag, type="class")
########
M_bag <- table(bag.nba_pred, post_tickets_flag)
M_bag
print(paste("The <TEST> error rate is", ((M_bag[1,2]+M_bag[2,1])/(sum(M_bag))),"HORRIBLE..."))

#Creating Random Forest
RF.nba_train <- randomForest(post_tickets_flag~., data=new_dataset0, subset=train, importance=TRUE)
######## forces to use entire response variable, not test response variable
RF.nba_pred <- predict(RF.nba_train, post_tickets_flag, type="class")
########
M_RF <- table(RF.nba_pred, post_tickets_flag)
M_RF
print(paste("The <TEST> error rate is", ((M_bag[1,2]+M_bag[2,1])/(sum(M_bag))),"HORRIBLE..."))
