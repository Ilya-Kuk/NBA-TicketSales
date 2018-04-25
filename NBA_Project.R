## Importing Packages and Setting Seed

#install.packages("tree")
require(tree)
#install.packages('randomForest')
library(randomForest)
#install.packages("gbm",repos='http://cran.us.r-project.org')
library(gbm)

set.seed(1)

## Data Preprocessing

#Reading in and storing dataset from file
nbatkts=read.csv("nbaTKTS4teams0412_sample.csv",header=T, sep=",")
nbatktsdf=data.frame(nbatkts)

#Creating subset that my computer can handle
 #match(nbatkts$post_tickets_flag,1)
 #dataset <- nbatktsdf[c(1:300,601:1000)]
## Otherwise, uncomment next line
dataset <- nbatktsdf
rm(nbatktsdf,nbatkts) #cleaning up environment space

#Removing unwanted variables, as per *REVISED* Data Dictionary - age is now ignored, age2 is included
data_dictionary <- read.csv("Copy_of_Ticket_Development_Sample_Data_Dictionary2_Copy.csv", header=TRUE, sep=",")
select.Variable <- data_dictionary[,c(2,3)] #matrix of two rows, the first is 1 if selected 0 otherwise, the second is the name of the variable
variables_used <- select.Variable[select.Variable[,1]==1,] #taking only the variables selected in the data dictionary
variables_used <- na.omit(variables_used)
variables_used <- variables_used[,2]
variables_used <- variables_used[-c(3,4,5,6,7,8,9,10,11,59)] #Removing tenure variables, as requested
variables_used <- variables_used[-c(6:23,81:98)] #Removing re-coded variables (SECOND PART WAS 63:80, the data dictionary is not consistent with dataset)
variables_used <- as.character(variables_used) #making a vector of names as opposed to a factor with many levels
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

#Removing other response variables besides post_tickets_flag, index is 43
dataset.ptf <- new_dataset[,-(33:42)]
colnames(dataset.ptf)[31:35] #looking at window around response variable - looks good!
attach(dataset.ptf)

##Training Set

train <- sample(1:nrow(dataset.ptf), nrow(dataset.ptf)*(.8))

## Growing Trees

#Creating Pure Classification Response Vector
y <- post_tickets_flag
Ticket <- ifelse(y<0.5,"No","Yes")
colnames(dataset.ptf)[33]
dataset.ptf[,33] <- Ticket
dataset.ptf[,33] <- factor(dataset.ptf[,33])
attach(dataset.ptf)
rm(y,Ticket) #cleaning up environment space

#Growing Preliminary Classification Tree
tree.ptf <- tree(post_tickets_flag~., dataset.ptf)
plot(tree.ptf)
text(tree.ptf,pretty=0)
#Estimate Error Rate
tree.ptf_train <- tree(post_tickets_flag~., dataset.ptf, subset=train)
plot(tree.ptf_train)
text(tree.ptf_train,pretty=0)
tree.ptf_pred <- predict(tree.ptf_train, dataset.ptf[-train,], type="class")
M <- table(tree.ptf_pred, post_tickets_flag[-train])
M
print(paste("The test error rate is", ((M[1,2]+M[2,1])/(sum(M)))))
#Pruning Tree
cv.ptf_train <- cv.tree(tree.ptf_train, FUN=prune.misclass)
cv.ptf_train
b <- which.min(cv.ptf_train$dev)
print(paste("The best trees seem to be of size",cv.ptf_train$size[b],"with cross-validation error of",cv.ptf_train$dev[b],"."))
prune.ptf_train <- prune.misclass(tree.ptf_train, best=6)
plot(prune.ptf_train)
text(prune.ptf_train,pretty=0)
prune.ptf_pred <- predict(prune.ptf_train, dataset.ptf[-train,], type="class")
M_prune <- table(prune.ptf_pred, post_tickets_flag[-train])
M_prune
print(paste("The test error rate is", ((M_prune[1,2]+M_prune[2,1])/(sum(M_prune)))))
print("A bit better...")

#Creating Bagged Tree
dataset.ptf0 <- dataset.ptf
dataset.ptf0[is.na(dataset.ptf0)] <- 0
attach(dataset.ptf0)
bag.ptf_train <- randomForest(post_tickets_flag~., data=dataset.ptf0, subset=train, mtry=ncol(dataset.ptf0)-1, importance=TRUE)
bag.ptf_pred <- predict(bag.ptf_train, dataset.ptf0[-train,], type="class")
M_bag <- table(bag.ptf_pred, post_tickets_flag[-train])
M_bag
print(paste("The test error rate is", ((M_bag[1,2]+M_bag[2,1])/(sum(M_bag)))))
print("Better yet.")

#Creating Random Forest
RF.ptf_train <- randomForest(post_tickets_flag~., data=dataset.ptf0, subset=train, importance=TRUE)
RF.ptf_pred <- predict(RF.ptf_train, dataset.ptf0[-train,], type="class")
M_RF <- table(RF.ptf_pred, post_tickets_flag[-train])
M_RF
print(paste("The test error rate is", ((M_RF[1,2]+M_bag[2,1])/(sum(M_RF)))))
print("A bit worse... What makes a good mtry argument? What about ntree? Use lab chapter 8 first question.")

#Creating Boosted Tree
boost.ptf_train <- gbm(post_tickets_flag~., dataset.ptf[train], distribution='gaussian', n.trees=5000, interaction.depth=4)
boost.ptf_pred <- predict(boost.ptf_train, dataset.ptf[-train,], type='class')
M_boost <- table(boost.ptf_pred, post_tickets_flag[-train])
M_boost
print(paste("The test error rate is", ((M_boost[1,2]+M_boost[2,1])/(sum(M_boost)))))
print("Make the same kind of heatmap using shrinkage and interaction.depth?")

#### Using Different Response Variables. Not only whether a fan ever bought a ticket.
  #Some reasonable questions to ask are:
    # A) Who should we sell to? - Which markets are the most loyal/active?
    # B) Come post-season, who should we sell post-season tickets to? - Which markets have the most potential to spend come the end of a season (to drive post-seaston sales)?
    # C) Who should we sell the best seats to? - Which markets are willing to spend the most?
  #For these questions, the following predictors are useful:
    # A) how many tickets a fan bought
    # B) how much total money was spent on tickets
    # C) who buys the most expensive tickets? Not only whether a fan ever bought a ticket.

###A)

#Removing other response variables besides post_total_tickets, index is 34
dataset.ptt <- new_dataset[,-c(33,35:43)]
colnames(dataset.ptt)[31:36] #looks good!
attach(dataset.ptt)

#Growing Preliminary Regression Tree
tree.ptt <- tree(post_total_tickets~., dataset.ptt)
plot(tree.ptt)
text(tree.ptt,pretty=0)
#Obtaining MSE
tree.ptt_train <- tree(post_total_tickets~., dataset.ptt, subset=train)
plot(tree.ptt_train)
text(tree.ptt_train,pretty=0)
tree.ptt_pred <- predict(tree.ptt_train, dataset.ptt[-train,])
plot(tree.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.ptt <- mean((tree.ptt_pred-post_total_tickets[-train])^2)
MSE.ptt
print(paste("Test MSE is",MSE.ptt,". Root MSE is",sqrt(MSE.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.ptt),1),"total tickets purchased. This is pretty bad, considering the vast majority seem to have bought fewer than 20 or so tickets total."))
#Pruning Tree
cv.ptt_train <- cv.tree(tree.ptt_train)
cv.ptt_train
b <- which.min(cv.ptt_train$dev)
print(paste("The best trees seem to be of size",cv.ptt_train$size[b],"with cross-validation error of",cv.ptt_train$dev[b],"."))
prune.ptt_train <- prune.tree(tree.ptt_train, best=5)
plot(prune.ptt_train)
text(prune.ptt_train,pretty=0)
prune.ptt_pred <- predict(prune.ptt_train, dataset.ptt[-train,])
plot(prune.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.prune.ptt <- mean((prune.ptt_pred-post_total_tickets[-train])^2)
MSE.prune.ptt
print(paste("Test MSE is",MSE.prune.ptt,". Root MSE is",sqrt(MSE.prune.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.prune.ptt),1),"total tickets purchased. Successfully pruned, but still pretty bad."))

#Creating Bagged Tree
dataset.ptt0 <- dataset.ptt
dataset.ptt0[is.na(dataset.ptt0)] <- 0
attach(dataset.ptt0)
bag.ptt_train <- randomForest(post_total_tickets~., data=dataset.ptt0, subset=train, mtry=ncol(dataset.ptt0)-1, importance=TRUE)
bag.ptt_pred <- predict(bag.ptt_train, dataset.ptt0[-train,])
plot(bag.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.bag.ptt <- mean((bag.ptt_pred-post_total_tickets[-train])^2)
MSE.bag.ptt
print(paste("Test MSE is",MSE.bag.ptt,". Root MSE is",sqrt(MSE.bag.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.bag.ptt),1),"total tickets purchased. No dice."))

#Creating Random Forest
RF.ptt_train <- randomForest(post_total_tickets~., data=dataset.ptt0, subset=train, importance=TRUE)
RF.ptt_pred <- predict(RF.ptt_train, dataset.ptt0[-train,])
plot(RF.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.RF.ptt <- mean((RF.ptt_pred-post_total_tickets[-train])^2)
MSE.RF.ptt
print(paste("Test MSE is",MSE.bag.ptt,". Root MSE is",sqrt(MSE.RF.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.RF.ptt),1),"total tickets purchased. Best of so far, nothing good yet."))

#Creating Boosted Tree
boost.ptt_train <- gbm(post_total_tickets~., dataset.ptt[train,], distribution='gaussian', n.trees=5000, interaction.depth=4)
boost.ptt_pred <- predict(boost.ptt_train, dataset.ptt[-train,], n.trees=5000)
plot(boost.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.boost.ptt <- mean((boost.ptt_pred-post_total_tickets[-train])^2)
MSE.boost.ptt
print(paste("Test MSE is",MSE.boost.ptt,". Root MSE is",sqrt(MSE.boost.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.boost.ptt),1),"total tickets purchased. Still not good enough."))

print("Seems like there's not enough useful data for this prediction.")


#TO DO: INTERPERET RESULTS , FIGURE OUT THE HEATMAP THING AND MAKE THEM , FOR REGRESSION TREES - MAKE BOXPLOT OF NON-ZERO VALUES FOR RESPONSE VARIABLES IN ORDER TO GAUGE MEAN SQUARED ERROR EFFECTIVELY


###B)

#Removing other response variables besides post_total_ticket_amt, index is 33
dataset.pttm <- new_dataset[,-c(34:43)]
colnames(dataset.pttm)[31:35] #looks good!
attach(dataset.pttm)

#Growing Preliminary Regression Tree
tree.pttm <- tree(post_total_ticket_amt~., dataset.pttm)
plot(tree.pttm)
text(tree.pttm,pretty=0)
#Obtaining MSE
tree.pttm_train <- tree(post_total_ticket_amt~., dataset.pttm, subset=train)
plot(tree.pttm_train)
text(tree.pttm_train,pretty=0)
tree.pttm_pred <- predict(tree.pttm_train, dataset.pttm[-train,])
plot(tree.pttm_pred, post_total_ticket_amt[-train])
abline(0,1)
MSE.pttm <- mean((tree.pttm_pred-post_total_ticket_amt[-train])^2)
MSE.pttm
print(paste("Test MSE is",MSE.pttm,". Root MSE is",sqrt(MSE.pttm),", indicating this model leads to test predictions within around",round(sqrt(MSE.pttm),1),"total money spent on tickets."))
#Pruning Tree
cv.pttm_train <- cv.tree(tree.pttm_train)
cv.pttm_train
b <- which.min(cv.pttm_train$dev)
print(paste("The best trees seem to be of size",cv.pttm_train$size[b],"with cross-validation error of",cv.pttm_train$dev[b],"."))
prune.pttm_train <- prune.tree(tree.pttm_train, best=3)
plot(prune.pttm_train)
text(prune.pttm_train,pretty=0)
prune.pttm_pred <- predict(prune.pttm_train, dataset.pttm[-train,])
plot(prune.pttm_pred, post_total_ticket_amt[-train])
abline(0,1)
MSE.prune.pttm <- mean((prune.pttm_pred-post_total_ticket_amt[-train])^2)
MSE.prune.pttm
print(paste("Test MSE is",MSE.prune.pttm,". Root MSE is",sqrt(MSE.prune.pttm),", indicating this model leads to test predictions within around",round(sqrt(MSE.prune.pttm),1),"total money spent on tickets. Identical to above."))

#Creating Bagged Tree
dataset.pttm0 <- dataset.pttm
dataset.pttm0[is.na(dataset.pttm0)] <- 0
attach(dataset.pttm0)
bag.pttm_train <- randomForest(post_total_ticket_amt~., data=dataset.pttm0, subset=train, mtry=ncol(dataset.pttm0)-1, importance=TRUE)
bag.pttm_pred <- predict(bag.pttm_train, dataset.pttm0[-train,])
plot(bag.pttm_pred, post_total_ticket_amt[-train])
abline(0,1)
MSE.bag.pttm <- mean((bag.pttm_pred-post_total_ticket_amt[-train])^2)
MSE.bag.pttm
print(paste("Test MSE is",MSE.bag.pttm,". Root MSE is",sqrt(MSE.bag.pttm),", indicating this model leads to test predictions within around",round(sqrt(MSE.bag.pttm),1),"total money spent on tickets. Better!"))

#Creating Random Forest
RF.pttm_train <- randomForest(post_total_ticket_amt~., data=dataset.pttm0, subset=train, importance=TRUE)
RF.pttm_pred <- predict(RF.pttm_train, dataset.pttm0[-train,])
plot(RF.pttm_pred, post_total_ticket_amt[-train])
abline(0,1)
MSE.RF.pttm <- mean((RF.pttm_pred-post_total_ticket_amt[-train])^2)
MSE.RF.pttm
print(paste("Test MSE is",MSE.bag.pttm,". Root MSE is",sqrt(MSE.bag.pttm),", indicating this model leads to test predictions within around",round(sqrt(MSE.pttm),1),"total money spent on tickets."))

#Creating Boosted Tree
boost.pttm_train <- gbm(post_total_ticket_amt~., dataset.pttm[train,], distribution='gaussian', n.trees=5000, interaction.depth=4)
boost.pttm_pred <- predict(boost.pttm_train, dataset.pttm[-train,], n.trees=5000)
plot(boost.pttm_pred, post_total_ticket_amt[-train])
abline(0,1)
MSE.boost.pttm <- mean((boost.pttm_pred-post_total_ticket_amt[-train])^2)
MSE.boost.pttm
print(paste("Test MSE is",MSE.boost.pttm,". Root MSE is",sqrt(MSE.boost.pttm),", indicating this model leads to test predictions within around",round(sqrt(MSE.boost.pttm),1),"total money spent on tickets."))

print("Seems like there's not enough useful data for this prediction.")


#C)
#Removing other response variables besides post_avg_ticket_price, index is 35
View(colnames(new_dataset))
dataset.patp <- new_dataset[,-c(33:34,36:43)]
colnames(dataset.patp) #looks good!
attach(dataset.patp)