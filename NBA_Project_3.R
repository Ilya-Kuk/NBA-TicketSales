## Importing Packages and Setting Seed

#install.packages("tree")
require(tree)
#install.packages('randomForest')
library(randomForest)
#install.packages("gbm",repos='http://cran.us.r-project.org')
library(gbm)

set.seed(1)

## Data Preprocessing

#Reading in and storing dataset 'd' from file
d <- read.csv("nbasample2GrpPlus0429.csv",header=T, sep=",")
#attach(d)
#post_tickets_flag
#d <- d[1:1000,]

#Removing ax_ and la_ variables, as requested

a <- colnames(d)
a
AX <- grepl("ax_",a) #seeing which values have prefix ax_
AXN <- c() #creating temporary vector recording positions
for(i in 1:length(a)){ #getting rejected values
  if(AX[i]){
    AXN <- c(AXN,i)
  }
}
AXN
d <- d[,-AXN] #removing rejected values
a <- colnames(d)
a #no ax_ variables
grepl("ax_",a)
LA <- grepl("la_",a)
LAN <- c() #creating temporary vector recording positions
for(i in 1:length(a)){ #getting rejected values
  if(LA[i]){
    LAN <- c(LAN,i)
  }
}
LAN
d <- d[,-LAN] #removing rejected values
a <- colnames(d)
a
grepl("la_",a) #no la_ variables
#Cleaning up Environment
rm(AX,AXN,LA,LAN)

#Removing other Variables

#closest_arena and closest_team are identical - removing closest_arena
x <- match("closest_arena",a)
x
d <- d[,-x]
a <- colnames(d)
match("closest_arena",a) #successfully removed

names(Filter(is.factor, d))#tree() can only handle 32 levels on factor predictors

#zip_team and zip_arena have 56 levels - too many to grow trees on - removing both
x <- match(c("zip_team","zip_arena"),a)
x
d <- d[,-x]
a <- colnames(d)
match(c("zip_team","zip_arena"),a) #successfully removed
#email_domain has 1468 levels - too many.. -removing
x <- match(c("email_domain"),a)
x
d <- d[,-x]
a <- colnames(d)
match(c("email_domain"),a) #successfully removed
#source_date has 3773 levels - too many.. -removing
x <- match(c("source_date"),a)
x
d <- d[,-x]
a <- colnames(d)
match(c("source_date"),a) #successfully removed

## Viewing Response Variables

POST <- grepl("post_",a)
POSTN <- c()
for(i in 1:length(a)){ #getting rejected values
  if(POST[i]){
    POSTN <- c(POSTN,i)
  }
}
POSTN
a[POSTN]

### Questions: A Who buys tickets? , B How many tickets do people buy? , C How much do people spend on tickets? , D Who buys tickets at which price?
### Response variables: A post_tickets_flag , B post_total_tickets , C post_total_ticket_amt , D post_avg_ticket_price

#A
#Removing other response variables besides post_tickets_flag
Q <- match('post_tickets_flag',a)
a[Q]
POSTN_ptf <- POSTN[! POSTN %in% Q]
POSTN_ptf
a[POSTN_ptf]
dataset.ptf <- d[,-POSTN_ptf]
attach(dataset.ptf)


##Training Set

train <- sample(1:nrow(dataset.ptf), nrow(dataset.ptf)*(.8))

## Growing Trees

#Creating Pure Classification Response Vector
#Finding response vector
a.ptf <- colnames(dataset.ptf)
resp.ptf <- grepl("post_",a.ptf)
for(i in 1:length(a.ptf)){
  if(resp.ptf[i]){
    R <- i
  }
}
a.ptf[R]
#Converting it to a yes/no factor
y <- post_tickets_flag
Ticket <- ifelse(y<0.5,"No","Yes")
a.ptf[R]
dataset.ptf[,R] <- Ticket
dataset.ptf[,R] <- factor(dataset.ptf[,R])
attach(dataset.ptf)
rm(Ticket) #cleaning up environment space

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
prune.ptf_train <- prune.misclass(tree.ptf_train, best=11)
plot(prune.ptf_train)
text(prune.ptf_train,pretty=0)
prune.ptf_pred <- predict(prune.ptf_train, dataset.ptf[-train,], type="class")
M_prune <- table(prune.ptf_pred, post_tickets_flag[-train])
M_prune
print(paste("The test error rate is", ((M_prune[1,2]+M_prune[2,1])/(sum(M_prune)))))
print("Same error rate.")

#Creating Bagged Tree
#Creating a modified dataset - na values are not accepted in bagged trees
dataset.ptf0 <- dataset.ptf
dataset.ptf0[is.na(dataset.ptf0)] <- 0
attach(dataset.ptf0)
bag.ptf_train <- randomForest(post_tickets_flag~., data=dataset.ptf0, subset=train, mtry=ncol(dataset.ptf0)-1, ntrees=500, importance=TRUE)
bag.ptf_pred <- predict(bag.ptf_train, dataset.ptf0[-train,], type="class")
M_bag <- table(bag.ptf_pred, post_tickets_flag[-train])
M_bag
print(paste("The test error rate is", ((M_bag[1,2]+M_bag[2,1])/(sum(M_bag)))))
print("About 90% accurate!")
#Deciding the best ntrees argument (plot.randomForest() is suspect)
#### TAKES A LONG TIME. PLOT IS SAVED AS NTREES.PDF
Z <- matrix(c("",""),nrow=1,ncol=2)
colnames(Z) <- c("nTrees","Error")
for(t in 1:20){
  Tr <- t*250
  bag.ptf_train <- randomForest(post_tickets_flag~., data=dataset.ptf0, subset=train, mtry=ncol(dataset.ptf0)-1, ntrees=Tr)
  bag.ptf_pred <- predict(bag.ptf_train, dataset.ptf0[-train,], type="class")
  M_bag <- table(bag.ptf_pred, post_tickets_flag[-train])
  E <- ((M_bag[1,2]+M_bag[2,1])/(sum(M_bag)))
  Z <- rbind(Z, c(Tr,E))
}
Z <- Z[-1,]
plot(Z)

#creating a labeled plot to understand this plot - the black represents overall error
#layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
#par(mar=c(5,4,4,0)) #No margin on the right side
#plot(bag.ptf_train)
#par(mar=c(5,0,4,2)) #No margin on the left side
#plot(c(0,1),type="n", axes=F, xlab="", ylab="")
#legend("top", colnames(bag.ptf_train$err.rate),col=1:4,cex=0.8,fill=1:4)
#dev.off()

#Looking at the importance of each predictor
bag.I <- importance(bag.ptf_train)
bag.I <- bag.I[ order(bag.I[,3],decreasing=TRUE), ]
print("The fifteen most important predictors (by error) according to this model are as follows:")
bag.I[1:15,] #includes variable 'tenure' - is this to be removed from this dataset as well?
bag.I <- bag.I[ order(bag.I[,4],decreasing=TRUE), ]
print("The fifteen most important predictors (by Gini index) according to this model are as follows:")
bag.I[1:15,] #includes variable 'tenure' - is this to be removed from this dataset as well?

#Creating Random Forest
RF.ptf_train <- randomForest(post_tickets_flag~., data=dataset.ptf0, subset=train, importance=TRUE)
RF.ptf_pred <- predict(RF.ptf_train, dataset.ptf0[-train,], type="class")
M_RF <- table(RF.ptf_pred, post_tickets_flag[-train])
M_RF
print(paste("The test error rate is", ((M_RF[1,2]+M_bag[2,1])/(sum(M_RF)))))
print("More than 90% accurate!")


importance(RF.ptf_train)


#Creating Various Random Forests
#Initializing matrix of variables sampled, tree count, and test MSE
Analysis <- matrix(
  c("",""),
  nrow=1,
  ncol=2)
colnames(Analysis) <- c("nVariables","MSE")
#######
for(i in 1:((ncol(dataset.ptf)-1)/4)){
  M <- 4*i
  RF.ptf_train <- randomForest(post_tickets_flag~., data=dataset.ptf0, subset=train, mtry=M, ntrees=1000)
  RF.ptf_pred <- predict(RF.ptf_train, dataset.ptf0[-train,], type="class")
  M_RF <- table(RF.ptf_pred, post_tickets_flag[-train])
  E <- ((M_RF[1,2]+M_RF[2,1])/(sum(M_RF)))
  Analysis <- rbind(Analysis, c(M,E))
}
#Removing first (empty) row of Matrix
Analysis <- Analysis[-1,]
print("Top 10 random forests based on MSE:")
#Ordering Matrix based on error rate
Analysis <- Analysis[ order(Analysis[,2],decreasing=FALSE), ]
Analysis[1:10,]



#Creating Boosted Tree
attach(dataset.ptf)
#Creating a modified dataset - columns with no variation are not accepted in boosted trees
dataset.ptfB <- dataset.ptf
colnames(dataset.ptfB)[c(36,56)]
dataset.ptfB <- dataset.ptfB[,-c(36,56)]
attach(dataset.ptfB)
#Modify response variable
dataset.ptfB$post_tickets_flag <- y
#distribution='bernoulli' requires 0,1 variable
boost.ptf_train <- gbm(post_tickets_flag~., data=dataset.ptfB[train,], distribution='bernoulli', n.trees=5000, interaction.depth=4)
boost.ptf_pred <- predict(boost.ptf_train, dataset.ptfB[-train,], type='class', n.trees=5000)
M_boost <- table(boost.ptf_pred, post_tickets_flag[-train])
M_boost
print(paste("The test error rate is", ((M_boost[1,2]+M_boost[2,1])/(sum(M_boost)))))
print("Make the same kind of heatmap using shrinkage and interaction.depth?")

#B
#Removing other response variables besides post_total_tickets
Q <- match('post_total_tickets',a)
a[Q]
POSTN_ptt <- POSTN[! POSTN %in% Q]
POSTN_ptt
a[POSTN_ptt]
dataset.ptt <- d[,-POSTN_ptt]
attach(dataset.ptt)

## Growing Trees

#Growing Preliminary Regression Tree
tree.ptt <- tree(post_total_tickets~., dataset.ptt)
plot(tree.ptt)
text(tree.ptt,pretty=0)
#Estimate Error Rate
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
cv.ptt_train <- cv.tree(tree.ptt_train, FUN=prune.misclass)
cv.ptt_train
b <- which.min(cv.ptt_train$dev)
print(paste("The best trees seem to be of size",cv.ptt_train$size[b],"with deviation of",cv.ptt_train$dev[b],"."))
prune.ptt_train <- prune.tree(tree.ptt_train, best=11)
plot(prune.ptt_train)
text(prune.ptt_train,pretty=0)
prune.ptt_pred <- predict(prune.ptt_train, dataset.ptt[-train,])
plot(prune.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.prune.ptt <- mean((prune.ptt_pred-post_total_tickets[-train])^2)
MSE.prune.ptt
print(paste("Test MSE is",MSE.prune.ptt,". Root MSE is",sqrt(MSE.prune.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.prune.ptt),1),"total tickets purchased. Successfully pruned, but still pretty bad."))

#Creating Bagged Tree
#Creating a modified dataset - na values are not accepted in bagged trees
dataset.ptt0 <- dataset.ptt
dataset.ptt0[is.na(dataset.ptt0)] <- 0
attach(dataset.ptt0)
bag.ptt_train <- randomForest(post_total_tickets~., data=dataset.ptt0, subset=train, mtry=ncol(dataset.ptt0)-1, ntrees=1000, importance=TRUE)
bag.ptt_pred <- predict(bag.ptt_train, dataset.ptt0[-train,])
plot(bag.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.bag.ptt <- mean((bag.ptt_pred-post_total_tickets[-train])^2)
MSE.bag.ptt
print(paste("Test MSE is",MSE.bag.ptt,". Root MSE is",sqrt(MSE.bag.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.bag.ptt),1),"total tickets purchased. No dice."))

#Looking at the importance of each predictor
bag.I <- importance(bag.ptt_train)
bag.I <- bag.I[ order(bag.I[,3],decreasing=TRUE), ]
print("The fifteen most important predictors (by error) according to this model are as follows:")
bag.I[1:15,]
bag.I <- bag.I[ order(bag.I[,4],decreasing=TRUE), ]
print("The fifteen most important predictors (by Gini index) according to this model are as follows:")
bag.I[1:15,]

#Creating Random Forest
RF.ptt_train <- randomForest(post_total_tickets~., data=dataset.ptt0, subset=train, importance=TRUE)
RF.ptt_pred <- predict(RF.ptt_train, dataset.ptt0[-train,])
plot(RF.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.RF.ptt <- mean((RF.ptt_pred-post_total_tickets[-train])^2)
MSE.RF.ptt
print(paste("Test MSE is",MSE.bag.ptt,". Root MSE is",sqrt(MSE.RF.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.RF.ptt),1),"total tickets purchased. Best of so far, nothing good yet."))

#Looking at the importance of each predictor
RF.I <- importance(RF.ptt_train)
RF.I <- RF.I[ order(RF.I[,3],decreasing=TRUE), ]
print("The fifteen most important predictors (by error) according to this model are as follows:")
RF.I[1:15,]
RF.I <- RF.I[ order(RF.I[,4],decreasing=TRUE), ]
print("The fifteen most important predictors (by Gini index) according to this model are as follows:")
RF.I[1:15,]

#Creating Boosted Tree
attach(dataset.ptt)
#Creating a modified dataset - columns with no variation are not accepted in boosted trees
dataset.pttB <- dataset.ptt
colnames(dataset.pttB)[c(36,56)]
dataset.pttB <- dataset.pttB[,-c(36,56)]
attach(dataset.pttB)
boost.ptt_train <- gbm(post_total_tickets~., data=dataset.pttB[train,], distribution='gaussian', n.trees=5000, interaction.depth=4)
boost.ptt_pred <- predict(boost.ptt_train, dataset.pttB[-train,], n.trees=5000)
plot(boost.ptt_pred, post_total_tickets[-train])
abline(0,1)
MSE.boost.ptt <- mean((boost.ptt_pred-post_total_tickets[-train])^2)
MSE.boost.ptt
print(paste("Test MSE is",MSE.boost.ptt,". Root MSE is",sqrt(MSE.boost.ptt),", indicating this model leads to test predictions within around",round(sqrt(MSE.boost.ptt),1),"total tickets purchased. Still not good enough."))

print("Seems like there's not enough useful data for this prediction.")
