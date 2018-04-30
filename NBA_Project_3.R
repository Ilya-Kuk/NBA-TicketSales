d <- read.csv("nbasample2GrpPlus0429.csv",header=T, sep=",")

## Data Preprocessing

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

#NOT WORKING
#Creating Boosted Tree
attach(dataset.ptf)
boost.ptf_train <- gbm(post_tickets_flag~., dataset.ptf[train], distribution='gaussian', n.trees=5000, interaction.depth=4)
boost.ptf_pred <- predict(boost.ptf_train, dataset.ptf[-train,], type='class')
M_boost <- table(boost.ptf_pred, post_tickets_flag[-train])
M_boost
print(paste("The test error rate is", ((M_boost[1,2]+M_boost[2,1])/(sum(M_boost)))))
print("Make the same kind of heatmap using shrinkage and interaction.depth?")
