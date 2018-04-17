
library(ggplot2)
library(gridExtra)
library(MASS)
library(vcd)
library(lme4)
library(captioner)
library(tree)
library(tidyr)
library(gridExtra)

setwd("d:\\backup2017July17\\NBA\\2018April03")
nbatkts=read.csv("nbaTKTS3teams35095unkdel.csv",header=T, sep=",")
attach(nbatkts)


barplot(table(as.factor(gender)),xlab="Gender",col="skyblue3",
        main="Fans by Gender")
mosaicplot(table(as.factor(gender),as.factor(post_tickets_flag)),color = c("skyblue4","skyblue1"), 
           main="Ticket Purchase Propensity by Gender")

barplot(table(as.factor(agecat)),xlab="Age Category", col="blue3",
        main="Fans by Age")
mosaicplot(table(as.factor(agecat),as.factor(post_tickets_flag)),color = c("blue4","blue1"), 
           main="Ticket Purchase Propensity by Age")                                          


barplot(table(as.factor(emaildays)),xlab="emaildays",col="purple3",
        main="Fans by Email Days")
mosaicplot(table(as.factor(emaildays),as.factor(post_tickets_flag)),color = c("purple4","purple1"), 
           main="Ticket Purchase Propensity by Email Days")

barplot(table(as.factor(domain)),xlab="emaildays",col="pink3",
        main="Fans by Email Domain")
mosaicplot(table(as.factor(domain),as.factor(post_tickets_flag)),color = c("pink4","pink1"), 
           main="Ticket Purchase Propensity by Email Domain")

barplot(table(as.factor(arenadis)),xlab="Distance to Nearest Arena",col="orange3", 
        main="Fans by Distance to Arena")
mosaicplot(table(as.factor(arenadis),as.factor(post_tickets_flag)),color = c("orange4","orange1"),
           main="Ticket Purchase Propensity by Distance to Nearest Arena")


barplot(table(as.factor(Division)),xlab="Division",col="red3",
        main="Fans by Division")
mosaicplot(table(as.factor(Division),as.factor(post_tickets_flag)),color = c("red4","red1"), 
           main="Ticket Purchase Propensity by Division")

nbatktsdf=data.frame(nbatkts)

barplot(table(as.factor(Team)),xlab="Team",col="red3",cex.axis = 0.60,las=2,
        main="Fans by Team")
mosaicplot(table(as.factor(Team),as.factor(post_tickets_flag)),color = c("red4","red1"), 
           main="Ticket Purchase Propensity by Team",dir = c("v", "h"))



# df$ftor <- factor(df$ftor, levels=df$ftor[order(df$order_ID)], ordered=TRUE)
# cc.df$origin <- reorder(cc.df$origin, - cc.df$count)
# https://sarahleejane.github.io/learning/r/2014/09/17/ordering-factors-by-another-column-with-R.html
# https://www.stat.berkeley.edu/classes/s133/factors.html
# http://monashbioinformaticsplatform.github.io/2015-09-28-rbioinformatics-intro-r/01-supp-factors.html
levels(Team)
teanfactormeans <- by(post_tickets_flag, Team, mean)
teanfactormeans
teanfactormeans[1]
sort(teanfactormeans)
barplot(sort(teanfactormeans, decreasing = TRUE),cex.axis = 0.60,las=2,
        col="red1",main="Ticket Purchase Propensity by Team")

nbatktsdf=data.frame(nbatkts)

tree01<-tree(post_tickets_flag~gender+agecat+emaildays+Division+domain+arenadis+tenure, data=nbatktsdf, na.action =
               na.exclude, mincut = 5, minsize = 10, mindev = 0.0001)
tree01
plot(tree01)
text(tree01)


tree03<-tree(post_tickets_flag~gender+agecat+emaildays+Division+domain+arenadis+tenure+pre_tickets_ever,
             data=nbatktsdf, na.action =na.exclude, mincut = 5, minsize = 10, mindev = 0.0001)
tree03
plot(tree03)
text(tree03)
cv.model03 <- cv.tree(tree03)
plot(cv.model03,xlim=c(1, 10))
cv.model03
pruned.tree03 <- prune.tree(tree03, best=5)
plot(pruned.tree03)
text(pruned.tree03)



tree05<-tree(post_tickets_flag~gender+agecat+emaildays+Division+domain+arenadis+tenure,
             data=nbatktsdf, na.action =na.exclude, mincut = 5, minsize = 10, mindev = 0.001)
tree05
plot(tree05)
text(tree05)

# cv.model <- cv.tree(tree.model)
# plot(cv.model)
# http://www.di.fc.ul.pt/~jpn/r/tree/tree.html
# cv.model.pruned <- prune.misclass(tree.model, best=best.size)
# summary(cv.model.pruned)
# pruned.tree <- prune.tree(tree.model, best=4)
# plot(pruned.tree)
# text(pruned.tree)


cv.model05 <- cv.tree(tree05)
plot(cv.model05)

tree06<-tree(post_tickets_flag~.-individual_id_str, data=nbatktsdf, na.action =
               na.exclude, mincut = 5, minsize = 10, mindev = 0.0001)
tree06
# plot(tree06)
# text(tree06)
cv.model06 <- cv.tree(tree06)
plot(cv.model06,xlim=c(1, 15))
pruned.tree06 <- prune.tree(tree06, best=8)
plot(pruned.tree06)
text(pruned.tree06)

tree07<-tree(post_tickets_flag~.-individual_id_str-pre_total_tickets_cat, data=nbatktsdf, na.action =
               na.exclude, mincut = 5, minsize = 10, mindev = 0.0001)
tree07
plot(tree07)
# text(tree07)
cv.model07 <- cv.tree(tree07)
plot(cv.model07,xlim=c(1, 15))
pruned.tree07 <- prune.tree(tree07, best=8)
pruned.tree07 
plot(pruned.tree07)
text(pruned.tree07)
                                                    