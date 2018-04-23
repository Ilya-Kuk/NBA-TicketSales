## Importing Packages

library(tree)

## Data Preprocessing

#Reading in and storing dataset from file
nbatkts=read.csv("nbaTKTS4teams0412_sample.csv",header=T, sep=",")
attach(nbatkts)
nbatktsdf=data.frame(nbatkts)
#Creaging subset that my computer can handle
  match(nbatkts$post_tickets_flag,1)
  dataset <- nbatktsdf[c(1:100,601:700),]
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

#Creating new dataset using only the useful variables, as per data dictionary
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
View(new_dataset) #looks good except for column names
if(ncol(new_dataset)==length(Are_Variables)) colnames(new_dataset) <- Are_Variables
View(new_dataset) #looks good

#Removing other response variables post_ticket
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
attach(new_dataset)

#Growing Classification Tree

tree.nba <- tree(post_tickets_flag~., new_dataset)
