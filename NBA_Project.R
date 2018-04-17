#Reading in and storing dataset from file
nbatkts=read.csv("nbaTKTS4teams0412_sample.csv",header=T, sep=",")
attach(nbatkts)
nbatktsdf=data.frame(nbatkts)
dataset <- nbatktsdf

#Removing unwanted variables, as per Data Dictionary
data_dictionary <- read.csv("Copy_of_Ticket_Development_Sample_Data_Dictionary2.csv", header=TRUE, sep=",")
select.Variable <- data_dictionary[,c(2,3)]
variables_used <- select.Variable[select.Variable[,1]==1,]
variables_used <- na.omit(variables_used)
variables_used <- variables_used[,2]
variables_used <- variables_used[-c(3,4,5,6,7,8,9,10,11,59)] #Removing tenure variables, as requested
variables_used <- variables_used[-c(6:23,63:80)] #Removing re-coded variables
variables_used <- as.character(variables_used)

#Creating new dataset using only the useful variables
all_variables <- colnames(dataset)
first_col <- rep(NA, times=nrow(dataset))
new_dataset <- data.frame(first_col)
for(i in 1:length(variables_used)){
  V <- (variables_used)[i]
  J <- match(V,all_variables)
  new_dataset <- cbind(new_dataset,dataset[,J])
  colnames(new_dataset)[i+1] <- V
}

cy <- post_tickets_flag
