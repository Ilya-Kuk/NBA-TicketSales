#Reading in and storing dataset 'd' from file
d <- read.csv("nbasample2GrpPlus0429.csv",header=T, sep=",")
attach(d)
#Creating vector checking to see if values are the same
x <- post_tickets_flag==post_ticket_response
#Making vector into factor to see if there are two levels (TRUE *and* FALSE, or just TRUE)
factor(x)
print("There are only TRUE values in this vector, so all the values of the two response variables post_tickets_flag and post_ticket_response are identical.")