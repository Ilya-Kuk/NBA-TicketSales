d <- read.csv("NBAsample2Groups.csv",header=T, sep=",")
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
a
grepl("ax_",a) #no ax_ variables
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

