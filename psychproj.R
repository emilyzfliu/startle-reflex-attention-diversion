library(readxl)
library(dplyr)
library(beanplot)
library(hash)
library(pracma)
library(survival)
library(ggplot2)
library(tableone)
library(oddsratio)
library(corrplot)
library(MASS)

cols <- c("ID", "gender", "age", "age_cat", "extra", "ex_cat")

x <- data.frame(matrix(ncol=6, nrow=60))

colnames(x) <- cols

x$ID <- c(1:60)
x$gender <- c(rep("M", 30), rep("F", 30))
x$age <- c(22,16,21,16,16,22,21,20,19,19,20,21,22,17,18,21,16,17,16,22,18,22,21,21,21,19,19,18,
           20,20,20,21,19,19,17,20,21,18,16,18,20,16,16,19,18,17,20,18,22,17,19,19,17,22,22,17,18,17,16,17)
x$extra <- c(41,45,23,48,47,18,54,38,77,99,44,31,23,40,50,96,87,33,47,28,67,71,6,53,81,22,31,2,69,30,37,95,29,31,1,84,
            77,4,61,30,44,66,25,65,92,55,57,66,55,41,26,32,67,38,64,84,29,5,67,6)

n <- c()
for (i in x$age) {
  if (i==16 | i==17) {
    n <- c(n, "young")
  }
  else if (i==18 | i==19 | i==20) {
    n <- c(n, "medium")
  }
  else if (i==21 | i==22) {
    n <- c(n, "old")
  }
  else {n<- c (n, "BLEAGH")}
}
x$age_cat <- n

n <- c()
for (i in x$extra) {
  if (i>=0 & i<=19) {
    n <- c(n, "very low")
  }else if (i>=20 & i<=39) {
    n <- c(n, "low")
  }else if (i>=40 & i<=59) {
    n <- c(n, "medium")
  }else if (i>=60 & i<=79) {
    n <- c(n, "high")
  }else if (i>=80 & i<=99) {
    n <- c(n, "very high")
  }
  else {n<- c (n, "BLEAGH")}
}
x$ex_cat<-n



ex_1 <- x[which(x$ex_cat=="very low"),]
ex_2 <- x[which(x$ex_cat=="low"),]
ex_3 <- x[which(x$ex_cat=="medium"),]
ex_4 <- x[which(x$ex_cat=="high"),]
ex_5 <- x[which(x$ex_cat=="very high"),]

#7, 18, 15, 12, 8
nrow (ex_1)
nrow (ex_2)
nrow (ex_3)
nrow (ex_4)
nrow (ex_5)

ag_1 <- x[which (x$age_cat=="young"),]
ag_2 <- x[which (x$age_cat=="medium"),]
ag_3 <- x[which (x$age_cat=="old"),]

#18, 25, 17
nrow (ag_1)
nrow (ag_2)
nrow (ag_3)

ex_mod <- rbind (ex_1, ex_2, ex_3, ex_4, ex_5)

rownames(ex_mod) <- c(1:60)

grp_0 <- data.frame(matrix(ncol=6, nrow=0)) #no attention diversion -- CONTROL
grp_1 <- data.frame(matrix(ncol=6, nrow=0)) #mild attention diversion
grp_2 <- data.frame(matrix(ncol=6, nrow=0)) #intense attention diversion

colnames (grp_0)<- cols
colnames (grp_1)<- cols
colnames (grp_2) <- cols

count <- 0

for (i in 1:nrow(ex_mod)) {
  print(i)
  print(ex_mod[i,])
  if (count%%3==0) {
    grp_0<-rbind (grp_0, ex_mod[i,])
  }
  else if (count%%3==1) {
    grp_1<-rbind (grp_1, ex_mod[i,])
  }
  else if (count%%3==2) {
    grp_2<-rbind (grp_2, ex_mod[i,])
  }
  count <- count+1
}

View (grp_0)
View (grp_1)
View (grp_2)

hist(x$extra, xlab="Percentile Extraversion", col="paleturquoise", main="Percentile Extraversion Distribution")


cat_names <-c("Very Low\n0-19", "Low\n20-39", "Medium\n40-59", "High\n60-79", "Very High\n80-99")
extra_cat <- c(nrow (ex_1), nrow (ex_2), nrow (ex_3), nrow (ex_4), nrow (ex_5))
barplot(extra_cat, main="Extraversion Distribution by Category", ylab="Frequency", 
        ylim=c(0, 20), xlab="Category", names.arg=cat_names,
        col="paleturquoise")

mean_vals <- 
  c(0.170, 0.160, 0.151, 0.139, 0.130, 0.271, 0.260, 0.249, 0.239, 0.230, 0.303, 0.290, 0.280, 0.270, 0.260)

barplot(matrix(mean_vals, nr=5), beside=T,
        col=c("coral1", "lightsalmon", "lemonchiffon", "mediumseagreen", "lightblue"), 
        names.arg=c("No Attention Diversion", "Mild Attention Diversion", "Intense Attention Diversion"),
        main=("Attention Diversion and Mean Skin Conductance Response (SCR)"),
        xlab="Degree of Attention Diversion", ylab="Mean SCR Magnitude (√ΔµS)")
