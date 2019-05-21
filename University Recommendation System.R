
yocketdata <- read.csv(file="C:\\Users\\Sukhmani\\Desktop\\admits.csv", header=TRUE, sep=",")
View(yocketdata)
summary(yocketdata)

drops <- c("major","specialization","program","department",
           "userProfileLink","topperCgpa","journalPubs","confPubs","ugCollege",
           "gmatA","gmatV","gmatQ","toeflEssay")
yocketdata <- yocketdata[ , !(names(yocketdata) %in% drops)]
View(yocketdata)
nrow(yocketdata)

################## removing all the NA values #######################

#yocketdata[!complete.cases(yocketdata),]
#nrow(yocketdata[!complete.cases(yocketdata),])
#yocketdata <- na.omit(yocketdata)

#apply(yocketdata, 1, function(x) sum(is.na(x)))

#View(yocketdata)

#yocketdata[,"greV"]

######################## Lookup function for verbal and quant ##############################

lookupgreV <- data.frame(greV= c(130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,
                                 145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
                                 160,161,162,163,164,165,166,167,168,169,170,200,210,220,230,
                                 240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,
                                 390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,
                                 540,550,560,570,580,590,600,610,620,630,640,650,660,670,680,
                                 690,700,710,720,730,740,750,760,770,780,790,800) ,
                         newgreV=(c(130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,
                                    145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
                                    160,161,162,163,164,165,166,167,168,169,170,130,130,130,130,
                                    131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,
                                    146,146,147,148,149,149,150,151,151,152,152,153,154,154,155,
                                    156,156,157,158,158,159,160,160,161,162,162,163,164,164,165,
                                    165,166,167,168,168,169,169,170,170,170,170,170)))
lookupgreV

lookupV <- unique(lookupgreV)
lookupV

outputV <- (merge(lookupV, yocketdata, by = 'greV'))

dropsV <- c("greV")
yocketdata1 <- outputV[ , !(names(outputV) %in% dropsV)]

lookupgreQ <- data.frame(greQ= c(130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,
                                 147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,
                                 164,165,166,167,168,169,170,200,210,220,230,240,250,260,270,280,290,
                                 300,310,320,330,340,350,360,370,380,390,400,410,420,430,440,450,460,
                                 470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,
                                 640,650,660,670,680,690,700,710,720,730,740,750,760,770,780,790,800) ,
                         newgreQ=(c(130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,
                                    147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,
                                    164,165,166,167,168,169,170,131,131,132,132,133,133,134,134,135,135,
                                    136,136,136,137,137,138,138,138,139,139,140,140,140,141,141,141,142,
                                    142,143,143,144,144,144,145,145,146,146,147,147,148,148,149,149,150,
                                    151,151,152,152,153,154,155,155,156,157,158,159,160,161,163,164,166)))
lookupgreQ

lookupQ <- unique(lookupgreQ)
lookupQ

outputQ <- (merge(lookupQ, yocketdata1, by = 'greQ'))

dropsQ <- c("greQ")
yocketdata2 <- outputQ[ , !(names(outputQ) %in% dropsQ)]

lookuptoeflScore <- data.frame(toeflScore= c(0,1,3,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,57,70,73,75,74,
                                             75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,
                                             93,94,95,96,97,98,99,100.101,102,103,104,105,106,107,
                                             108,109,110,111,112,113,114,115,116,117,118,119,120) ,
                               
                               newtoeflScore=(c(31,31,31,31,34,45,59,78,93,101,109,114,117,120,57,70,73,75,74,
                                                75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,
                                                93,94,95,96,97,98,99,100.101,102,103,104,105,106,107,
                                                108,109,110,111,112,113,114,115,116,117,118,119,120)))


lookuptoeflScore

lookuptoeflScore <- unique(lookuptoeflScore)
lookuptoeflScore

outputtoeflScore <- (merge(lookuptoeflScore, yocketdata2, by = 'toeflScore'))

dropstoeflScore <- c("toeflScore")

yocketdata3 <- outputtoeflScore[ , !(names(outputtoeflScore) %in% dropstoeflScore)]

View(yocketdata3)
yocket_final <- yocketdata3[c(4,5,6,1,7,3,2,8,9,10,11,12,13)]





################################ Clean up CPGA ######################

for (i in 1:43523) {
  if(yocket_final[i,"cgpaScale"] == 0 & yocket_final[i,"cgpa"]>0) {
    yocket_final[i, "cgpaScale"] = 10
  }
  if(yocket_final[i,"cgpaScale"] == 4 & yocket_final[i,"cgpa"]>4) {
    yocket_final[i, "cgpaScale"] = 10
  }
  if(yocket_final[i,"cgpaScale"] == 5 & yocket_final[i,"cgpa"]>5) {
    yocket_final[i, "cgpaScale"] = 10
  }}

for (i in 1:43523) {
  if(yocket_final[i,"cgpaScale"] == 10 & yocket_final[i,"cgpa"]>10) {
    yocket_final[i, "cgpaScale"] = 100
  }}

for (i in 1:43523){
  if(yocket_final[i,"cgpaScale"] == 100 & yocket_final[i,"cgpa"]>100) {
    yocket_final[i, "cgpa"] = 100
  }}

for (i in 1:43523) {
  if(yocket_final[i,"cgpa"]==0) {
    yocket_final[i, "cgpaScale"] = 0
  }}

for (i in 1:43523) {
  if(yocket_final[i,"cgpaScale"] == 100 & yocket_final[i,"cgpa"]>0 & yocket_final[i,"cgpa"]<4.01) {
    yocket_final[i, "cgpaScale"] = 4
  }
  if(yocket_final[i,"cgpaScale"] == 100 & yocket_final[i,"cgpa"]>4 & yocket_final[i,"cgpa"]<5.01) {
    yocket_final[i, "cgpaScale"] = 5
  }
  if(yocket_final[i,"cgpaScale"] == 100 & yocket_final[i,"cgpa"]>5 & yocket_final[i,"cgpa"]<10.01) {
    yocket_final[i, "cgpaScale"] = 10
  }}

###################################Convert everything to a 4-scale CGPA############
for (i in 1:43523) {
  if(yocket_final[i,"cgpaScale"] == 10) {
    yocket_final[i, "cgpa"] = yocket_final[i,"cgpa"]/2.5
  }}

for (i in 1:43523) {
  if(yocket_final[i,"cgpaScale"] == 100) {
    yocket_final[i, "cgpa"] = yocket_final[i,"cgpa"]/25
  }}

for (i in 1:43523) {
  if(yocket_final[i,"cgpaScale"] == 5) {
    yocket_final[i, "cgpa"] = yocket_final[i,"cgpa"]/1.25
  }}

#dropscgpa <- c("X1","X1_1","X2_2")
#yocket_final1 <- yocket_final[ , !(names(yocket_final) %in% dropscgpa)]
write.csv(yocket_final, file = "yocket_final.csv")



##########################Removing the term and username columns ############################## 

yocketfinal1<-yocket_final[,-9]
yocketfinal1<-yocketfinal1[,-1]
#Getting the structure
str(yocketfinal1)
View(yocketfinal1)
#Converted the admit attribute to categorical
yocketfinal1$admit<-as.factor(yocketfinal1$admit)

##Normalize 
normalize <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}
#remove the admit and UniName as they are categorical values and we will later add them
yocketfinal2<-yocketfinal1[,-(10:11)]

#Apply normalization on dataset
yocket_n <- as.data.frame(lapply(yocketfinal2,normalize))
#We remove GRE(A) because major chunk of the data was NA
yocket_n<-yocket_n[,-7]
#add admit and Uniname in the normalized dataset
yocket_n$admit<-yocketfinal1$admit
yocket_n$uniName<-yocketfinal1$univName
View(yocket_n)

#But for performing the logistic and knn we dont need University name
yocket_n1<-yocket_n[,-10]
yocket_n1$u<-yocket_n$uniName


#install.packages("dummies")
#library(dummies)

#yocket_n1 <- cbind(yocket_n, dummy(yocket_n$uniName, sep = "_"))
#View(yocket_n1)
#yocket_n1<-yocket_n1[,-11]
#View(yocket_n1)

#Generate the accuracy
accuracy <- function(x)
{
  num <- x[1,1] + x[2,2]
  den <- x[1,1] + x[2,2]+x[2,1]+x[1,2]
  return(num/den)
  
}

#-------------------------------------------------------------------------------------------------------------------------------------------------
#Full Logistic Model
#--------------------------------------------------------------------------------------------------------------------------------------------------

log<-function()
{ 
  id<-which(yocket_n1$u=="University of North Carolina Charlotte")
  tryy<-yocket_n1[id,]
  
  tryy<-tryy[,-10]
  
  set.seed(123)
  id<-sample(nrow(tryy),nrow(tryy)*0.75)
  train<-tryy[id,]
  test<-tryy[-id,]
  
  model<-glm(admit~.,data=train,family="binomial")
  summary(model)
  
  #Prediction
  predicts= predict(model,test,type = "response")
  predicts = ifelse(predicts <0.5,0,1)
  
  
  acc_log<-table(predicts,test$admit)
  a<-accuracy(acc_log)
  return(a)
}
lresult<-replicate(n = 20, expr =log())
AVGl<-mean(lresult)
AVGl



#-----------------------------------------------------------------------------------
##Decision Trees
#-----------------------------------------------------------------------------------
library(party)

decision_tree<-function()
{
  id<-which(yocket_n1$u=="University of North Carolina Charlotte")
  tryy<-yocket_n1[id,]
  
  tryy<-tryy[,-10]
  
  set.seed(123)
  id<-sample(nrow(tryy),nrow(tryy)*0.75)
  train<-tryy[id,]
  test<-tryy[-id,]
  tree.model<-ctree(admit~.,data=train)
  
  tree_result<-predict(tree.model,newdata=test)
  table_tree<-table(tree_result,test$admit)
  tree_acc<-accuracy(table_tree)
  return(tree_acc)
  
}

Dresult<-replicate(n = 20, expr =decision_tree())
AVG<-mean(Dresult)
AVG


#-------------------------------------------------------------------------------------------------
##Random Forest
#-------------------------------------------------------------------------------------------------
library(randomForest)

random_forest<-function()
{
  id<-which(yocket_n1$u=="University of North Carolina Charlotte")
  tryy<-yocket_n1[id,]
  
  tryy<-tryy[,-10]
  
  set.seed(123)
  id<-sample(nrow(tryy),nrow(tryy)*0.75)
  train<-tryy[id,]
  test<-tryy[-id,]
  
  rf<-randomForest(admit~.,data=train,ntree=500)
  pr_rf<-predict(rf,newdata=test)
  rf_table<-table(pr_rf,test$admit)
  acc<-accuracy(rf_table)
  
  return(acc)
  
}


rf_result<-replicate(n = 20, expr =random_forest())
AVG_rf<-mean(rf_result)
AVG_rf



#----------------------------------------------------------------------------------------------------------------------------
##Clustering:We did clustering to find what all similar profiles exist that have got admits ,so that the student can connect with these people
#-----------------------------------------------------------------------------------------------------------------------------
#Now take in only that dataset that has admit=1

id<-which(yocket_n$admit==1)
accept_yocket<-yocket_n[id,]


#for clustering we dont need column 9(Admit) and 10(University Name)
try<-accept_yocket[,-(9:10)]

try<-scale(try)


km.out=kmeans(try,5,nstart=20)

plot(try,col=(km.out$cluster+1), main="K-Means Clustering Results with K=5", xlab="", ylab="", pch=20,cex=2)

accept_yocket$cluster<-km.out$cluster


##--------------------------------------------------------------------------------------------
#Generate the tier:Tell which tier colleges the person should apply to
##--------------------------------------------------------------------------------------------
University<-unique(accept_yocket$uniName)
uni<-as.data.frame(University)

s<-sample(1:4,54,replace = TRUE)
uni$tier<-s

id<-which(uni$tier==1)
Tier1<-uni$University[id]
id2<-which(uni$tier==2)
Tier2<-uni$University[id2]
id3<-which(uni$tier==3)
Tier3<-uni$University[id3]
id4<-which(uni$tier==4)
Tier4<-uni$University[id4]

accept_yocket$Tier<-0
accept_yocket[accept_yocket$uniName %in% Tier1, ]$Tier=1
accept_yocket[accept_yocket$uniName %in% Tier2, ]$Tier=2
accept_yocket[accept_yocket$uniName %in% Tier3, ]$Tier=3
accept_yocket[accept_yocket$uniName %in% Tier4, ]$Tier=4

#-------------------------------------------------------------------------------------------------
##Multinomial Logistic Regression to predict which tier University to opt fro
#-------------------------------------------------------------------------------------------------
set.seed(234)
id<-sample(nrow(accept_yocket),nrow(accept_yocket)*0.75)
train<-accept_yocket[id,]
test<-accept_yocket[-id,]

library(nnet)
tier_model<-multinom(Tier~., data = train)
a<-predict(tier_model,newdata=test[,-12])
result<-table(a,test$Tier)
Tier_acc<-accuracy(result)





