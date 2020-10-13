
setwd('C:/Users/60184/Desktop/STAT440/M1')
Sys.setenv(LANGUAGE = "en")


library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)


test = read.table('test2.txt',header = T,sep = ",",fill = T ,encoding = 'UTF-8')
train = read.table('train2.txt',header = T,sep = ",",fill = T ,encoding = 'UTF-8')

colnames(train)

###-GOALS
#levels issue in logestic regression
#random forest morel
#Make an ensemble out of logistic +RF model
#Submit to kaggle
#-baseline,logistic;RF;ensemble


##-removing d because we cannot get any training in d from training set



#Need to be transfer:
#1.Age:make the rank into medium(prob as lecture, add more)
#PCA to variables? variable selection
#2.Conformed date missed(only one), others to date(is it needed?)
#3.Separate the symptoms, get the number/separate the fever temperature
#How about getting the number of sympthoms? the temperature? 
# with higher temp might cause longer days until recover 

# length(unique(train$age))  #74
# length(unique(train$confirmed)) #38
# length(unique(train$sex))  #5
# length(unique(train$city))  #107
# length(unique(train$province))  #40
# length(unique(train$country))  #12
# length(unique(train$V1))  #11
# length(unique(train$countsym)) #6
# plot(train$mean)
# summary(test$mean)
# is.numeric(test$mean)


# train = cbind(as.numeric(train$age),train$sex,train$city,train$province,train$V1
#               ,train$V1,train$confirmed,train$symptoms,train$outcome,train$duration)
train$confirmed=as.Date(train$confirmed, "%d.%m.%Y")
test$confirmed=as.Date(test$confirmed,"%d.%m.%Y")
train$mean = as.numeric(train$age)
test$mean=as.numeric(test$age)

train$countsym = 'missing'
train$fever = NA
test$countsym = 'missing'
test$fever = NA

# mean(test$mean,na.rm = TRUE)


#See the mean, midterm and distribution of mean
# hist(train$mean)
# mean(train$mean)
# median(train$mean)
# 
# hist(test$mean)
# mean(test$mean,na.rm = TRUE)
# mdian(test$mean,na.rm=T)


# prof's code
#Data cleaning


for (i in 1:dim(train)[1]) {
  
  x = train$age[i]
  #Get the mean of age, filling average for missing value
  if (grepl('-', x, fixed = TRUE)) {
    z = unlist(strsplit(x, '-'))
    x = mean(as.numeric(c(z[[1]], z[[2]])))
    train$mean[i] = x
  }
  if (x==''){
    train$mean[i]=mean(train$mean,na.rm = T)
  }
  
  #Count of symptoms if exists
  y = train$symptoms[i]
  y1 = unlist(strsplit(y, '; '))
  if (length(y1)!=0){
    train$countsym[i] = length(y1)}
  
  #have fever or not
  train$fever[i] = grepl('fever', train$symptoms[i], fixed = T)
  
}


#Same for test dataset
for (i in 1:dim(test)[1]) {
  
  x = test$age[i]
  if (grepl('-', x, fixed = TRUE)) {
    z = unlist(strsplit(x, '-'))
    x = mean(as.numeric(c(z[[1]], z[[2]])))
    test$mean[i] = x
  }
  if (x==''){
    test$mean[i]=mean(train$mean,na.rm = T)
  }
  
  y = test$symptoms[i]
  y1 = unlist(strsplit(y, '; '))
  if (length(y1)!=0){
    test$countsym[i] = length(y1)}
  
  test$fever[i] = grepl('fever', test$symptoms[i], fixed = T)
  
}

#Removing NAs in test & training data
train<-na.omit(train)
test=na.omit(test)

train$countsym=as.character(train$countsym)
test$countsym=as.character(test$countsym)



# RMSE = function(a, b){
#   return(sqrt(mean((a - b)^2)))
# }
# 


# summary(model1)
# summary(model1)$r.squared
# summary(model1)$adj.r.squared
# 
# 
# summary(full)$r.squared
# summary(full)$adj.r.squared


# Prof's code
#Creating dummy variables into matrix
new.levels.country=unique(c(train$country))
new.levels.sex=unique(c(train$sex))
new.levels.V1=unique(c(train$V1))
new.levels.city=unique(c(train$city))
new.levels.province=unique(c(train$province))


d=length(new.levels.country)
d.sex=length(new.levels.sex)
d.V1=length(new.levels.V1)
d.city=length(new.levels.city)
d.province=length(new.levels.province)


n=dim(train)[1]
n.test=dim(test)[1]


train.countries=matrix(0,n,d)
test.countries=matrix(0,n.test,d)
train.sex=matrix(0,n,d.sex)
test.sex=matrix(0,n.test,d.sex)
train.V1=matrix(0,n,d.V1)
test.V1=matrix(0,n.test,d.V1)
train.city=matrix(0,n,d.city)
test.city=matrix(0,n.test,d.city)
train.province=matrix(0,n,d.province)
test.province=matrix(0,n.test,d.province)


for (i in 1:d){
  level=new.levels.country[i]
  train.countries[train$country == level,i ] = 1
  test.countries[test$country == level,i ] = 1

}

for (i in 1:d.sex){
  level.sex=new.levels.sex[i]
  train.sex[train$sex==level.sex,i]=1
  test.sex[test$sex==level.sex,i]=1
}

for (i in 1:d.V1){
  level.V1=new.levels.V1[i]
  train.V1[train$V1==level.V1,i]=1
  test.V1[test$V1==level.V1,i]=1
}

for (i in 1:d.city){
  level=new.levels.city[i]
  train.city[train$city == level,i ] = 1
  test.city[test$city == level,i ] = 1
  
}

for (i in 1:d.province){
  level=new.levels.province[i]
  train.province[train$province == level,i ] = 1
  test.province[test$province == level,i ] = 1
  
}


##Trying to add levels to the train data
#Failed

# lvls_expand(train$country,unique(paste(unique(train$country),unique(test$country))))
# unique(paste(unique(train$country),unique(test$country)))


######options(error=recover)   tell the position where the error occur



#Rename all dummy variables

colnames(test.countries)=c("C-59dcd" ,"C-16725", "C-c263d", "C-fb9d7", "C-d019a", "C-38fc4", "C-57155", "C-40f91", "C-3f760" ,"C-f99a5", "C-992f7", "C-61080")
colnames(train.countries)=c("C-59dcd" ,"C-16725", "C-c263d", "C-fb9d7", "C-d019a", "C-38fc4", "C-57155", "C-40f91", "C-3f760" ,"C-f99a5", "C-992f7", "C-61080")
colnames(train.sex)=c("S-8a467","S-d516d", "S-1e6ee", "S-38fc4","S-f7f7c")
colnames(test.sex)=c("S-8a467","S-d516d", "S-1e6ee", "S-38fc4","S-f7f7c")
colnames(test.V1)=c("V-35843", "V-d68ec", "V-b6ab9", "V-8dde4", "V-dd554", "V-10775", "V-b3641", "V-f9037", "V-fa873", "V-9a45a", "V-6a004")
colnames(train.V1)=c("V-35843", "V-d68ec", "V-b6ab9", "V-8dde4", "V-dd554", "V-10775", "V-b3641", "V-f9037", "V-fa873", "V-9a45a", "V-6a004")
colnames(test.province)=c(
 "P-89dd9", "P-fe869", "P-38fc4", "P-8ac5e", "P-d7cac", "P-87899", "P-c4d2f", "P-31991", "P-27e87", "P-a06a0", "P-94da6", "P-6d21d", "P-52611", "P-f163b", "P-55fe6", "P-7a0f6", "P-2c0f0", "P-b59f0",
 "P-c26a8", "P-72c53", "P-c9a98", "P-61aef", "P-bfb97", "P-3d44f", "P-ca1bd", "P-38d67", "P-37303", "P-2e628", "P-8b61f", "P-e900f", "P-731e6", "P-1cbcc", "P-06b0d", "P-e3bba", "P-c9d3e", "P-1c5d0",
 "P-8447c", "P-1eb71", "P-aed28"  )
colnames(train.province)=c(
  "P-89dd9", "P-fe869", "P-38fc4", "P-8ac5e", "P-d7cac", "P-87899", "P-c4d2f", "P-31991", "P-27e87", "P-a06a0", "P-94da6", "P-6d21d", "P-52611", "P-f163b", "P-55fe6", "P-7a0f6", "P-2c0f0", "P-b59f0",
  "P-c26a8", "P-72c53", "P-c9a98", "P-61aef", "P-bfb97", "P-3d44f", "P-ca1bd", "P-38d67", "P-37303", "P-2e628", "P-8b61f", "P-e900f", "P-731e6", "P-1cbcc", "P-06b0d", "P-e3bba", "P-c9d3e", "P-1c5d0",
  "P-8447c", "P-1eb71", "P-aed28"  )
colnames(train.city)=new.levels.city
colnames(test.city)=new.levels.city
train=cbind(train,train.V1,train.countries,train.sex,train.province,train.city)
test=cbind(test,test.V1,test.countries,test.sex,test.province,test.city)

# train=cbind(train,train.V1)
# test=cbind(test,test.V1)

# train=droplevels(train)
# test=droplevels(test)

# grepl('fever', train$symptoms[5], fixed = T)
# z = unlist(strsplit(train$symptoms[i], '; '))




#select the data to call just all variables apart from the origional ones. 
new=train[-c(1:6,8)]



full=lm(duration~.,data=new)
null=lm(duration~1,data = new)

# variable selection
# AIC
# forward,backword,stepwise
step(null, data=new, scope=list(lower=null, upper=full), direction="forward")
# forwrd:
# lm(formula = duration ~ confirmed + `P-38fc4` + `C-fb9d7` + `0b204` + 
     # `C-38fc4` + `P-aed28` + `V-10775` + `P-c4d2f` + `C-57155` + 
     # b4ff4 + cf966 + `44bc6` + `P-e3bba` + baec1 + `S-1e6ee` + 
     # e9282 + `2720c` + `9e542` + `1b112` + fbadb + `P-94da6` + 
     # b7422 + `80b09` + `P-3d44f` + `44886` + `38fc4` + `3e456` + 
     # `36567` + `02c49` + `2efa1` + `P-1c5d0` + `28d6a` + `5347f` + 
     # `V-fa873` + c4d2f + `S-f7f7c` + b1224 + d0d10 + `6d709` + 
     # `204ad` + `P-06b0d` + `1c1e6` + `V-dd554` + b3d59 + `247e5` + 
     # `V-b6ab9` + aa512 + `0bd76` + `3e121`, data = new)

step(full, data=new, direction="backward")
# backward:
# lm(formula = duration ~ confirmed + mean + countsym + fever + 
#      `V-35843` + `V-d68ec` + `V-b6ab9` + `V-8dde4` + `V-dd554` + 
#      `V-b3641` + `V-f9037` + `V-9a45a` + `C-59dcd` + `C-16725` + 
#      `C-c263d` + `C-fb9d7` + `C-d019a` + `C-38fc4` + `C-57155` + 
#      `C-3f760` + `S-1e6ee` + `S-38fc4` + `P-89dd9` + `P-fe869` + 
#      `P-38fc4` + `P-8ac5e` + `P-d7cac` + `P-87899` + `P-c4d2f` + 
#      `P-31991` + `P-27e87` + `P-a06a0` + `P-94da6` + `P-6d21d` + 
#      `P-52611` + `P-2c0f0` + `P-c26a8` + `P-72c53` + `P-bfb97` + 
#      `P-e900f` + `P-1cbcc` + `P-06b0d` + `P-1c5d0` + `44886` + 
#      `38fc4` + `6d709` + `89a35` + d7cac + c4d2f + `1c1e6` + `5ed5a` + 
#      aa512 + `2720c` + `44bc6` + b1224 + d0d10 + `0bd76` + `2efa1` + 
#      `308d3` + `1b112` + `4e8ba` + `204ad` + `0b204` + `3e121` + 
#      `1f408` + `3e456` + `02c49` + e4693 + `9b46e` + `5347f` + 
#      `36567` + fbadb + baec1 + b3d59 + `9e542` + ef709, data = new)

step(null, data=new,scope=list(upper=full), direction="both")
#stepwise: 
# lm(formula = duration ~ confirmed + `P-38fc4` + `C-fb9d7` + `0b204` + 
#      `C-38fc4` + `P-aed28` + `V-10775` + `P-c4d2f` + `C-57155` + 
#      b4ff4 + cf966 + `44bc6` + `P-e3bba` + baec1 + e9282 + `2720c` + 
#      `9e542` + `1b112` + fbadb + `P-94da6` + b7422 + `80b09` + 
#      `P-3d44f` + `44886` + `38fc4` + `3e456` + `36567` + `02c49` + 
#      `2efa1` + `P-1c5d0` + `28d6a` + `5347f` + `V-fa873` + c4d2f + 
#      `S-f7f7c` + b1224 + d0d10 + `6d709` + `204ad` + `1c1e6` + 
#      `V-dd554` + b3d59 + `247e5` + `V-b6ab9` + aa512 + `P-06b0d` + 
#      `0bd76` + `3e121`, data = new)



# BIC
step(null, data=new, scope=list(lower=null, upper=full), direction="forward",k=log(n))
# Forward
# lm(formula = duration ~ confirmed + `P-38fc4` + `C-fb9d7` + `0b204` + 
#      `C-38fc4` + `P-aed28` + `V-10775` + `P-c4d2f` + `C-57155` + 
#      b4ff4 + cf966 + `44bc6`, data = new)

step(full, data=new, direction="backward",k=log(n))
# Backword:
# lm(formula = duration ~ confirmed + `V-10775` + `C-16725` + `C-fb9d7` + 
#      `C-d019a` + `C-38fc4` + `C-57155` + `P-38fc4` + `P-94da6` + 
#      `P-6d21d` + `P-52611` + `P-c26a8` + `0b204`, data = new)

step(null, data=new,scope=list(upper=full), direction="both",k=log(n))
# Stepwise:
# lm(formula = duration ~ confirmed + `P-38fc4` + `C-fb9d7` + `0b204` + 
#      `C-38fc4` + `P-aed28` + `V-10775` + `P-c4d2f` + `C-57155` + 
#      b4ff4 + cf966 + `44bc6`, data = new)





#testing the imfortance
general1=lm(duration~ confirmed + countsym + fever+`C-59dcd` +`C-16725`+ `C-c263d`+ `C-fb9d7`+ `C-d019a`+ `C-38fc4`+`C-57155`
            +`C-40f91`+ `C-3f760` +`C-f99a5`+ `C-992f7`+ `C-61080`, data = train)
general2=lm(duration ~ confirmed + `P-38fc4` + `C-fb9d7` + `0b204` + `C-38fc4` + `P-aed28` + `V-10775` + `P-c4d2f` + `C-57155` +
            + b4ff4 + cf966 + `44bc6`, data = train)
general3


summary(general1)
summary(general2)


# pred = predict(lm(duration ~ confirmed + V6 + C2 + C4 + C5 + C6 + 
#                     C7 + P3 + P11 + P12 + P13 + P17 +P32+P33+ P19 + +`57`+`59`+`65`+`84`, train), new = test)




#(Use the predict function to predict the result, then export the output to txt.)

pred = predict(lm(duration ~  confirmed + countsym + fever + C1 + C2 + C3 + C4 + C5 + C6 + 
                    C7+C8+C9+C10, train), new = test)

kaggle=cbind(1:200,pred)
colnames(kaggle)=c('Id','duration')
write.table(kaggle, file = 'kaggle.txt', sep = ',', quote = F, row.name = FALSE)


#c:4.40--ts:4.59
#+s:4.41
#Only V:4,63
#C S V;4.57559







