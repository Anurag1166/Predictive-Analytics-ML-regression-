getwd()
setwd("C:/Jig16520")
data<-read.csv("C://Jig16520//data.csv",stringsAsFactors = T)
tet<-read.csv("C://Jig16520//test2",stringsAsFactors = T)
options(scipen = 999)
names(data)




library(dplyr)         #To Work With DATAFRAMES
library(gains)         #Gains tables,Chart & Lift Chart for prediction Algorithm
library(irr)           #Kappa Matrix;Model Accuracy algorithm
library(ROCR)          #ROCR CURVE,Cutoff Parameterized performance Curve Design
library(caret)         #Confusion Matrix:Regression and Classification Training
library(car)           #Multicollinearity
library(VIF)



names(data)
str(data)
summary(data)

####***********************MAKING DATA QUALITY REPORT MANUALLY**************************####

#Taking Names of Variables
variable<-names(data)
qr<-as.data.frame(variable)


####************************Taking Data Type of Each Variable****************************####
qr$DataType<-sapply(data,class)

##**************************Number of Records for Each*************************************##
qr$No.ofRecords<-nrow(data)

##**************************Number of Unique Values****************************************##
for(i in 1:ncol(data))
{
  qr$Unique_Records[i]<-length(unique(data[,i]))
}
##**************************Number of observations for each variable percentage*************##
qr$DataAvailable<-colSums(!is.na(data))
qr$Available_Percent<-round(colMeans(!is.na(data)),4)
#round(qr$Available_Percent)
##**************************Sum & % of Missing Values***************************************##
qr$Missing<-colSums(is.na(data))
qr$Missing_Percent<-round(colMeans(is.na(data)),4)

##**************************Max,Min,Values,Quantiles for Each Variable**********************##
for(i in 1:ncol(data))
{
   qr$Maximum[i]                  <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",max(data[,i],na.rm=T),0),2)
   qr$Min[i]                      <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",min(data[,i],na.rm=T),0),2)
   qr$Mean[i]                     <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",mean(data[,i],na.rm=T),0),2)
   qr$fifth_percentile[i]         <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.05,na.rm=T),0),2)
   qr$tenth_percentile[i]         <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.10,na.rm=T),0),2)
   qr$Twenty_fifth_percentile[i]  <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.25,na.rm=T),0),2)
   qr$fiftyth_percentile[i]       <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.50,na.rm=T),0),2)
   qr$Seventy_fifth_percentile[i] <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.75,na.rm=T),0),2)
   qr$Ninetyth_percentile[i]      <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.90,na.rm=T),0),2)
   qr$Nenety_fifth_percentile[i]  <-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.95,na.rm=T),0),2)
}
str(qr)
##**Now We will Export whole Data in to a CSV**##
#write.csv(qr,"test2.CSV",row.names = T)
           
#Anomolys/Missing Value tratment of var,"retdays" & Making Dummy Variable
summary(data$retdays)
sort(unique(data$retdays),na.last = F)
data$retdays_n<-ifelse(is.na(data$retdays)==TRUE,0,1)
str(data$retdays_n)
summary(data$retdays_n)
head(data$retdays_n)

#As We know we have to OMITT those Variables which more then 15% Missing Values and can make New Compatible DB
#[telds<=Telecom Dataset]
telds<-data[,colMeans(is.na(data))<=0.15]
#telds$retdays_n
#As Given in Data Dictionory drop_blk_Meanes is made by
#blck_dat_Mean+BLCK_VCE_MEAN+DROP_DAT_MEAN+DROP_VCE_MEAN
#So We Can omitting Variable blck_dat_Mean
names(telds)
#So From Below Command We can See Position of our Current Target Variable
data.frame(colnames(telds))
#Now We Will Omitt That
telds<-telds[,-50]
#Check Again
data.frame(colnames(telds))


##******************************************DATA EXPLORATION ***********************************************##
##************************VARIABLE PROFILING OF CONTINUES & CATEGORICAL VARIABLE ****************************##
##*****************************ON THE BASIS OF TARGET VARIABLE WE WILL DECILE*********************************##

names(telds)
str(telds)
#{1}Var_Name=>'mou_Mean'
summary(telds$mou_Mean)
da1<-telds%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da1$N<-unclass(telds%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da1$Churn_perc<-da1$n/da1$N
da1$Greater_Than<-unclass(telds%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
da1$Less_Than<-unclass(telds%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
da1$Variable<-rep("mou_Mean",nrow(da1))


#{2}Var_Name=>'totmrc_Mean'
summary(telds$totmrc_Mean)
da2<-telds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da2$N<-unclass(telds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da2$Churn_perc<-da2$n/da2$N
da2$Greater_Than<-unclass(telds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
da2$Less_Than<-unclass(telds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
da2$Variable<-rep("totmrc_Mean",nrow(da2))


#{3}Var_Name=>'rev_Range'
summary(telds$rev_Range)
da3<-telds%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
da3$N<-unclass(telds%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
da3$Churn_perc<-da3$n/da3$N
da3$Greater_Than<-unclass(telds%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
da3$Less_Than<-unclass(telds%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
da3$Variable<-rep("rev_Range",nrow(da3))


#{4}Var_Name=>'mou_Range'
summary(telds$mou_Range)
da4<-telds%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
da4$N<-unclass(telds%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
da4$Churn_perc<-da4$n/da4$N
da4$Greater_Than<-unclass(telds%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
da4$Less_Than<-unclass(telds%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
da4$Variable<-rep("mou_Range",nrow(da4))


#{5}Var_Name=>'change_mou'
summary(telds$change_mou)
da5<-telds%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
da5$N<-unclass(telds%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
da5$Churn_perc<-da5$n/da5$N
da5$Greater_Than<-unclass(telds%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
da5$Less_Than<-unclass(telds%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
da5$Variable<-rep("change_mou",nrow(da5))



#{6}Var_Name=>'drop_blk_Mean'
summary(telds$drop_blk_Mean)
da6<-telds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da6$N<-unclass(telds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da6$Churn_perc<-da6$n/da6$N
da6$Greater_Than<-unclass(telds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
da6$Less_Than   <-unclass(telds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
da6$Variable<-rep("drop_blk_Mean",nrow(da6))


#{7}Var_Name=>'drop_vce_Mean'
summary(telds$drop_vce_Range)
da7<-telds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
da7$N<-unclass(telds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
da7$Churn_perc<-da7$n/da7$N
da7$Greater_Than<-unclass(telds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
da7$Less_Than<-unclass(telds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
da7$Variable<-rep("drop_vce_Range",nrow(da7))

#{8}Var_Name=>'owylis_vce_Range'
summary(telds$owylis_vce_Range)
da8<-telds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
da8$N<-unclass(telds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
da8$Churn_perc<-da8$n/da8$N
da8$Greater_Than<-unclass(telds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
da8$Less_Than<-unclass(telds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
da8$Variable<-rep("owylis_vce_Range",nrow(da8))


#{9}Var_Name=>'mou_opkv_Range'
summary(telds$mou_opkv_Range)
da9<-telds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
da9$N<-unclass(telds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
da9$Churn_perc<-da9$n/da9$N
da9$Greater_Than<-unclass(telds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
da9$Less_Than<-unclass(telds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
da9$Variable<-rep("mou_opkv_Range",nrow(da9))

#{10}Var_Name=>'months'
summary(telds$months)
da10<-telds%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)
da10$N<-unclass(telds%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
da10$Churn_perc<-da10$n/da10$N
da10$Greater_Than<-unclass(telds%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
da10$Less_Than<-unclass(telds%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
da10$Variable<-rep("months",nrow(da10))


#{11}Var_Name=>'totcalls'
summary(telds$totcalls)
da11<-telds%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)
da11$N<-unclass(telds%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
da11$Churn_perc<-da11$n/da11$N
da11$Greater_Than<-unclass(telds%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
da11$Less_Than<-unclass(telds%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
da11$Variable<-rep("totcalls",nrow(da11))


#{12}Var_Name=>'eqpdays'
#Missing Value treatment & Decing on Basis of CHURN.
summary(telds$eqpdays)
To_Remove<-which(is.na(telds$eqpdays))
telds<-telds[-To_Remove,]
#Now We Decile on Basis of CHURN
da12<-telds%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)
da12$N<-unclass(telds%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
da12$Churn_perc<-da12$n/da12$N
da12$Greater_Than<-unclass(telds%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
da12$Less_Than<-unclass(telds%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
da12$Variable<-rep("eqpdays",nrow(da12))


#{13}Var_Name=>'custcare_Mean' #Getting Less Deciles so omitt.....#
summary(telds$custcare_Mean)
dat13<-telds%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
dat13$varname<-rep("custcare_Mean",nrow(dat13))
#plot(telds$churn,telds$custcare_Mean,col="purple")

#{14}Var_Name=>'callwait_Mean'
da14<-telds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da14$N<-unclass(telds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
da14$Churn_perc<-da14$n/da14$N
da14$Greater_Than<-unclass(telds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
da14$Less_Than<-unclass(telds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
da14$Variable<-rep("callwait_Mean",nrow(da14))


#{15}Var_Name=>'iwylis_vce_Mean'
da15<-telds%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)
da15$N<-unclass(telds%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
da15$Churn_perc<-da15$n/da15$N
da15$Greater_Than<-unclass(telds%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
da15$Less_Than<-unclass(telds%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
da15$Variable<-rep("iwylis_vce_Mean",nrow(da15))

#{16}Var_Name=>'callwait_Range' #OMITT
summary(telds$callwait_Range)
da16<-telds%>%mutate(dec=ntile(callwait_Range,n=6))%>%count(churn,dec)%>%filter(churn==1)
da16$Variable<-rep("callwait_Range",nrow(da16))

#{17}Var_Name=>'ccrndmou_Range' #OMITT
summary(telds$ccrndmou_Range)
da17<-telds%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
da17$Variable<-rep("ccrndmou_Range",nrow(da17))

#{18}Var_Name=>'adjqty' 
summary(telds$adjqty)
da18<-telds%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
da18$N<-unclass(telds%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
da18$Churn_perc<-da18$n/da18$N
da18$Greater_Than<-unclass(telds%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
da18$Less_Than<-unclass(telds%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
da18$Variable<-rep("adjqty",nrow(da18))

#{19}Var_Name=>'ovrrev_Mean' #OMITT
summary(telds$ovrrev_Mean)
da19<-telds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da19$N<-unclass(telds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
da19$Churn_perc<-da19$n/da19$N
da19$Greater_Than<-unclass(telds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
da19$Less_Than<-unclass(telds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
da19$Variable<-rep("ovrrev_Mean",nrow(da19))

#{20}Var_Name=>'rev_Mean'
summary(telds$rev_Mean)
da20<-telds%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da20$N<-unclass(telds%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da20$Churn_perc<-da20$n/da20$N
da20$Greater_Than<-unclass(telds%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
da20$Less_Than<-unclass(telds%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
da20$Variable<-rep("rev_Mean",nrow(da20))

#{21}Var_Name=>'ovrmou_Mean'
summary(telds$ovrmou_Mean)
da21<-telds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da21$N<-unclass(telds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
da21$Churn_perc<-da21$n/da21$N
da21$Greater_Than<-unclass(telds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
da21$Less_Than<-unclass(telds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
da21$Variable<-rep("ovrmou_Mean",nrow(da21))

#{22}Var_Name=>'comp_vce_Mean'**TRANSFORM & DELETE**
summary(telds$comp_vce_Mean)
da22<-telds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da22$N<-unclass(telds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da22$Churn_perc<-da22$n/da22$N
da22$Greater_Than<-unclass(telds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
da22$Less_Than<-unclass(telds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
da22$Variable<-rep("comp_vce_Mean",nrow(da22))

#{23}Var_Name=>'plcd_vce_Mean'**TRANSFORM & DELETE**
summary(telds$plcd_vce_Mean)
da23<-telds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da23$N<-unclass(telds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da23$Churn_perc<-da23$n/da23$N
da23$Greater_Than<-unclass(telds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
da23$Less_Than<-unclass(telds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
da23$Variable<-rep("plcd_vce_Mean",nrow(da23))


#{24}Var_Name=>'avg3mou'
summary(telds$avg3mou)
da24<-telds%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
da24$N<-unclass(telds%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
da24$Churn_perc<-da24$n/da24$N
da24$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
da24$Less_Than<-unclass(telds%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
da24$Variable<-rep("avg3mou",nrow(da24))

#{25}Var_Name=>'avgmou'
summary(telds$avgmou)
da25<-telds%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
da25$N<-unclass(telds%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
da25$Churn_perc<-da25$n/da25$N
da25$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
da25$Less_Than<-unclass(telds%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
da25$Variable<-rep("avgmou",nrow(da25))

#{26}Var_Name=>'avg3qty'
summary(telds$avg3qty)
da26<-telds%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
da26$N<-unclass(telds%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
da26$Churn_perc<-da26$n/da26$N
da26$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
da26$Less_Than<-unclass(telds%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
da26$Variable<-rep("avg3qty",nrow(da26))

#{27}Var_Name=>'avgqty'
summary(telds$avgqty)
da27<-telds%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
da27$N<-unclass(telds%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
da27$Churn_perc<-da27$n/da27$N
da27$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
da27$Less_Than<-unclass(telds%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
da27$Variable<-rep("avg3qty",nrow(da27))

#{28}Var_Name=>'avg6mou'
summary(telds$avg6mou)
da28<-telds%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
da28$N<-unclass(telds%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
da28$Churn_perc<-da28$n/da28$N
da28$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
da28$Less_Than<-unclass(telds%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
da28$Variable<-rep("avg6mou",nrow(da28))


#{29}Var_Name=>'avg6qty'
summary(telds$avg6qty)
da29<-telds%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
da29$N<-unclass(telds%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
da29$Churn_perc<-da29$n/da29$N
da29$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
da29$Less_Than<-unclass(telds%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
da29$Variable<-rep("avg6qty",nrow(da29))

#{30}Var_Name=>'age1' #Can Be Used As Factor
summary(telds$age1)
da30<-telds%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)
da30$N<-unclass(telds%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
da30$Churn_perc<-da30$n/da30$N
da30$Greater_Than<-unclass(telds%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
da30$Less_Than<-unclass(telds%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
da30$Variable<-rep("age1",nrow(da30))


#{31}Var_Name=>'age2' #Getting Less Deciles But Can Be Used As Factor
summary(telds$ccrndmou_Range)
da31<-telds%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)
da31$Variable<-rep("age2",nrow(da31))

#{32}Var_Name=>'age2' #Getting Less Deciles But Can Be Used As Factor
summary(telds$models)
da32<-telds%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)
da32$Variable<-rep("models",nrow(da32))

#{33}Var_Name=>'hnd_price' #Can Be Used As Factor
summary(telds$hnd_price)
da33<-telds%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)
da33$N<-unclass(telds%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
da33$Churn_perc<-da33$n/da33$N
da33$Greater_Than<-unclass(telds%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
da33$Less_Than<-unclass(telds%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
da33$Variable<-rep("hnd_price",nrow(da33))

#{34}Var_Name=>'actvsubs' #Getting Less Deciles But Can Be Used As Factor
summary(telds$actvsubs)
da34<-telds%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)
da34$Variable<-rep("actvsubs",nrow(da34))

#{35}Var_Name=>'actvsubs' #Getting Less Deciles But Can Be Used As Factor
summary(telds$uniqsubs)
da35<-telds%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)
da35$Variable<-rep("uniqsubs",nrow(da34))


#{36}Var_Name=>'forgntvl' #Getting Less Deciles But Can Be Used As Factor
summary(telds$forgntvl)
da36<-telds%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)
da36$Variable<-rep("forgntvl",nrow(da36))

#{37}Var_Name=>'opk_dat_Mean' #Getting Less Deciles #OMITT MACHA
summary(telds$opk_dat_Mean)
da37<-telds%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da37$Variable<-rep("opk_dat_Mean",nrow(da37))

#{38}Var_Name=>'mtrcycle' #Getting Less Deciles #Factor
summary(telds$mtrcycle)
da38<-telds%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)
da38$Variable<-rep("mtrcycle",nrow(da38))

#{39}Var_Name=>'truck' #Getting Less Deciles #Factor
summary(telds$truck)
da39<-telds%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)
da39$Variable<-rep("truck",nrow(da39))

#{40}Var_Name=>'roam_Mean' #Getting Less Deciles #OMITT
summary(telds$roam_Mean)
da40<-telds%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da40$Variable<-rep("roam_Mean",nrow(da40))

#{41}Var_Name=>'recv_sms_Mean' #Getting Less Deciles #OMITT
summary(telds$recv_sms_Mean)
da41<-telds%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da41$Variable<-rep("recv_sms_Mean",nrow(da41))

#{42}Var_Name=>'mou_pead_Mean' #Getting Less Deciles #OMITT
summary(telds$mou_pead_Mean)
da42<-telds%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da42$Variable<-rep("mou_pead_Mean",nrow(da42))


#{43}Var_Name=>'da_Mean' 
summary(telds$da_Mean)
da43<-telds%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da43$N<-unclass(telds%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
da43$Churn_perc<-da43$n/da43$N
da43$Greater_Than<-unclass(telds%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
da43$Less_Than<-unclass(telds%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
da43$Variable<-rep("da_Mean",nrow(da43))

#{44}Var_Name=>'da_Range' 
summary(telds$da_Range)
da44<-telds%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
da44$N<-unclass(telds%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
da44$Churn_perc<-da44$n/da44$N
da44$Greater_Than<-unclass(telds%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
da44$Less_Than<-unclass(telds%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
da44$Variable<-rep("da_Range",nrow(da44))

#{45}Var_Name=>'datovr_Mean' #Omitt
summary(telds$datovr_Mean)
da45<-telds%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da45$Variable<-rep("datovr_Mean",nrow(da45))

#{46}Var_Name=>'datovr_Range' #Omitt
summary(telds$datovr_Range)
da46<-telds%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
da46$Variable<-rep("datovr_Range",nrow(da46))

#{47}Var_Name=>'datovr_Range' #Omitt #Sooo Many Zeros are there
summary(telds$drop_dat_Mean)
da47<-telds%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
da47$Variable<-rep("drop_dat_Mean",nrow(da47))

#{48}Var_Name=>'drop_vce_Mean' 
summary(telds$drop_vce_Mean)
da48<-telds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da48$N<-unclass(telds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da48$Churn_perc<-da48$n/da48$N
da48$Greater_Than<-unclass(telds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
da48$Less_Than<-unclass(telds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
da48$Variable<-rep("drop_vce_Mean",nrow(da48))

#{49}Var_Name=>'adjmou' 
summary(telds$adjmou)
da49<-telds%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
da49$N<-unclass(telds%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
da49$Churn_perc<-da49$n/da49$N
da49$Greater_Than<-unclass(telds%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
da49$Less_Than<-unclass(telds%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
da49$Variable<-rep("adjmou",nrow(da49))

#{50}Var_Name=>'totrev' 
summary(telds$totrev)
da50<-telds%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
da50$N<-unclass(telds%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
da50$Churn_perc<-da50$n/da50$N
da50$Greater_Than<-unclass(telds%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
da50$Less_Than<-unclass(telds%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
da50$Variable<-rep("totrev",nrow(da50))

#{51}Var_Name=>'adjrev' 
summary(telds$adjrev)
da51<-telds%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
da51$N<-unclass(telds%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
da51$Churn_perc<-da51$n/da51$N
da51$Greater_Than<-unclass(telds%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
da51$Less_Than<-unclass(telds%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
da51$Variable<-rep("adjrev",nrow(da51))
  

#{52}Var_Name=>'avgrev'
summary(telds$avgrev)
da52<-telds%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
da52$N<-unclass(telds%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
da52$Churn_perc<-da52$n/da52$N
da52$Greater_Than<-unclass(telds%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
da52$Less_Than<-unclass(telds%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
da52$Variable<-rep("adjrev",nrow(da52))

#{53}Var_Name=>'datovr_Range' #Transform & OMITT
summary(telds$comp_dat_Mean)
da53<-telds%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da53$Variable<-rep("comp_dat_Mean",nrow(da53))

#{54}Var_Name=>'plcd_dat_Mean' #Transform & OMITT
summary(telds$plcd_dat_Mean)
da54<-telds%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
da54$Variable<-rep("comp_dat_Mean",nrow(da54))


  ##****DATA TRANSFORMATION & CREATING DUMMY VARIABLES****##  
#{55}Create Dummy Variable plcd_Atempt_Mean & Deciling
telds$plcd_attempt_Mean<-telds$plcd_vce_Mean+telds$plcd_dat_Mean

summary(telds$plcd_attempt_Mean)
da55<-telds%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da55$N<-unclass(telds%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da55$Churn_perc<-da55$n/da55$N
da55$Greater_Than<-unclass(telds%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_attempt_Mean)))[[2]]
da55$Less_Than<-unclass(telds%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_attempt_Mean)))[[2]]
da55$Variable<-rep("plcd_attempt_Mean",nrow(da55))

#{56}Create Dummy Variable complete_Mean & Deciling
telds$complete_Mean<-telds$comp_vce_Mean+telds$comp_dat_Mean

summary(telds$complete_Mean)
da56<-telds%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
da56$N<-unclass(telds%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(dec)%>%unname())[[2]]
da56$Churn_perc<-da56$n/da56$N
da56$Greater_Than<-unclass(telds%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(min(complete_Mean)))[[2]]
da56$Less_Than<-unclass(telds%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(max(complete_Mean)))[[2]]
da56$Variable<-rep("complete_Mean",nrow(da56))

#Now We Have Add all Variables named from da_(numbers) & Create a new Obj.
decl<-rbind(da1,da2,da3,da4,da5,da6,da7,da8,
            da9,da10,da11,da12,da14,da15,da18,
            da19,da20,da21,da22,da23,da24,da25,
            da26,da27,da28,da29,da30,da33,
            da48,da49,da50,da51,da52,da55,
            da56,da30,da43,da44)

#Exporting Deciled Variables
#write.csv(decl,"Decile_Continues_var.CSV",row.names = F)

#Remove those variiables that can not be deciled as they will be Not Good For Our MODEL.
#And Omitt Transformed Var.
names(telds)
telds<-telds[,-c(13,16,17,22,23,45,48:50,65,66,56,57,58)]
names(telds)

##***************************Categorical Variable****************************##
#************************Checking Event Rate in Categorical Var************************##

#Those Leval who will show 5% Churn_Rate So We will OMITT.

#{19} Var "crclscod"
summary(telds$crclscod)
dac19<-telds%>%count(churn,levels=crclscod)%>%filter(churn==1)
dac19$N<-unclass(telds%>%filter(crclscod%in%dac19$levels)%>%count(crclscod))[[2]]
dac19$Churn_perc<-dac19$n/dac19$N
dac19$Variable<-rep("crclscod",nrow(dac19))

#{20} Var "asl_flag"
summary(telds$asl_flag)
dac20<-telds%>%count(churn,levels=asl_flag)%>%filter(churn==1)
dac20$N<-unclass(telds%>%filter(asl_flag%in%dac20$levels)%>%count(asl_flag))[[2]]
dac20$Churn_perc<-dac20$n/dac20$N
dac20$Variable<-rep("asl_flag",nrow(dac20))

#{21} Var "prizm_social_one"
summary(telds$prizm_social_one)
dac21<-telds%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)
dac21$N<-unclass(telds%>%filter(prizm_social_one%in%dac21$levels)%>%count(prizm_social_one))[[2]]
dac21$Churn_perc<-dac21$n/dac21$N
dac21$Variable<-rep("prizm_social_one",nrow(dac21))

#{22} Var "area"
summary(telds$area)
dac22<-telds%>%count(churn,levels=area)%>%filter(churn==1)
dac22$N<-unclass(telds%>%filter(area%in%dac22$levels)%>%count(area))[[2]]
dac22$Churn_perc<-dac22$n/dac22$N
dac22$Variable<-rep("area",nrow(dac22))


#{23} Var "refurb_new"
summary(telds$refurb_new)
dac23<-telds%>%count(churn,levels=refurb_new)%>%filter(churn==1)
dac23$N<-unclass(telds%>%filter(refurb_new%in%dac23$levels)%>%count(refurb_new))[[2]]
dac23$Churn_perc<-dac23$n/dac23$N
dac23$Variable<-rep("refurb_new",nrow(dac23))

#{24} Var "hnd_webcap"
summary(telds$hnd_webcap)
dac24<-telds%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)
dac24$N<-unclass(telds%>%filter(hnd_webcap%in%dac24$levels)%>%count(hnd_webcap))[[2]]
dac24$Churn_perc<-dac23$n/dac23$N
dac24$Variable<-rep("hnd_webcap",nrow(dac24))

#{25} Var "martial"
summary(telds$martial)
dac25<-telds%>%count(churn,levels=marital)%>%filter(churn==1)
dac25$N<-unclass(telds%>%filter(marital%in%dac25$levels)%>%count(marital))[[2]]
dac25$Churn_perc<-dac25$n/dac25$N
dac25$Variable<-rep("martial",nrow(dac25))

#{26} Var "ethnic"
summary(telds$ethnic)
dac26<-telds%>%count(churn,levels=ethnic)%>%filter(churn==1)
dac26$N<-unclass(telds%>%filter(ethnic%in%dac26$levels)%>%count(ethnic))[[2]]
dac26$Churn_perc<-dac26$n/dac26$N
dac26$Variable<-rep("ethnic",nrow(dac26))

#{27} Var "car_buy"
summary(telds$car_buy)
dac27<-telds%>%count(churn,levels=car_buy)%>%filter(churn==1)
dac27$N<-unclass(telds%>%filter(car_buy%in%dac27$levels)%>%count(car_buy))[[2]]
dac27$Churn_perc<-dac27$n/dac27$N
dac27$Variable<-rep("car_buy",nrow(dac27))

#{28} Var "csa"
summary(telds$csa)
dac28<-telds%>%count(churn,levels=csa)%>%filter(churn==1)
dac28$N<-unclass(telds%>%filter(csa%in%dac28$levels)%>%count(csa))[[2]]
dac28$Churn_perc<-dac28$n/dac28$N
dac28$Variable<-rep("csa",nrow(dac28))

#{29} Var "retdays_n"
summary(telds$retdays_n)
telds$retdays_n<-as.factor(telds$retdays_n)#was giving prob while Binding
dac29<-telds%>%count(churn,levels=retdays_n)%>%filter(churn==1)
dac29$N<-unclass(telds%>%filter(retdays_n%in%dac29$levels)%>%count(retdays_n))[[2]]
dac29$Churn_perc<-dac29$n/dac29$N
dac29$Variable<-rep("retdays_n",nrow(dac29))


#Using AS Facrtors :-truck forgntvl,uniqsubs,mtrcycle,actvsubs,models,age2
#{31} Var "age2"
summary(telds$age2)
dac31<-telds%>%count(churn,levels=age2)%>%filter(churn==1)
dac31$N<-unclass(telds%>%filter(age2%in%dac31$levels)%>%count(age2))[[2]]
dac31$Churn_perc<-dac31$n/dac31$N
dac31$Variable<-rep("age2",nrow(dac31))

#{32} Var "models"
summary(telds$models)
dac32<-telds%>%count(churn,levels=models)%>%filter(churn==1)
dac32$N<-unclass(telds%>%filter(models%in%dac32$levels)%>%count(models))[[2]]
dac32$Churn_perc<-dac32$n/dac32$N
dac32$Variable<-rep("models",nrow(dac32))

#{34} Var "actvsubs"
summary(telds$actvsubs)
dac34<-telds%>%count(churn,levels=actvsubs)%>%filter(churn==1)
dac34$N<-unclass(telds%>%filter(actvsubs%in%dac34$levels)%>%count(actvsubs))[[2]]
dac34$Churn_perc<-dac34$n/dac34$N
dac34$Variable<-rep("actvsubs",nrow(dac34))

#{35} Var "uniqsubs"
summary(telds$uniqsubs)
dac35<-telds%>%count(churn,levels=uniqsubs)%>%filter(churn==1)
dac35$N<-unclass(telds%>%filter(uniqsubs%in%dac35$levels)%>%count(uniqsubs))[[2]]
dac35$Churn_perc<-dac35$n/dac35$N
dac35$Variable<-rep("uniqsubs",nrow(dac35))

#{36} Var "forgntvl"
summary(telds$forgntvl)
dac36<-telds%>%count(churn,levels=forgntvl)%>%filter(churn==1)
dac36$N<-unclass(telds%>%filter(forgntvl%in%dac36$levels)%>%count(forgntvl))[[2]]
dac36$Churn_perc<-dac36$n/dac36$N
dac36$Variable<-rep("forgntvl",nrow(dac36))

#{37} Var "mtrcycle"
summary(telds$mtrcycle)
dac37<-telds%>%count(churn,levels=mtrcycle)%>%filter(churn==1)
dac37$N<-unclass(telds%>%filter(mtrcycle%in%dac37$levels)%>%count(mtrcycle))[[2]]
dac37$Churn_perc<-dac37$n/dac37$N
dac37$Variable<-rep("mtrcycle",nrow(dac37))

#{38} Var "truck"
summary(telds$truck)
dac38<-telds%>%count(churn,levels=truck)%>%filter(churn==1)
dac38$N<-unclass(telds%>%filter(truck%in%dac38$levels)%>%count(truck))[[2]]
dac38$Churn_perc<-dac38$n/dac38$N
dac38$Variable<-rep("truck",nrow(dac38))


#{39} Var "hnd_price"
summary(telds$hnd_price)
dac39<-telds%>%count(churn,levels=hnd_price)%>%filter(churn==1)
dac39$N<-unclass(telds%>%filter(hnd_price%in%dac39$levels)%>%count(hnd_price))[[2]]
dac39$Churn_perc<-dac39$n/dac39$N
dac39$Variable<-rep("hnd_price",nrow(dac39))



#Adding Above Var to create new obj
datc_1<-rbind(dac19,dac20,dac21,dac22,dac23,dac24,dac25,dac26,dac27,dac28,dac29)
datc_1<-rbind(dac31,dac32,dac34,dac35,dac36,dac37,dac38,dac39)

#EXPORTING ABOVE
write.csv(datc_1,"Categorical_Event_Rate.CSV",row.names = F)
write.csv(datc_1,"Categorical_Event_Rate.CSV",row.names = F)

#REMOVING THOSE VARIABLES WITH LEVELS LESS THAN5% CHURN.

names(telds)
telds<-telds[,-c(25,44)]
names(telds)
data.frame(colnames(telds))#This code will Give you column names with there index number of ur DF

####=========================Data Preparation==============================####
####=========================Outlier Treatment==============================####
####=========================Continues Variable==============================####
####=========================Using BOXPLOT==============================####
names(telds)
str(telds)
str(telds)
head(telds)
#Factor Variables****>>asl_flag,prizm_social_one,area,refurb_new,hnd_webcap,martial,ethnic,age1,age2
                       #models,hnd_price,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,churn,car_buy,
                       #Customer_ID,retdays.
#lnli =list

lnli<-names(telds)
lnli<-lnli[-c(25:42,50,51)]
lnli<-as.factor(lnli)
data.frame(colnames(telds))#This code will Give me Column Nmaes with there index number of my DF


##***********************************OUTLYING PLOTS*************************************##
par(mfrow=c(3,11))
 for(i in 1:length(lnli)) 
 {
   boxplot(telds[,lnli[i]],main=lnli[i])
 }
for(i in 1:length(lnli)) 
{
   plot(telds[,lnli[i]],main=lnli[i])
}
##=============================TREATMENT OUTLIER=================================##
for(i in 1:length(lnli))
{
  x<-boxplot(telds[,lnli[i]],main=lnli[i])
  out<-x$out
  index<-which(telds[,lnli[i]]%in% x$out)
  telds[index,lnli[i]]<-mean(telds[,lnli[i]],na.rm = T)
  rm(x)
  rm(out)
} 
#Lets Check Again above
for (i in 1:length(lnli)) 
{
  boxplot(telds[,lnli[i]],main=lnli[i])
}
for (i in 1:length(lnli)) 
{
  plot(telds[,lnli[i]],main=lnli[i])
}
dev.off()
##===================MISSING VALUE IMPUTATION===================##

names(telds)
names(telds)

#Factor Variables****>>asl_flag,prizm_social_one,area,refurb_new,hnd_webcap,martial,ethnic,age1,age2
#models,hnd_price,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,churn,car_buy,
#Customer_ID,retdays.

##===================Removing MISSING Values===================##

index1<-which(is.na(telds[,c(1:5)]))
telds<-telds[-index1,]
str(telds)
summary(telds)

index2<-which(is.na(telds$change_mou))
telds<-telds[-index2,]

index4<-which(is.na(telds$area))
telds<-telds[-index4,]

index5<-which(is.na(telds$marital))
telds<-telds[-index5,]
summary(telds)
##===============================Mean Imputation====================================##
telds$avg6mou[is.na(telds$avg6mou)]   <-mean(telds$avg6mou,na.rm = T)
telds$avg6qty[is.na(telds$avg6qty)]   <-mean(telds$avg6qty,na.rm = T)
telds$hnd_price[is.na(telds$hnd_price)]<-mean(telds$hnd_price,na.rm = T)
summary(telds)

#Create Extra Category MISSING for Factor Variables.
telds$prizm_social_one_new<-ifelse(is.na(telds$prizm_social_one),"Missing",as.factor(telds$prizm_social_one))
str(telds$prizm_social_one_new)
telds$prizm_social_one_new<-as.factor(telds$prizm_social_one_new)
summary(telds$prizm_social_one)
summary(telds$prizm_social_one_new)
telds$prizm_social_one_new<-factor(telds$prizm_social_one_new,labels =c("C","R","S","T","U","Missing"))
summary(telds$prizm_social_one_new)

names(telds)
telds<-telds[,-26]
summary(telds)

telds$hnd_webcap_new<-ifelse(is.na(telds$hnd_webcap),"Missing",as.factor(telds$hnd_webcap))
str(telds$hnd_webcap_new)
telds$hnd_webcap_new<-as.factor(telds$hnd_webcap_new)
summary(telds$hnd_webcap)
summary(telds$hnd_webcap_new)
telds$hnd_webcap_new<-factor(telds$hnd_webcap_new,labels =c("UNKW","WC","WCMB","Missing"))
summary(telds$hnd_webcap_new)

names(telds)
telds<-telds[,-28]
summary(telds)

#Crosscheck ChurnRate From New DATASET
table(data$churn)/nrow(data)
table(telds$churn)/nrow(telds)

#Creating DUMMY Variables also Converting to Factor
##=>age1,age2,models,hnd_price,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,Customer ID,Churn

#Variable=>age1
str(telds$age1)
telds$age1_new<-ifelse(telds$age1==0,"Default",ifelse(telds$age1<=30,"Young",
                ifelse(telds$age1>30 & telds$age1<=55,"Mid Age","Old")))

str(telds$age1_new)
telds$age1_new<-as.factor(telds$age1_new)
summary(telds$age1_new)

names(telds)
telds<-telds[,-30]
summary(telds)

#Variable=>age2
str(telds$age2)
telds$age2_new<-ifelse(telds$age2==0,"Default",ifelse(telds$age2<=30,"Young",
                                                      ifelse(telds$age2>30 & telds$age2<=55,"Mid Age","Old")))

str(telds$age2_new)
telds$age2_new<-as.factor(telds$age2_new)
summary(telds$age2_new)

names(telds)
telds<-telds[,-30]
summary(telds)

str(telds$models)
summary(telds$models)
telds$models<-as.factor(telds$models)
summary(telds$models)


str(telds$hnd_price)
summary(telds$hnd_price)
telds$hnd_price<-as.factor(telds$hnd_price)
summary(telds$hnd_price)

str(telds$actvsubs)
summary(telds$actvsubs)
telds$actvsubs<-as.factor(telds$actvsubs)
summary(telds$actvsubs)

str(telds$uniqsubs)
summary(telds$uniqsubs)
telds$uniqsubs<-as.factor(telds$uniqsubs)
summary(telds$uniqsubs)

str(telds$forgntvl)
summary(telds$forgntvl)
telds$forgntvl<-as.factor(telds$forgntvl)
summary(telds$forgntvl)

str(telds$mtrcycle)
summary(telds$mtrcycle)
telds$mtrcycle<-as.factor(telds$mtrcycle)
summary(telds$mtrcycle)

str(telds$truck)
summary(telds$truck)
telds$truck<-as.factor(telds$truck)
summary(telds$truck)

#*************************************************************************************************************
#*******************************************====================**********************************************
#********************************************LOGISTIC REGRESSION**********************************************
#*******************************************====================**********************************************
#*************************************************************************************************************
  
#Splitt DS IN Training and Test DS.
set.seed(200)
index<-sample(nrow(telds),0.70*nrow(telds),replace = F)
train<-telds[index,]
test<-telds[-index,]
summary(train)

#Checking Churn Rate
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)
names(telds)

#Buiding Logistic Regression Model after excluding var "Customer_id"
mod<-glm(churn~.,data = train[,-46],family = "binomial")
summary(mod)

#***************************As SYSTEM IS TOO MUCH HANGING WE Will Doi it MANUALLY************#
#step(mod,direction = "both")

#Now We Will Make Some More Dummy Variable to put in sec

summary(train$asl_flag)
train$asl_flag_Yep<-ifelse(train$asl_flag == "Y",1,0)
test$asl_flag_Yep<-ifelse(test$asl_flag =="Y",1,0)

summary(train$area)
train$area_cali_north<-ifelse(train$area == "CALIFORNIA NORTH AREA",1,0)
test$area_cali_north<-ifelse(test$area =="CALIFORNIA NORTH AREA",1,0)

train$area_texas<-ifelse(train$area == "CENTRAL/SOUTH TEXAS AREA",1,0)
test$area_texas<-ifelse(test$area =="CENTRAL/SOUTH TEXAS AREA",1,0)

train$area_nor_flor<-ifelse(train$area == "NORTH FLORIDA AREA",1,0)
test$area_nor_flor<-ifelse(test$area =="NORTH FLORIDA AREA",1,0)

train$area_north_west<-ifelse(train$area == "NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$area_north_west<-ifelse(test$area =="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

train$area_sth_flrda<-ifelse(train$area == "SOUTH FLORIDA AREA",1,0)
test$area_sth_flrda<-ifelse(test$area =="SOUTH FLORIDA AREA",1,0)

train$area_sth_wst<-ifelse(train$area == "SOUTHWEST AREA",1,0)
test$area_sth_wst<-ifelse(test$area =="SOUTHWEST AREA",1,0)

train$area_tennise<-ifelse(train$area == "TENNESSEE AREA",1,0)
test$area_tennise<-ifelse(test$area =="TENNESSEE AREA",1,0)

summary(train$refurb_new)
train$refurb_R<-ifelse(train$refurb_new == "R",1,0)
test$refurb_R<-ifelse(test$refurb_new =="R",1,0)

summary(train$ethnic)
train$ethnic_C<-ifelse(train$ethnic == "C",1,0)
test$ethnic_C<-ifelse(test$ethnic =="C",1,0)

train$ethnic_N<-ifelse(train$ethnic == "N",1,0)
test$ethnic_N<-ifelse(test$ethnic =="N",1,0)

train$ethnic_O<-ifelse(train$ethnic == "O",1,0)
test$ethnic_O<-ifelse(test$ethnic =="O",1,0)

train$ethnic_S<-ifelse(train$ethnic == "S",1,0)
test$ethnic_S<-ifelse(test$ethnic =="S",1,0)

train$ethnic_U<-ifelse(train$ethnic == "U",1,0)
test$ethnic_U<-ifelse(test$ethnic =="U",1,0)

train$ethnic_Z<-ifelse(train$ethnic == "Z",1,0)
test$ethnic_Z<-ifelse(test$ethnic =="Z",1,0)

summary(train$hnd_price)
train$hnd_price_79.98<-ifelse(train$hnd_price == "79.98999023",1,0)
test$hnd_price_79.98<-ifelse(test$hnd_price =="79.98999023",1,0)

train$hnd_price_105.08<-ifelse(train$hnd_price == "105.083038078331",1,0)
test$hnd_price_105.08<-ifelse(test$hnd_price =="105.083038078331",1,0)

train$hnd_price_129.98<-ifelse(train$hnd_price == "129.9899902",1,0)
test$hnd_price_129.98<-ifelse(test$hnd_price =="129.9899902",1,0)

train$hnd_price_149.98<-ifelse(train$hnd_price == "149.9899902",1,0)
test$hnd_price_149.98<-ifelse(test$hnd_price =="149.9899902",1,0)

train$hnd_price_199.98<-ifelse(train$hnd_price == "199.9899902",1,0)
test$hnd_price_199.98<-ifelse(test$hnd_price =="199.9899902",1,0)

train$hnd_price_249.98<-ifelse(train$hnd_price == "249.9899902",1,0)
test$hnd_price_249.98<-ifelse(test$hnd_price =="199.9899902",1,0)

summary(train$uniqsubs)
train$unq_1<-ifelse(train$uniqsubs == "1",1,0)
test$unq_1<-ifelse(test$uniqsubs =="1",1,0)

train$unq_2<-ifelse(train$uniqsubs == "2",1,0)
test$unq_2<-ifelse(test$uniqsubs =="2",1,0)

train$unq_3<-ifelse(train$uniqsubs == "3",1,0)
test$unq_3<-ifelse(test$uniqsubs =="3",1,0)

train$unq_4<-ifelse(train$uniqsubs == "4",1,0)
test$unq_4<-ifelse(test$uniqsubs =="4",1,0)

train$unq_5<-ifelse(train$uniqsubs == "5",1,0)
test$unq_5<-ifelse(test$uniqsubs =="5",1,0)

train$unq_6<-ifelse(train$uniqsubs == "6",1,0)
test$unq_6<-ifelse(test$uniqsubs =="6",1,0)

train$unq_7<-ifelse(train$uniqsubs == "7",1,0)
test$unq_7<-ifelse(test$uniqsubs =="7",1,0)

train$unq_8<-ifelse(train$uniqsubs == "8",1,0)
test$unq_8<-ifelse(test$uniqsubs =="8",1,0)

train$unq_9<-ifelse(train$uniqsubs == "9",1,0)
test$unq_9<-ifelse(test$uniqsubs =="9",1,0)

summary(train$prizm_social_one_new)
train$prizm_social_R<-ifelse(train$prizm_social_one_new == "R",1,0)
test$prizm_social_R<-ifelse(test$prizm_social_one_new =="R",1,0)

train$prizm_social_T<-ifelse(train$prizm_social_one_new == "T",1,0)
test$prizm_social_T<-ifelse(test$prizm_social_one_new =="T",1,0)

summary(train$age1_new)
train$age1_Mid<-ifelse(train$age1_new == "Mid Age",1,0)
test$age1_Mid<-ifelse(test$age1_new =="Mid Age",1,0)

train$age1_Old<-ifelse(train$age1_new == "Old Age",1,0)
test$age1_Old<-ifelse(test$age1_new =="Old Age",1,0)

train$age1_Young<-ifelse(train$age1_new == "Young Age",1,0)
test$age1_Young<-ifelse(test$age1_new =="Young Age",1,0)

summary(train$age2_new)
train$age2_OLD<-ifelse(train$age1_new == "Old",1,0)
test$age2_OLD<-ifelse(test$age1_new =="Old",1,0)



##****************Running Model From SIGNIFICANT Variables**********************##
#***************************SYSTEM IS HANGING TOOOOOOOOO MUCH*******************##
#**After Saving also data is Gone Last Session Approx 2.30Hour after saving also,Doing it Again**# 
names(train)
model1<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean+
            drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean+
              avgmou + avg3qty + avgqty + avg6mou + asl_flag_Yep + asl_flag_Yep + area_cali_north+
              area_texas + area_nor_flor + area_north_west + area_sth_flrda + area_sth_wst +
              area_tennise + refurb_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U+
              ethnic_Z + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 +
              hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 + unq_1+
              unq_2 + unq_3 + unq_4 + unq_5 + unq_6 + unq_7 + unq_8 + unq_9 + truck+
              adjmou + totrev + retdays_n + complete_Mean + prizm_social_R + prizm_social_T +
              age1_Mid + age1_Old + age1_Young + age2_OLD,data = train,family = "binomial")


summary(model1)

#**Further Running Model With Significant Variable**#


model2<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean+
              drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean+
              avgmou + avg3qty + avgqty + avg6mou + asl_flag_Yep + asl_flag_Yep + area_cali_north+
              area_texas + area_nor_flor + area_north_west + area_sth_flrda + area_sth_wst +
              area_tennise + refurb_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U+
              ethnic_Z + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 +
              hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 +
              unq_2 + unq_3 + unq_4 + unq_7 +adjmou + totrev + retdays_n + complete_Mean + prizm_social_R +
              prizm_social_T +age1_Mid + age2_OLD,data = train,family = "binomial")

summary(model2)

#We Found SigniFicant Variables
#*************MODEL DIGNOSTIC**************#

install.packages("car")
library(car)
vif(model2)

#**running Model with variables ommited to to handle problem of Multicollinearity**#

model3<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean+
              drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean+
              avgqty + asl_flag_Yep + asl_flag_Yep + area_cali_north+
              area_texas + area_nor_flor + area_north_west + area_sth_flrda + area_sth_wst +
              area_tennise + refurb_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U+
              ethnic_Z + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 +
              hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 +
              unq_2 + unq_3 + unq_4 + unq_7 +adjmou + totrev + retdays_n + complete_Mean + prizm_social_R +
              prizm_social_T +age1_Mid + age2_OLD,data = train,family = "binomial")

summary(model3)

#********Removing avgqty**********#

model4<-glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean+
              drop_vce_Range + mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean+
              avgqty + asl_flag_Yep + asl_flag_Yep + area_cali_north+
              area_texas + area_nor_flor + area_north_west + area_sth_flrda + area_sth_wst +
              area_tennise + refurb_R + ethnic_C + ethnic_N + ethnic_O + ethnic_S + ethnic_U+
              ethnic_Z + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 +
              hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98 +
              unq_2 + unq_3 + unq_4 + unq_7 +adjmou + totrev + retdays_n + complete_Mean + prizm_social_R +
              prizm_social_T +age1_Mid + age2_OLD,data = train,family = "binomial")

summary(model4)
vif(model4)
#Confidence Leval
confint(model4)


##********TESTING-OUR-MODEL**********##
##************Prediction************##

pred<-predict(model4,type = "response",newdata = test)
head(pred)
summary(pred)
names(test)

#Seeing Cut-off probablityasper the churn rate in DS
table(telds$churn)/nrow(telds)

#Choosing cutoff value according to KAPPA VALUE
s<-seq(0.2,0.5,0.01)
n<-1
a<-as.vector(length(s))
for (i in s)
{
print(i)  
test$result<-ifelse(pred>i,1,0)
a[n]<-confusionMatrix(table(test$result,test$churn),positive = "1")$overall[2]
print(n)
n=n+1
}
max(a) #0.1477348 We Got Finally
#Testing....
#a[n]<-confusionMatrix(test$result,test$churn,positive = "1")$overall[2]
#length(test$churn)
#length(test$result)
#summary(test$churn)
#summary(test$result)

pred1<-ifelse(pred>=0.1477348,1,0)
table(pred1)

#**************KAPPA MATRIX*************#
library(irr)
kappa2(data.frame(test$churn,pred1))

#*************Confusion Matrix*********#
library(caret)
confusionMatrix(table(pred1,test$churn),positive = "1")
table(test$churn)
#Model is working Fine

#ROCR Curve
pred2<-prediction(pred1,test$churn)
pref<-performance(pred2,"tpr","fpr")
plot(pref,col="red")
abline(0,1,lty=8,col="grey")
auc<-performance(pred2,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc
#The Auc is 0.54 and which is more than 0.50,Also The curve is above the Gray Line.
# Model Looks Like Fine and Acceptable

#Gains Chart
library(gains)
gains(test$churn,predict(model4,type="response",newdata = test),groups=10)

test$prob<-predict(model4,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#So The Top 50% of the Probability is between 0.22974918 - 0.25088703#By using This We Can Give the Ans of asked Questions About CHURN

#1.What are the top five factors driving likehood of churn at Telecom Company?

head(sort(abs(model4$coefficients),decreasing = T),500)
summary(model4)

#<{Top 5 Factors}> <Beta Cofficients>
#hnd_price            1.1412241
#ethnic               0.9763352
#retdays_n            0.7120694 
#uniqsubs as unq_7    0.7314313      
#asl_flag_Yep         0.4245597


#Above Among #Confidence Leval
# totmrc_Mean      -0.00506708505 -0.002410847396
# rev_Range        -0.00047704300  0.000482540478
# mou_Range         0.00013788641  0.000323633965
# change_mou       -0.00024981812 -0.000071231111
# drop_blk_Mean     0.00150252477  0.005525778971
# drop_vce_Range    0.00258413433  0.008981890509
# mou_opkv_Range   -0.00043612307 -0.000083623166
# months           -0.01679027376 -0.008963860793
# eqpdays           0.00083461224  0.001073783316
# iwylis_vce_Mean  -0.00434902902 -0.000596014601
# ovrrev_Mean       0.00163754530  0.003785882812
# avgqty            0.00067399700  0.001260104072
# asl_flag_Yep     -0.50081231852 -0.348955851499
# area_cali_north   0.02406406808  0.210824130477
# area_texas       -0.25668005080 -0.025542856569
# area_nor_flor     0.01012775698  0.229522196499
# area_north_west   0.17486541079  0.389186375048
# area_sth_flrda    0.16884214496  0.409826041657
# area_sth_wst      0.01137840071  0.197022551445
# area_tennise     -0.32446837294 -0.036266351169
# refurb_R          0.10724418275  0.249186154880
# ethnic_C         -1.63331581730 -0.413684561981
# ethnic_N         -0.16414843120 -0.054855494953
# ethnic_O          0.17433491518  0.396377709025
# ethnic_S         -0.16675016962 -0.022146703537
# ethnic_U         -0.18182126984 -0.028452813271
# ethnic_Z         -0.49719839444 -0.258351305542
# hnd_price_79.98  -0.21274189090 -0.056134034118
# hnd_price_105.08 -0.66188464060 -0.116753422281
# hnd_price_129.98 -0.28612807825 -0.132019175533
# hnd_price_149.98 -0.19736985491 -0.066458933265
# hnd_price_199.98 -0.44503600597 -0.267763002286
# hnd_price_249.98 -1.86059438938 -0.537417069079
# unq_2             0.11332560789  0.215511286904
# unq_3             0.06727556136  0.247627701503
# unq_4             0.08650441608  0.369647279794
# unq_7             0.06178118427  1.365484118742
# adjmou           -0.00000965243  0.000003855481
# totrev           -0.00003212801  0.000102326130
# retdays_n1        0.59859264849  0.824759060570
# complete_Mean    -0.00219001808 -0.001420098086
# prizm_social_R    0.00799067745  0.215790923440
# prizm_social_T    0.05843590378  0.182880483663
# age1_Mid         -0.29028240239 -0.194007640641
# age2_OLD         -0.39546230065 -0.245528868645


#2.VAlidation of survey findings.a)Whether "cost and billing" and network and services quality are
#important factors influencing churn behaviour .b)Are data usage connectivity issues turning out to be
#costly? In other word,is it leading to churn?

#"Cost & billing" FACTORS
#Variable totmrc_Mean =>Base plan Charge  Representing Cost to Customer
#Variable rev_Range   =>DATOVR_MEAN+VCEOVR_MEAN =>Mean overage revenue 
#it is sum of the data and voice overage revenues)representing the overage revenue earned from
#customers after billing the same to them.
#overage revenues) representing the overage revenue earned from customers after billing after the same to them
#and var totrev =>TOATAL REVENUE representing total revenue earned from customers.
#totmrc_Mean       0.003737854234
#totrev            0.000035413396
#rev_Range         0.000001005520
#ovrrev_Mean       0.002715080721

#"Network & Service Quality"
#change_mou          0.000160273357
#drop_blk_Mean       0.003530952226
#iwylis_vce_Mean     0.002456387915
#mou_opkv_Range      0.000258952325
#complete_Mean       0.001804045211
#retdays_n1          0.712069415368
#A)Yes,Beta coefficient of variables is showing very important factor influncing churn increase/decrease.
#That is considering with customers join date from analyse day how he/she makes a call to retent.
#Customer Chance of churn is High
#The Factor value helped customer need to be focused to give offers to retain.
#The Beata Cofficient of Variable retdays_1 is expressing a very important Factor influencing Churn Behaviour.
#That is increase in the number of days since a customer makes a retention call,the customer's chances of churning is very high.
#This cokkluld be probably Because their griviances are not being catered to prperly .
#These customers should be paid more ATTENTION & SPECIAL OFFERS should be made to Them depending upon their grieviances.

#Data Usage Factor #Is it LEADING TO CHURN??

#comp_dat_Mean  =>Mean no.of completed data calls.
#plcd_dat_Mean  =>Mean number of attempted data calls placed.
#opk_dat_Mean   =>Mean number off-peak data calls.
#blck_dat_Mean  =>Mean no.of Blocked / failed data calls.
#datovr_Mean    =>Mean no.of blocked/failed data calls.
#datovr_Range   =>Mean revenue of data overage.
#drop_dat_Mean  =>Mean no.of Dropped/Failed Data Calls.

#So The above VAriables Express Data Usage Connectivity.
quantile(data$plcd_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
#The Data Quality Report for all the above variables show that only 10 to 15% customers are actully
#Making Data calls or using the INTERNET.This Could BE Matter of Concern Since the Global Market survey report Shows
#Subscribers who have switched the oprators in recent months reported two key information sources in their information
#Souces in their Decision=> The Internet & the recommandation of family & friends.
#In this case it seems Customers are not really Using the INTERNET .
#So it would i think to work Towards Proving Network Quality Connectivity and Service.
#To Provide Maximumcustomer satisfaction & Reduce Churn.
#Since There is Not enough usable data the above Variables they are Not any Influence on the 
#CHURN BEHAVIOUR AT MOBICOM.

#Q=> Would you recommend rate plan Migration as a proactive retention Strategy?

#YES FOR FEW NOT FOR ALL
#REASON Is => Variable Ovrrev Mean is with 0.004 coeff; its a Mean Overage Revenue in Buisness
#So We can say that =>Sum of Data and Voice Overage Revenue,after Bill.
#Coeff Not strong to show churn activitys, So We Can Consider For few Set of Customers who have Some
#Case Basis Validation Success. But Rate plan Migration as a proactive retention Strategy  Will 
#Not Work for all Customers.

#D)What Would Be Your Recommandation on How to use this  churn model for priortisation of Customers for 
#a Proactive retention Campaigns in Future?
gains(test$churn,predict(model4,type="response",newdata=test),groups = 10)
#Top 20% Probablity shows 29.7%,Closely 30% are more likely to churn

#High Churn RAte Prediction
test$prob<-predict(model4,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#  10%        20%        30%        40%        50%        60%        70% 
# 0.09089993 0.15169267 0.18377774 0.20842158 0.22974918 0.25088703 0.27393039 
#  80%        90%       100% 
# 0.30073009 0.33889326 0.95865255 

#Above % shows 20% of the probablity contains <From 80 to 100(20)> - 0.30073009 0.95865255
#This Notifies us that it May Highly likely CHURN.

#SO Now We Will Apply our CUT-OFF to predict Customer who will Churn.
pred4<-predict(model4,type="response",newdata=test)
pred4<-ifelse(pred4>=0.30073009,1,0)
table(pred4,test$churn)

Targeted<-test[test$prob>0.30073009 & test$prob<=0.95865255 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

write.csv(Targeted,"Target_Customers.csv",row.names=F)




























