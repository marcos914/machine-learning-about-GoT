#数据格式更改
#1清空坏境
rm(list=ls())
setwd('G://seminar')
#2数据导入
library(survival)
Lung <- lung
write.csv(Lung,"exampledata.csv")
Baseline <- read.csv('exampledata.csv',header = T,encoding = 'UTF-8')
Baseline <- Baseline[,2:11]
#3分组
Baseline$Age[Baseline$age>65]<-'>65'
Baseline$Age[Baseline$age<=65]<-'<=65'
#4 频率统计
Base<-subset(Baseline,select=c(Age,sex,status,ph.ecog))
Freq<-lapply(Base,table)
Prop<-lapply(Freq, prop.table)
#5 建表
names(Freq[1])
names(Freq[[1]])
character<-c(names(Freq[1]),names(Freq[[1]]))
Freq[[1]]
Prop[[1]]
NOC<-c(NA,paste0(Freq[[1]],'(',Prop[[1]],')'))
characteristic<-data.frame('characteristic'=character,'Number of cases'= NOC)
#5.1 循环建表
char<-NULL
for(i in 1:3){
  character<-c(names(Freq[i]),names(Freq[[i]]))
  NOC<-c(NA,paste0(Freq[[i]],'(',Prop[[i]]*100,')'))
  characteristic<-data.frame('characteristic'=character,'Number of cases(%)'= NOC)
  char<-rbind(char,characteristic)
}
#导出数据
write.csv(char,'characteristic.csv')
#6 cox回归
#6.1 加载包
library(survival)
library(plyr)
#6.2 转换因子型为数字型
as.numeric(Baseline$sex)
#6.3 单因素cox回归
Basurv<-Surv(time = Baseline$time,event=Baseline$status)
Baseline$Basurv <- with(Baseline,Basurv)
Gcox<-coxph(Basurv~sex,data=Baseline)
GSum<-summary(Gcox)
GSum$coefficients
HR<-round(GSum$coefficients[,2],2)
Pvalue<-round(GSum$coefficients[,5],3)
CI<-paste0(round(GSum$conf.int[,3:4],2), collapse = '-')
Unix<-data.frame('characteristics'='sex','Hazard Ratio'=HR,'CI95'=CI,'Pvalue'=Pvalue)
#6.4 构建循环
UniCox<- function(x){
  FML <- as.formula(paste0('Basurv~',x))
  Gcox<-coxph(FML,data=Baseline)
  GSum<-summary(Gcox)
  GSum$coefficients
  HR<-round(GSum$coefficients[,2],2)
  Pvalue<-round(GSum$coefficients[,5],3)
  CI<-paste0(round(GSum$conf.int[,3:4],2),collapse ='-')
  Unix<-data.frame('characteristics'= x,'Hazard Ratio'=HR,'CI95'=CI,'Pvalue'=Pvalue)
  return(UniCox)
}
UniCox('inst')
VarNames <- colnames(Baseline)[c(1,4:5)]
VarNames
Univar<-lapply(VarNames, UnixCox)
Univar<-ldply(Univar,data.frame)
#多因素cox回归
MFML <-as.formula(paste0('Basurv~',paste0(Univar$Characteristics[Univar$Pvalue<0.05],collapse = "+")))
Multicox <- coxph(MFML,data=Baseline)
MultiSum <- summary(Multicox)
Multiname<- as.character(Univar$Characteristics[Univar$Pvalue<0.05])
MHR<-round(MultiSum$coefficients[,2],2)
MPvalue<-round(MultiSum$coefficients[,5],3)
MCI<-paste0(round(MultiSum$conf.int[,3:4],2),collapse ='-')
MUnix<-data.frame('characteristics'= Multiname,'Hazard Ratio'=MHR,'CI95'=MCI,'Pvalue'=MPvalue)

