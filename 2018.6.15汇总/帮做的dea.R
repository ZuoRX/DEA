rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(zoo)
library(xts)
library(bootstrap)
library(TFDEA)
library(lpSolve)
cat("\014")  

#--------------#
#SL数据优化处理#
#--------------#
aaa<-read.xlsx("C:/Users/lenovo/Desktop/aaa.xlsx",1)
x<-data.frame(aaa$a,aaa$b)
x<-as.matrix(x)
y<-aaa$c
y<-as.matrix(y)
d<-dea(x,y)
dd<-d$eff
dd
#----超效率dea------#
e <- sdea(x,y,RTS = "vrs", ORIENTATION = "in")
ee<-e$eff
ee
list(ee)
write.xlsx(ee,"C:/Users/lenovo/Desktop/super1.xlsx")
write.xlsx(dd,"C:/Users/lenovo/Desktop/bbb.xlsx")
