rm(list=ls())
library(rJava)
library(xlsx)    
library(plyr)
library(dplyr) 
#数据包络模型
library(lpSolveAPI) 
library(Rglpk)
library(Rdonlp2)
library(lpSolve)  #线性规划
library(ucminf)
library(Benchmarking)
library(bootstrap)
library(TFDEA)
#一个高效的管道操作工具包，通过管道的连接方式，让数据或表达式的传递更高效，
#使用操作符%>%，可以直接把数据传递给下一个函数调用或表达式。
library(magrittr)


raw_data<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/all_new/all_f.csv",1)
data<-filter(raw_data,raw_data$outcome==1)
#---------------------------------------------------------------------#
#--------------------benchmarking封装包做法---------------------------#
#---------------------------------------------------------------------#
data$e_f<-data$error+data$foul
data$defend<-data$steal+data$block+data$rebound
data$attack<-data$score+data$assist  
data$vs_attack<-data$vs_score+data$vs_assist
data$vs_defend<-data$vs_steal+data$vs_block+data$vs_rebound

z2<-data.frame(data$attack,data$defend,data$e_f,data$vs_attack,data$vs_defend)
z2<-as.matrix(z2)

data$vs_e_f<-data$vs_error+data$vs_foul
y<-data.frame(data$scoregap,data$vs_e_f)
y<-as.matrix(y)

e2<-dea(z2,y,RTS="crs",ORIENTATION = "out")   #ORIENTATION = "out"
e2$eff[1:50]

#---------------------------------------------------------------------#
#----------------线性规划分解做法(投入导向的CCR)----------------------#
#---------------------------------------------------------------------#
n <- nrow(data)
inputs<-z2            #5个投入，2个产出，1230个DMU
s <- ncol(inputs)
output<-y
m <- ncol(output)
eff<-list(NULL)  #构建一个空值的list
length(eff)<-1230

for(i in 1:n){
  obj.f <- c(rep(0,s), as.numeric(output[i,]))
  # left-hand sides of the constraints
  cons_lhs_1 <- cbind(-1*inputs, output)
  cons_lhs_2 <- c(as.numeric(inputs[i,]), rep(0,m))
  cons.f <- rbind(cons_lhs_1, cons_lhs_2)
  cons.dir <- c(rep("<=",n), "=")
  # right-hand sides of the constraints
  rhs <- c(rep(0,n),1)
  results <- lp("max", obj.f, cons.f, cons.dir, rhs, scale = 0)
  eff [i]<- results$objval
  # if (i==1) {
  #   weights <- multipliers
  #   effcrs <- efficiency
  #   lambdas <- duals [seq(1,N)]
  # } else {
  #   weights <- rbind(weights,multipliers)
  #   effcrs <- rbind(effcrs , efficiency)
  #   lambdas <- rbind(lambdas,duals[seq(1,N)])
  # }
}
eff[1:10]


#---------------------------------------------------------------------#
#----------------线性规划分解做法(投入导向的BCC)----------------------#
#---------------------------------------------------------------------#

n <- nrow(data)
inputs<-z2            #5个投入，2个产出，1230个DMU
s <- ncol(inputs)
output<-y
m <- ncol(output)
eff<-list(NULL)  #构建一个空值的list
length(eff)<-1230

for(i in 1:n){
  #按照     --x-----,---------y-------------,--u--的顺序排列
  obj.f <- c(rep(0,s), as.numeric(output[i,]),1,-1)
  # left-hand sides of the constraints
  cons_lhs_1 <- cbind(-1*inputs, output,1,-1)
  cons_lhs_2 <- c(as.numeric(inputs[i,]), rep(0,m+2))
  cons.f <- rbind(cons_lhs_1, cons_lhs_2)
  cons.dir <- c(rep("<=",n), "=")
  # right-hand sides of the constraints
  rhs <- c(rep(0,n),1)
  results <- lp("max", obj.f, cons.f, cons.dir, rhs, scale = 0)
  eff [i]<- results$objval
  # if (i==1) {
  #   weights <- multipliers
  #   effcrs <- efficiency
  #   lambdas <- duals [seq(1,N)]
  # } else {
  #   weights <- rbind(weights,multipliers)
  #   effcrs <- rbind(effcrs , efficiency)
  #   lambdas <- rbind(lambdas,duals[seq(1,N)])
  # }
}
eff[1:10]



#---------------------------------------------------------------------#
#----------------线性规划分解做法(产出导向的BCC)----------------------#
#---------------------------------------------------------------------#

n <- nrow(data)
inputs<-z2            #5个投入，2个产出，1230个DMU
s <- ncol(inputs)
output<-y
m <- ncol(output)
eff<-list(NULL)  #构建一个空值的list
length(eff)<-1230

for(i in 1:n){
  #按照     -----------------x-----,----y---,--u--的顺序排列
  obj.f <- c(as.numeric(inputs[i,]),rep(0,m), 1,-1)
  # left-hand sides of the constraints
  cons_lhs_1 <- cbind(-1*inputs, output,-1,1)
  cons_lhs_2 <- c( rep(0,s),as.numeric(output[i,]),0,0)
  cons.f <- rbind(cons_lhs_1, cons_lhs_2)
  cons.dir <- c(rep("<=",n), "=")
  # right-hand sides of the constraints
  rhs <- c(rep(0,n),1)
  results <- lp("min", obj.f, cons.f, cons.dir, rhs, scale = 0)
  eff [i]<- results$objval
  # if (i==1) {
  #   weights <- multipliers
  #   effcrs <- efficiency
  #   lambdas <- duals [seq(1,N)]
  # } else {
  #   weights <- rbind(weights,multipliers)
  #   effcrs <- rbind(effcrs , efficiency)
  #   lambdas <- rbind(lambdas,duals[seq(1,N)])
  # }
}
eff[1:10]






























































