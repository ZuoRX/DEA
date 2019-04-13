rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(plyr)
library(dplyr)   #截取   很强大
library(gdata)   #读取xls 文件 excel
library(Hmisc)    #缺失值处理 #
#爬虫
library(bitops)
library(RCurl)
library(XML)
#数据包络模型
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(bootstrap)
library(TFDEA)
library(productivity) #Malmquist Productivity Index
library(rDEA)         #考虑环境变量
library(frm)         #分式规划

all$salary16[all$player=="凯-费尔德" & all$season=="16-17"]<-54
all$salary17[all$player=="凯-费尔德" & all$season=="17-18"]<-131
all$code[all$player=="凯-费尔德"]<-54
all$name[all$player=="凯-费尔德"]<-"Kay Felder "





#------------------------------------------------------------------------------#
d16_2<-all
#添加总时间和板凳时间
alltime<-rep(48,nrow(d16_2))
benchtime<-rep(0,nrow(d16_2))
d16_2<-cbind(alltime,d16_2)
d16_2<-cbind(benchtime,d16_2)

#-----------取总比分的差值----------#
# gsub("-","\\n",d16_2$game)
#d16_2$game
#汉字的正则表达[^\x00-\xff] 或 [\u4e00-\u9fa5]
tscore<-gsub("76[\u4e00-\u9fa5]","",d16_2$game)
tscore<-gsub("[\u4e00-\u9fa5]","",tscore)
#tscore
scoreleft<-gsub("-\\d+","",tscore)
#scoreleft
scoreright<-gsub("\\d+-","",tscore)
#scoreright
#scoregap赋值一定要放在循环外面，否则一直循环出错
scoregap<-rep(0,nrow(d16_2))
for (i in 1:nrow(d16_2)) {
  scoregap[i]<-as.numeric(scoreright)[i] - as.numeric(scoreleft)[i]
}
scoregap<-scoregap*0.02
scoregap   #存在缺失值
d16_2<-data.frame(d16_2,scoregap)
#---------对胜负替换成虚拟变量--------#
d16_2$outcome<-gsub("胜",1, d16_2$outcome)
d16_2$outcome<-gsub("负",-1, d16_2$outcome) 
as.numeric(d16_2$outcome)
#------- --板凳时间---------#
d16_2$benchtime<-d16_2$alltime-d16_2$time

#简单插补缺失值
d16_2<-impute(d16_2,0)
#导出各赛季值
d2016<-filter(d16_2,d16_2$season=="16-17")
d2017<-filter(d16_2,d16_2$season=="17-18")
write.csv(d2016,"c:/users/lenovo/desktop/BALL/data_NBA/2016_raw/d2016.csv",row.names = F)
write.csv(d2017,"c:/users/lenovo/desktop/BALL/data_NBA/2016_raw/d2017.csv",row.names = F)

#------------------------------------------------------------------------------#
#-----------------------------两阶段DEA(原始变量)------------------------------#
#------------------------------------------------------------------------------#
rm(list=ls())
d16_2<-read.csv("c:/users/lenovo/desktop/BALL/data_NBA/2016_raw/d2016.csv",1)
x<-data.frame(d16_2$time,d16_2$salary) 
#投入是年度薪金，上场时间
z<-data.frame(d16_2$error,
              d16_2$hit_target,d16_2$free_throw,
              d16_2$rebound,d16_2$assist,d16_2$steal,
              d16_2$blocks,d16_2$a_foul,d16_2$benchtime)
#中间产出包括进攻得分、防守得分、板凳时间、失误
y<-data.frame(d16_2$outcome,d16_2$scoregap)
#产出即胜负和分差的结合
y<-as.matrix(y)
x<-as.matrix(x)
z<-as.matrix(z)
e1<-sdea(x,z)
e1$eff
e2<-sdea(z,y)
e2$eff
e<-e1$eff*e2$eff
e

































































































