rm(list=ls())
#基本包
library(rJava)
library(xlsx)     #读表
library(reshape)  #修改变量名称
#未知
library(lattice)
library(NLP)
library(SnowballC)
library(mvtnorm)
library(hdrcde)
library(locfit)
library(ash)
library(KernSmooth)
library(misc3d)
library(rgl)
library(ks)
library(sp)
library(grid)
library(vcd)
library(topicmodels)
library(rFerns)
library(ranger)
library(Boruta) 
library(lattice)
library(caret)
library(slam)
library(Matrix)
library(foreach)
library(glmnet)
#数据转换
library(plyr)
library(dplyr)   #截取   很强大
#画图表
library(plotrix)
library(igraph)
library(ggplot2)
#基本统计信息
library(car)      #拟合和评价回归模型
library(gvlma)
library(leaps)    #全子集回归
library(nnet)
library(caret)
library(pastecs)
#颜色
library(RColorBrewer)
library(rainbow)
#画地图
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
#支持向量机
library(e1071)
library(SVMMatch)
library(kernlab)
#主成分
library(pcaPP)
#文本挖掘
library(tm)
library(Rwordseg)
library(wordcloud)
library(wordcloud2)
#随机森林
library(randomForest)
#s神经网络
library(Rcpp)#与RSNNS关联
library(RSNNS)#涉及到神经网络中的其它拓扑结构和网络模型
#Stuttgart Neural Network Simulator（SNNS）是德国斯图加特大学开发的优秀神经网络仿真软件
library(nnet)#提供了最常见的前馈反向传播神经网络算法
library(AMORE)#提供了更为丰富的控制参数，并可以增加多个隐藏层
library(neuralnet)#提供了弹性反向传播算法和更多的激活函数形式
library(autoencoder)
library(deepnet)#实现了一些Deep Learning结构和Neural Network相关算法，
#包括BP，RBM训练，Deep Belief Net，Deep Auto-Encoder
#聚类分析
library(cluster)
library(MASS)
library(stats)  #hclust,kmeans等函数
library(fpc)
library(amap)
#emd或eemd
library(Rlibeemd)
library(EMD)
library(zoo)
#library(xts)#程辑包‘xts’是用R版本3.5.0 来建造的 
#数据包络模型
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
#处理缺失值
library(VIM)
library(mice)

#卸载安装
# remove.packages("lpSolveAPI")
# install.packages("ucminf")

#------------------------------#
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(zoo)
library(xts)
library(Rlibeemd)
library(hht)
cat("\014")  


#----------------------------#
#-----三个趋势图-------------#
#----------------------------#


#-------一、Taliban----------#
rm(list=ls())
Taliban_trend<-read.csv("E:\\GTD\\adjust/aaa/adjust_time/Taliban_new.csv",1)
hh_score<-xts(Taliban_trend$tt,as.Date(Taliban_trend$dates,format='%Y/%m/%d'))
plot(hh_score,type = 'l',main='Talibna Efficiency ')  

hh<-hh_score

imfs=ceemdan(hh)
#ts.plot(rowSums(imfs[,1:ncol(imfs)]))
plot(imfs[,1])
plot(imfs[,2])
plot(imfs[,3])
plot(imfs[,4])
plot(imfs[,5])
plot(imfs[,6])
plot(imfs[,7])
plot(imfs[,8])
ts.plot(imfs[,9])
ts.plot(imfs[,10])
ts.plot(imfs[,11])
ts.plot(imfs[,12])
ts.plot(imfs[,13])
ts.plot(imfs[,14])


plot(ts.union(imf1 =imfs[,1],imf2 =imfs[,2],imf3 =imfs[,3],
              imf1 =imfs[,4],imf5 =imfs[,5],imf6 =imfs[,6],
              imf7 =imfs[,7],imf8 =imfs[,8],imf9 =imfs[,9],
              Trend =imfs[,13]),
     main = "Taliban Trend"
     )




#-------二、ISIL----------#
rm(list=ls())
ISIL_trend<-read.csv("E:\\GTD\\adjust/aaa/adjust_time/ISIL_new.csv",1)
hh_score<-xts(ISIL_trend$tt,as.Date(ISIL_trend$date,format='%Y/%m/%d'))
plot(hh_score,type = 'l',main='ISIL Efficiency ')  

hh<-hh_score
imfs=ceemdan(hh)
# ts.plot(hh,imfs[,ncol(imfs)],col=1:2)
# ts.plot(rowSums(imfs[,1:ncol(imfs)]))
plot(imfs[,1])
plot(imfs[,2])
plot(imfs[,3])
plot(imfs[,4])
plot(imfs[,5])
plot(imfs[,6])
plot(imfs[,7])
plot(imfs[,8])
ts.plot(imfs[,9])
ts.plot(imfs[,10])
ts.plot(imfs[,11])
ts.plot(imfs[,12])
ts.plot(imfs[,13])
ts.plot(imfs[,14])

plot(ts.union(imf1 =imfs[,1],imf2 =imfs[,2],imf3 =imfs[,3],
              imf1 =imfs[,4],imf5 =imfs[,5],imf6 =imfs[,6],
              imf7 =imfs[,7],imf8 =imfs[,8],imf9 =imfs[,9],
              Trend =imfs[,10]),
     main = "ISIL Trend"
)


#-------三、SL----------#
rm(list=ls())
SL_trend<-read.csv("E:\\GTD\\adjust/aaa/adjust_time/SL_new_a.csv",1)
hh_score<-xts(SL_trend$tt,as.Date(SL_trend$dates,format='%Y/%m/%d'))
#plot(hh_score,type = 'l',main='SL Efficiency ') 

hh<-hh_score
imfs=ceemdan(hh)
ts.plot(rowSums(imfs[,1:ncol(imfs)]))
plot(imfs[,1])
plot(imfs[,2])
plot(imfs[,3])
plot(imfs[,4])
plot(imfs[,5])
plot(imfs[,6])
plot(imfs[,7])
plot(imfs[,8])
ts.plot(imfs[,9])
ts.plot(imfs[,10])
ts.plot(imfs[,11])
ts.plot(imfs[,12])
ts.plot(imfs[,13])
ts.plot(imfs[,14])


plot(ts.union(imf1 =imfs[,1],imf2 =imfs[,2],imf3 =imfs[,3],
              imf1 =imfs[,4],imf5 =imfs[,5],imf6 =imfs[,6],
              imf7 =imfs[,7],imf8 =imfs[,8],imf9 =imfs[,9],
              Trend =imfs[,12]),
     main = "SL Trend"
)





#----------------------------#
#--从原始数据提取更多因子----#
#----------------------------#

all_a<-read.csv("E:\\GTD/adjust/aaa/detail/70_16.csv",1,encoding="UTF-8")
all<-all_a
all1<-filter(all,success==1)
all2<-filter(all1,gname!="Unknown")   #去掉不知道组织名称的数据
all3<-filter(all2,guncertain1==0)   #去掉组织名称不确定的数据
Taliban_raw<-filter(all3,gname=="Taliban")  
SL_raw<-filter(all3,gname=="Shining Path (SL)")  
ISIL_raw<-filter(all3,gname=="Islamic State of Iraq and the Levant (ISIL)")  
Al_Qaida<-filter(all3,gname=="Al-Qaida")
#"a-"不能做名称

write.csv(Taliban_raw,"E:\\GTD/adjust/aaa/detail/Taliban_raw.csv")
write.csv(SL_raw,"E:\\GTD/adjust/aaa/detail/SL_raw.csv")
write.csv(ISIL_raw,"E:\\GTD/adjust/aaa/detail/ISIL_raw.csv")
write.csv(Al_Qaida,"E:\\GTD/adjust/aaa/detail/Al_Qaida.csv")


#----提取高效率值的攻击类型---------#
high_ISIL<-read.xlsx("E:\\GTD/adjust/aaa/high_ISIL.xlsx",1)
table(high_ISIL$attacktype1)

high_SL<-read.xlsx("E:\\GTD/adjust/aaa/high_SL.xlsx",1)
table(high_SL$attacktype1)

high_Taliban<-read.xlsx("E:\\GTD/adjust/aaa/high_Taliban.xlsx",1)
table(high_Taliban$attacktype1)




#-------攻击类型翻转处理（便于观看）------------#
TALIBAN2<-read.csv("E:\\GTD\\a_all/Taliban/TALIBAN2.csv",1)
#攻击类型替换成得分
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.001)]<-"Hostage Taking (Kidnapping)"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.084)]<-"Bombing/Explosion"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.088)]<-"Hostage Taking (Barricade Incident)"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.132)]<-"Hijacking"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.333)]<-"Assassination"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.364)]<- "Armed Assault"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.393)]<- "Unknown"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.492)]<-"Facility/Infrastructure Attack"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==1)]<-"Unarmed Assault"  

write.csv(TALIBAN2,"E:\\GTD\\a_all/Taliban/TALIBAN3.csv")


