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

#--------------------------这部分所要用到的包-----------------------------#
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(VIM)
library(mice)
rm(list=ls())
cat("\014")  
#.rs.restartR()
#-----------------------攻击类型伤亡分析-------------------------------------#
score<-read.csv("e:/GTD/score/70_16.csv",1)
attcak_type<-table(score$attacktype1_txt)
attcak_type

#第一步提取Armed Assault攻击类型
weap1<-filter(score,attacktype1_txt=="Armed Assault")
#计算伤亡人数
num_weap1<-data.frame(weap1$nkill,weap1$nwound)
Armed_Assault<-write.xlsx(num_weap1,"e:/GTD/score/num_weap1.xlsx","sheet 1")
#求均值
a<-(153131+71869)/40223
a #5.593814

#第二步提取Bombing/Explosion攻击类型
weap2<-filter(score,attacktype1_txt=="Bombing/Explosion")
#计算伤亡人数
num_weap2<-data.frame(weap2$nkill,weap2$nwound)
Bombing<-write.xlsx(num_weap2,"e:/GTD/score/num_weap2.xlsx","sheet 1")
#求均值
a<-(145326+356153)/40223
a

#第三步提取 Hijacking攻击类型
weap3<-filter(score,attacktype1_txt=="Hijacking")
#计算伤亡人数
num_weap3<-data.frame(weap3$nkill,weap3$nwound)
Hijacking<-write.xlsx(num_weap3,"e:/GTD/score/num_weap3.xlsx","sheet 1")
#求均值      剔除911事件
a<-(153131+71869)/40223
a   #6.036606358
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)

#第四步提取Hostage Taking (Kidnapping)攻击类型
weap4<-filter(score,attacktype1_txt=="Hostage Taking (Kidnapping)")
#计算伤亡人数
num_weap4<-data.frame(weap4$nkill,weap4$nwound)
Hostage<-write.xlsx(num_weap4,"e:/GTD/score/num_weap4.xlsx","sheet 1")
#求均值
a
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)

#第五步提取 Assassination攻击类型
weap5<-filter(score,attacktype1_txt=="Assassination")
#计算伤亡人数
num_weap5<-data.frame(weap5$nkill,weap5$nwound)
Assassination<-write.xlsx(num_weap5,"e:/GTD/score/num_weap5.xlsx","sheet 1")
#求均值
a
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)

#第六步提取 Facility/Infrastructure Attack攻击类型
weap6<-filter(score,attacktype1_txt=="Facility/Infrastructure Attack")
#计算伤亡人数
num_weap6<-data.frame(weap6$nkill,weap6$nwound)
Facility<-write.xlsx(num_weap6,"e:/GTD/score/num_weap6.xlsx","sheet 1")
#求均值
a
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)

#第七步提取Hostage Taking (Barricade Incident)攻击类型
weap7<-filter(score,attacktype1_txt=="Hostage Taking (Barricade Incident)")
#计算伤亡人数
num_weap7<-data.frame(weap7$nkill,weap7$nwound)
Barricade<-write.xlsx(num_weap7,"e:/GTD/score/num_weap7.xlsx","sheet 1")
#求均值
a
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)

#第八步提取Unarmed Assault攻击类型
weap8<-filter(score,attacktype1_txt=="Unarmed Assault")
#计算伤亡人数
num_weap8<-data.frame(weap8$nkill,weap8$nwound)
Unarmed<-write.xlsx(num_weap8,"e:/GTD/score/num_weap8.xlsx","sheet 1")
#求均值
a
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)

#第九步提取Unknown攻击类型
weap9<-filter(score,attacktype1_txt=="Unknown")
#计算伤亡人数
num_weap9<-data.frame(weap9$nkill,weap9$nwound)
Unknown<-write.xlsx(num_weap9,"e:/GTD/score/num_weap9.xlsx","sheet 1")
#求均值
a
#excel函数  =IFERROR(IF(B2<0,0,B2),0)    =IFERROR(IF(D2<0,0,D2),0)



#------塔利班效率--------#
Taliban<-read.xlsx("e:/GTD/accum_Taliban.xlsx",1,encoding="UTF-8")
names(Taliban)
x<-data.frame(Taliban$adj_nperps,
              Taliban$nperpcap,Taliban$adj_nkillter)
#如果未成功，效率=0 ？？
y<-data.frame(Taliban$adj_nkill,
              Taliban$adj_nwound)
e<-dea(x,y)
summary(e)
# efficiency <- eff(e)   #查看具体的效率值
# efficiency
print(e)
lambda(e)
(1-e$eff) * x    #投入潜力







#---------------------------数据基本处理-----------------------#
# gtd_4<-read.csv("e:/GTD/Book1.csv",1,encoding="UTF-8")
# gtd_4_1<-filter(gtd_4,gname!="Unknown")   #去掉不知道组织名称的数据
# gtd_4_2<-filter(gtd_4_1,guncertain1==0)   #去掉组织名称不确定的数据
# class(gtd_4_2)
# write.table(gtd_4_2,"e:/GTD/adj_4year1.csv",sep=",",col.names = TRUE,row.names = FALSE)
# names(gtd_4_2)
# gtd_4_2<-gtd_4_2[-20]           #去除变量propvalue,只有157个有效值
#aa<-summary(gtd_4_2$gname)       #提取组织频数
#write.xlsx(aa,"e:/GTD/num.xlsx","sheet 1")

#----------关于数据调整的说明——————————#
#关于袭击者人数缺失值的处理#
  #方案一：袭击者人数>=袭击者死亡人数+袭击者受伤人数（且袭击者人数>=1）
  #方案二：按攻击类型取均值（但有效数据样本只有11%）
#关于是否有财产损失，原始数据-9表示unknown,我修改成0
#去除变量propvalue,只有157个有效值

#---------已经调整的4年数据-----------#
adj_4year<-read.csv("e:/GTD/adj_4year1.csv",2)
names(adj_4year)
adj_4year<-adj_4year[,c(-9,-14,-16,-20)]
names(adj_4year)

org<-table(adj_4year$gname)
org
order(org)
plot(org)
write.xlsx(org,"e:/GTD/org.xlsx","sheet 1")
org_t<-table(org)
plot(org_t)

#------------------------用塔利班组织做分析---------------------#
Taliban<-filter(adj_4year,gname=="Taliban")  
#write.xlsx(Taliban,"e:/GTD/Taliban.xlsx","sheet 1")
sum(is.na(Taliban))            #3562个缺失值

#最后四个变量，塔利班属于阿富汗组织，去掉美国本土变量
adj_Taliban<-Taliban[,c(-20,-21,-22,-23)]
names(adj_Taliban)

#------袭击者人数为输入，杀人数为输出，算效率--------#
#攻击类型、武器类型未录入
x<-data.frame(adj_Taliban$suicide,adj_Taliban$adj_nperps,
              adj_Taliban$nperpcap,adj_Taliban$nkillter,
              adj_Taliban$nwoundte)
#如果未成功，效率=0 ？？
y<-data.frame(adj_Taliban$success,adj_Taliban$nkill,
              adj_Taliban$nwound,adj_Taliban$adj_property,
              adj_Taliban$propextent)
e<-dea(x,y)
summary(e)
# efficiency <- eff(e)   #查看具体的效率值
# efficiency
print(e)
lambda(e)
(1-e$eff) * x    #投入潜力 Input savings potential for each firm

# fdh+ with limits in the interval [.7, 1.2]
dea(x,y,RTS="fdh+", param=c(.7,1.2))



#------------------------用频数为1的组织做分析---------------------#
rm(list=ls())
cat("\014")
# freq_1<-filter(adj_4year,freq_gname==1) 
# write.xlsx(freq_1,"e:/GTD/freq_1.xlsx","sheet 1")
freq_1<-read.xlsx("e:/GTD/freq_1.xlsx",1)
names(freq_1)
freq_2<-freq_1[,c(7,10,12,14,15)]
#freq_2<-t(freq_2)
freq_3<-freq_1[,c(16,17,18,19,20)]
#freq_3<-t(freq_3)
x<-data.frame(freq_2)
y<-data.frame(freq_3)
e_freq_1<-dea(x,y)
summary(e_freq_1)
print(e_freq_1)
lambda(e_freq_1) #the weight of the peers, for each firm
(1-e_freq_1$eff) * x  

#-------------------------塔利班物力投入----------------------------------#
accum_Taliban<-read.xlsx("e:/GTD/accum_Taliban.xlsx",1)
attcak_type<-table(accum_Taliban$attacktype1_txt)
attcak_type
plot(attcak_type)




#-------------------------缺失值处理---------------------------------#
data("sleep")
sleep
aa<-sleep[complete.cases(sleep),]
summary(aa)
print(aa)
count(aa)
sleep[!complete.cases(sleep),]
sum(is.na(sleep$Dream))





















