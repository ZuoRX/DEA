rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(zoo)
library(xts)
cat("\014")  

#----------------修改nkill和nwound---------------------------------------#
#nkill容易修改，直接减去nkiller
#nwound 要把袭击者受伤人数加进去，再减
all_a<-read.csv("E:\\GTD/adjust/all_a.csv",1,encoding="UTF-8")
all<-all_a
all1<-filter(all,success==1)

#物力投入得分也要修改    此项里面组织名称可以不用确切
names(all1)
all_score<-all1[,c(7,11,12)]
n1<-filter(all_score,attacktype1==1)
n2<-filter(all_score,attacktype1==2)
n3<-filter(all_score,attacktype1==3)
n4<-filter(all_score,attacktype1==4)
n5<-filter(all_score,attacktype1==5)
n6<-filter(all_score,attacktype1==6)
n7<-filter(all_score,attacktype1==7)
n8<-filter(all_score,attacktype1==8)
n9<-filter(all_score,attacktype1==9)
write.xlsx(n1,"E:\\GTD/adjust/score/n1.xlsx")
write.xlsx(n2,"E:\\GTD/adjust/score/n2.xlsx")
write.xlsx(n3,"E:\\GTD/adjust/score/n3.xlsx")
write.xlsx(n4,"E:\\GTD/adjust/score/n4.xlsx")
write.xlsx(n5,"E:\\GTD/adjust/score/n5.xlsx")
write.xlsx(n6,"E:\\GTD/adjust/score/n6.xlsx")
write.xlsx(n7,"E:\\GTD/adjust/score/n7.xlsx")
write.xlsx(n8,"E:\\GTD/adjust/score/n8.xlsx")
write.xlsx(n9,"E:\\GTD/adjust/score/n9.xlsx")

#-------------------------------------------------#
all2<-filter(all1,guncertain1==0)
names(all2)
all3<-all2[,c(-4,-5)]

#截取第一个组织Taliban
Taliban<-filter(all3,gname=="Taliban")
write.csv(Taliban,"E:\\GTD\\adjust/Taliban/Taliban.csv")
#截取第二个组织ISIL
ISIL<-filter(all3,gname=="Islamic State of Iraq and the Levant (ISIL)")
write.csv(ISIL,"E:\\GTD\\adjust/ISIL/ISIL.csv")
#截取第三个组织SL
SL<-filter(all3,gname=="Shining Path (SL)")
write.csv(SL,"E:\\GTD\\adjust/SL/SL.csv")

Al_Qaida<-filter(all3,gname=="Al-Qaida")
write.csv(Al_Qaida,"E:\\GTD\\adjust/Al-Qaida/Al_Qaida.csv")































































