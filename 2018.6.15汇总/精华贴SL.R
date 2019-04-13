rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(zoo)
library(xts)
library(bootstrap)
library(TFDEA)
library(lpSolve)
# library(textmineR)
# 1.topic 最近 关系最强 
# 2.





cat("\014")  

#--------------#
#SL数据优化处理#
#--------------#
SL<-read.csv("E:\\GTD\\adjust/SL/SL.csv",1)
#攻击类型替换成得分
SL$attacktype1[which(SL$attacktype1==1)]<-0.063  #Assassination
SL$attacktype1[which(SL$attacktype1==2)]<-0.258  #Armed Assault
SL$attacktype1[which(SL$attacktype1==3)]<-0.339  #Bombing/Explosion
SL$attacktype1[which(SL$attacktype1==4)]<-0.061  #Hijacking
SL$attacktype1[which(SL$attacktype1==5)]<-0.415  #Hostage Taking (Barricade Incident)
SL$attacktype1[which(SL$attacktype1==6)]<-0.092  #Hostage Taking (Kidnapping)
SL$attacktype1[which(SL$attacktype1==7)]<-0.001  #Facility/Infrastructure Attack
SL$attacktype1[which(SL$attacktype1==8)]<-1.000  #Unarmed Assault
SL$attacktype1[which(SL$attacktype1==9)]<-0.275  #Unknown

aa<-function(y,m,d,at,pk,pw,pc,vk,vw,tk,tw){
  k<-matrix(NA,ncol=11,nrow=4071)   
  for(i in 1:4070) {
    if(y[i]==y[i+1] & m[i]==m[i+1] & d[i]==d[i+1]){
      at[i+1]<-max(at[i],at[i+1]);          #求最大值
      pk[i+1]<-pk[i]+pk[i+1];#死（perpetrator）
      pw[i+1]<-pw[i]+pw[i+1];#伤
      pc[i+1]<-pc[i]+pc[i+1];#抓
      vk[i+1]<-vk[i]+vk[i+1];#victim死
      vw[i+1]<-vw[i]+vw[i+1];#victim伤
      tk[i+1]<-tk[i]+tk[i+1];#总伤亡
      tw[i+1]<-tw[i]+tw[i+1]
    }
    else{
      a<-cbind(y[i],m[i],d[i],at[i],pk[i],pw[i],pc[i],vk[i],vw[i],tk[i],tw[i])
      k[i, ]=a
    }
  }
  return(k)
}

SL1<-aa(y<-SL$iyear,
        m<-SL$imonth,
        d<-SL$iday,
        at<-SL$attacktype1,
        pk<-SL$nkillter,
        pw<-SL$nwoundte,
        pc<-SL$nperpcap,
        vk<-SL$nkillv,
        vw<-SL$nwoundv,
        tk<-SL$nkill,
        tw<-SL$nwound) 
write.csv(SL1,"E:\\GTD\\adjust/SL/SL1.csv")



#--------------------#
#SL数据可视化处理#
#--------------------#
SL_score<-read.csv("E:\\GTD\\adjust/SL/Data_score.csv",1)
hh_score<-xts(SL_score$Score,as.Date(SL_score$Date,format='%Y/%m/%d'))
plot(hh_score,type = 'l',main='Talibna Efficiency ')  

eemd(hh)
imfs=eemd(hh)
ts.plot
ts.plot(rowSums(imfs[,1:ncol(imfs)]))
plot(imfs)
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


#-----------月频率统计---------------#
num_rep<-function(y,m,d){
  j<-matrix(1,nrow=4045,ncol = 1)
  k<-matrix(NA,ncol=4,nrow=4045)
  for(i in 1:4044) {
    if(y[i]==y[i+1] & m[i]==m[i+1]){
      j[i+1]=j[i]+1
    }
    else{
      a<-cbind(y[i],m[i],d[i],j[i])
      k[i, ]=a
    }
  }
  return(k)
}

rep1<-num_rep(y<-SL$iyear,
              m<-SL$imonth,
              d<-SL$iday)
write.csv(rep1,"E:\\GTD\\a_all/SL/mon_freq1.csv")             

#-------攻击类型翻转处理（便于观看）------------#
SL2<-read.csv("E:\\GTD\\a_all/SL/SL2.csv",1)
#攻击类型替换成得分
SL2$attacktype2[which(SL2$attacktype2==0.001)]<-"Hostage Taking (Kidnapping)"
SL2$attacktype2[which(SL2$attacktype2==0.084)]<-"Bombing/Explosion"
SL2$attacktype2[which(SL2$attacktype2==0.088)]<-"Hostage Taking (Barricade Incident)"
SL2$attacktype2[which(SL2$attacktype2==0.132)]<-"Hijacking"
SL2$attacktype2[which(SL2$attacktype2==0.333)]<-"Assassination"
SL2$attacktype2[which(SL2$attacktype2==0.364)]<- "Armed Assault"
SL2$attacktype2[which(SL2$attacktype2==0.393)]<- "Unknown"
SL2$attacktype2[which(SL2$attacktype2==0.492)]<-"Facility/Infrastructure Attack"
SL2$attacktype2[which(SL2$attacktype2==1)]<-"Unarmed Assault"  

write.csv(SL2,"E:\\GTD\\a_all/SL/SL3.csv")




#-------画频率图------------#
freq_mon<-read.csv("E:\\GTD\\a_all/SL/mon_freq.csv",1)        #月频率序列（少）
hh<-xts(freq_mon$freq,as.Date(freq_mon$DATE,format='%Y/%m/%d'))

freq_mon1<-read.csv("E:\\GTD\\a_all/SL/mon_freq1.csv",1)      #月频率序列（多）
hh1<-xts(freq_mon1$Freq,as.Date(freq_mon1$DATE,format='%Y/%m/%d'))

SL_score<-read.csv("E:\\GTD\\a_all/SL/Date_score.csv",1) #效率得分图（多）
hh_score<-xts(SL_score$Score,as.Date(SL_score$Date,format='%Y/%m/%d'))

plot(hh_score,type = 'l',main='Talibna Efficiency ') 
par(new=TRUE)
lines(hh1,col = "red",axis(4,col="red",col.ticks="red",col.axis="red"))


library(plotrix)
xpos <-SL_score$Date
y1<-SL_score$Score
y2<-freq_mon1$Freq
twoord.plot(xpos,y1,xpos,y2,
            lylim=c(0,2),rylim=c(0,200),
            lcol=4,rcol=2,xlab="DATE",ylab="Efficiency",rylab="frequency/(month)",
            xtickpos=as.numeric(SL_score$Date),
            xticklab = as.character(SL_score$Date),
            type=c("l","l"),halfwidth=0.2)
length(xtickpos=as.numeric(SL_score$Date))








ts.plot(hh,hh_score,
        gpars=list(xlab="time series",
                   ylab="frequency/month", 
                   lty=c(1:2),
                   axis(4,c(0,150),col = "violet", col.axis = "dark violet", lwd = 2))
)




#--------------------#
#SL数据 bootstrap DEA#
#--------------------#
rm(list=ls())
SL<-read.xlsx("E:\\GTD\\adjust/SL/SL2.xlsx",1)

framex <- data.frame(SL$attacktype,SL$nkillter,SL$nperpcap)
x<-as.matrix(framex)
framey <- data.frame(SL$nkillv,SL$nwoundv)
y<-as.matrix(framey)
is.numeric(y)
#------------------算DEA效率值----------------#
#-----dea------#
d<-dea(x,y)
dd<-d$eff
dd[1:30]
dd<-as.matrix(dd)
#----超效率dea------#   benchmarking 
e <- sdea(x,y,RTS = "vrs", ORIENTATION = "graph")
ee<-e$eff
head(ee)
ee[1:20]
#----超效率dea------#    TFDEA
e <- SDEA(x,y,rts="vrs", orientation="input")
etf<-e$eff
etf[753]
head(etf)
etf[1:200]
write.xlsx(etf,"E:\\GTD\\adjust/SL/TFDEA/b$eff.xlsx")


##--------------------一步到位---------------------------#
nrep <- 1000
# nrep <- 2000
??dea.boot

b <- dea.boot(x,y, NREP=nrep,RTS="vrs", XREF=x,YREF=y, EREF=ec$eff)
b
bb<-data.frame(b$eff,b$bias,b$eff.bc,b$var,b$conf.int,b$boot)
write.csv(bb,"E:\\GTD\\adjust/SL/bb.csv")
# write.xlsx(b$eff,"E:\\GTD\\adjust/SL/b/b$eff.xlsx") 原效率
# write.xlsx(b$eff.bc,"E:\\GTD\\adjust/SL/b/b$eff.bc.xlsx")修改后的效率
# write.xlsx(b$bias,"E:\\GTD\\adjust/SL/b/b$bias.xlsx")
# # write.xlsx(b$var,"E:\\GTD\\adjust/SL/b/b$var.xlsx")
# write.xlsx(b$conf.int,"E:\\GTD\\adjust/SL/b/b$conf.int.xlsx")
# write.csv(b$boot,"E:\\GTD\\adjust/SL/b/b$boot.csv")
b$eff
b$eff.bc








x1 <- matrix(c(1,2,3,4),ncol=1,dimnames=list(LETTERS[1:4],"X"))
y1 <- matrix(c(1,3,4,3),ncol=1,dimnames=list(LETTERS[1:4],"Y"))




#----------------线性规划-------------------------#
lps.model <- make.lp(2, 4)
set.row(lps.model, 2, c(6,2,4,9))
set.row(lps.model, 1, c(3,1,5), indices = c(1,2,4))
#对应第一、二、四个值，第三个默认设置为0
lps.model

f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
lp_s<-lp ("max", f.obj, f.con, f.dir, f.rhs)
lp_s$solution#关键
lp_s











summary(lp_s)
lp_s

f.con.d <- matrix (c(rep (1:2,each=3), rep (1:3, 2), t(f.con)), ncol=3)
lp ("max", f.obj,  ,f.dir, f.rhs, dense.const=f.con.d)
