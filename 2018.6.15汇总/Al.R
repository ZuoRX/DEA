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

#------------------#
#AI-Qaida数据优化处理#
#------------------#
AI-Qaida<-read.csv("E:\\GTD\\adjust/AI-Qaida/AI-Qaida.csv",1)
#攻击类型替换成得分
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==1)]<-0.074  #Assassination
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==2)]<-0.265  #Armed Assault
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==3)]<-0.345  #Bombing/Explosion
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==4)]<-0.070  #Hijacking
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==5)]<-0.365  #Hostage Taking (Barricade Incident)
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==6)]<-0.103  #Hostage Taking (Kidnapping)
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==7)]<-0.001  #Facility/Infrastructure Attack
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==8)]<-1.000  #Unarmed Assault
AI-Qaida$attacktype1[which(AI-Qaida$attacktype1==9)]<-0.282  #Unknown

aa<-function(y,m,d,at,pk,pw,pc,vk,vw,tk,tw){
  k<-matrix(NA,ncol=11,nrow=4045)   
  for(i in 1:4044) {
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

AI-Qaida1<-aa(y<-AI-Qaida$iyear,
              m<-AI-Qaida$imonth,
              d<-AI-Qaida$iday,
              at<-AI-Qaida$attacktype1,
              pk<-AI-Qaida$nkillter,
              pw<-AI-Qaida$nwoundte,
              pc<-AI-Qaida$nperpcap,
              vk<-AI-Qaida$nkillv,
              vw<-AI-Qaida$nwoundv,
              tk<-AI-Qaida$nkill,
              tw<-AI-Qaida$nwound) 
write.csv(AI-Qaida1,"E:\\GTD\\adjust/AI-Qaida/AI-Qaida1.csv")



#--------------------#
#塔利班数据可视化处理#
#--------------------#
AI-Qaida_score<-read.csv("E:\\GTD\\a_all/AI-Qaida/Date_score.csv",1)
hh_score<-xts(AI-Qaida_score$Score,as.Date(AI-Qaida_score$Date,format='%Y/%m/%d'))
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

rep1<-num_rep(y<-AI-Qaida$iyear,
              m<-AI-Qaida$imonth,
              d<-AI-Qaida$iday)
write.csv(rep1,"E:\\GTD\\a_all/AI-Qaida/mon_freq1.csv")             

#-------攻击类型翻转处理（便于观看）------------#
AI-Qaida2<-read.csv("E:\\GTD\\a_all/AI-Qaida/AI-Qaida2.csv",1)
#攻击类型替换成得分
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.001)]<-"Hostage Taking (Kidnapping)"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.084)]<-"Bombing/Explosion"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.088)]<-"Hostage Taking (Barricade Incident)"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.132)]<-"Hijacking"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.333)]<-"Assassination"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.364)]<- "Armed Assault"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.393)]<- "Unknown"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==0.492)]<-"Facility/Infrastructure Attack"
AI-Qaida2$attacktype2[which(AI-Qaida2$attacktype2==1)]<-"Unarmed Assault"  

write.csv(AI-Qaida2,"E:\\GTD\\a_all/AI-Qaida/AI-Qaida3.csv")



#-------画频率图------------#
freq_mon<-read.csv("E:\\GTD\\a_all/AI-Qaida/mon_freq.csv",1)        #月频率序列（少）
hh<-xts(freq_mon$freq,as.Date(freq_mon$DATE,format='%Y/%m/%d'))

freq_mon1<-read.csv("E:\\GTD\\a_all/AI-Qaida/mon_freq1.csv",1)      #月频率序列（多）
hh1<-xts(freq_mon1$Freq,as.Date(freq_mon1$DATE,format='%Y/%m/%d'))

AI-Qaida_score<-read.csv("E:\\GTD\\a_all/AI-Qaida/Date_score.csv",1) #效率得分图（多）
hh_score<-xts(AI-Qaida_score$Score,as.Date(AI-Qaida_score$Date,format='%Y/%m/%d'))

plot(hh_score,type = 'l',main='Talibna Efficiency ') 
par(new=TRUE)
lines(hh1,col = "red",axis(4,col="red",col.ticks="red",col.axis="red"))


library(plotrix)
xpos <-AI-Qaida_score$Date
y1<-AI-Qaida_score$Score
y2<-freq_mon1$Freq
twoord.plot(xpos,y1,xpos,y2,
            lylim=c(0,2),rylim=c(0,200),
            lcol=4,rcol=2,xlab="DATE",ylab="Efficiency",rylab="frequency/(month)",
            xtickpos=as.numeric(AI-Qaida_score$Date),
            xticklab = as.character(AI-Qaida_score$Date),
            type=c("l","l"),halfwidth=0.2)
length(xtickpos=as.numeric(AI-Qaida_score$Date))








ts.plot(hh,hh_score,
        gpars=list(xlab="time series",
                   ylab="frequency/month", 
                   lty=c(1:2),
                   axis(4,c(0,150),col = "violet", col.axis = "dark violet", lwd = 2))
)
