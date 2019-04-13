library(Benchmarking)
library(TFDEA)

rm(list=ls())


data<-read.csv("c:/users/lenovo/desktop/famer/data.csv")
head(data)
names(data)[1]<-"year"

data$machine<-data$machine/data$family
data$fertilizer<-data$fertilizer/data$family
data$area<-data$area/data$family
data$animals<-data$animals/data$family
data$people<-data$people/data$family

data$revenue<-data$revenue/data$family



framex <- data.frame(data$machine,data$fertilizer,data$area,
                     data$animals,data$people)
x<-as.matrix(framex)

y<-as.matrix(data$revenue)

#综合效率VRS=技术效率CRS*规模效率
#------------------算DEA效率值----------------#
#-----dea------#
#纯技术效率
d1<-dea(x,y,RTS = "crs",SLACK=T)
d1_e<-d1$eff
d1$sx
#综合效率
d2<-dea(x,y,RTS = "vrs",SLACK=T)
d2_e<-d2$eff
d2$sx
d3<-sdea(x,y)
d3_e<-d3$eff

scale_e<-d1_e/d2_e

result<-data.frame(d3_e,d1_e,d2_e,scale_e)

write.csv(result,"c:/users/lenovo/desktop/famer/result1.csv")

# radial movement表示投入指标的松弛变量取值，即投入冗余值；
# slack movement 表示产出指标的松弛变量取值，即产出不足值 ;

