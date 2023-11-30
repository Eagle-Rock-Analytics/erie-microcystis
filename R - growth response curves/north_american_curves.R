#Owen Doherty - March 16, 2018
#Eagle Rock Analytics

# Generate 1 curve per experiment
# Create data.frame of polynomial fit values

#Go to directory with raw data
setwd('/Users/owen/Documents/research/hab_gl/data')
list.files() #print list of data files

library("readxl")
m_in <- read_excel("microsystis_tagged_3.8.18.xlsx")
le_in <- read_excel("erie_results_formatted.xlsx")
microsystis.df <- data.frame(m_in,x=as.numeric(1:dim(m_in)[1]))

#Simplify some names of microsystis
colnames(microsystis.df)[4] <- "Temperature"
colnames(microsystis.df)[5] <- "Growth.Rate"
colnames(microsystis.df)[6] <- "Scaled.Rate"

#Simplify some names of lake erie
colnames(le_in)[3] <- "Temperature"
colnames(le_in)[4] <- "Growth.Rate"
colnames(le_in)[5] <- "Scaled.Rate"

#Create a transformed column
base_rate = max(le_in$Growth.Rate)

#base rate times scaled growth rate
microsystis.df$Transformed = base_rate*microsystis.df$Scaled.Rate
le_in$Transformed = base_rate*le_in$Scaled.Rate

#subset to include only experiments with more than 4 temperature points
#df[ave(df$x, df$name, FUN = length) < 3, ]
#microsystis.short.df <- microsystis.df[ave(microsystis.df$x, microsystis.df$Study, FUN = length) > 4, ]

#drop questionable Van der Westhizen
#microsystis.drop.df <- microsystis.short.df[!(microsystis.short.df$Study =="Van der Westhuizen & Eloff 1985"),]

#drop questionable Van der Westhizen & keep only North America
microsystis.na.df <- microsystis.df[!(microsystis.df$Study =="Van der Westhuizen & Eloff 1985"),]
microsystis.na.df <- microsystis.na.df[!(microsystis.na.df$Location =="Global"),] #drop global values from data.frame

#Create small array, drop an experiment, merge together 
le.sub.df<-le_in[, -c(2,6)]
#le.sub.df <- le.sub.df[!(le.sub.df$Name =="Gobler-2h"),]
#le.sub.df <- le.sub.df[!(le.sub.df$Name =="Gobler-2l"),]
microsystis.sub.df<-microsystis.na.df[,-c(2,3,7)]
colnames(microsystis.sub.df)[1] <- "Name"

merged.sub.df <-merge(microsystis.sub.df,le.sub.df,all=TRUE)

######
#Fit a polynomial to each study
######

merged.sub.df$Namef <- factor(merged.sub.df$Name) #Study name as factor
experiment.names<-unique(merged.sub.df$Name)

#Run a polynmomial fit on each experiment
require(plyr)
modelList_4 <- dlply(merged.sub.df, "Namef", function(x) lm(Transformed~poly(Temperature,4,raw=TRUE), data=x))

#Create a curve for each experiment and for each temperature between 0 and 40
pred_temps <- 0:40
newData <- expand.grid(experiment.names,pred_temps)
names(newData) <- c("Experiment.Name","Temperature") 
newData <-as.data.frame(newData)

pred_4 <- ddply(newData, "Experiment.Name", function(x)
  transform(x, value=predict(modelList_4[[paste(x$Experiment.Name[1])]], newdata=x, interval="confidence", level=0.95)))

#Test fittings
library(ggplot2)
ggplot(pred_4,aes(y=value.fit,x=Temperature,color=Experiment.Name))+geom_line()+ylim(-0.05,0.45)
ggplot(pred_4,aes(y=value.fit,x=Temperature,color=Experiment.Name))+geom_line()+facet_grid(.~Experiment.Name,as.table=FALSE)+xlim(5,40)+ylim(-0.05,0.45)+theme(legend.position="bottom")

######
# Drop all experiments that don't have a 4th order fit
######
kept_pred_4 <- pred_4[!(pred_4$Experiment.Name =="Chalifour & Juneau 2013a"),]
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Chalifour & Juneau 2013b"),]
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Coles & Jones 2000a"),]
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Gobler-1h"),]
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Gobler-1l"),]
ggplot(kept_pred_4,aes(y=value.fit,x=Temperature,color=Experiment.Name))+geom_line()+facet_grid(.~Experiment.Name,as.table=FALSE)+theme(legend.position="bottom")

#Drop all experiments that blow up near zero
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Gobler-3l"),]
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Gobler-4h"),]
kept_pred_4 <- kept_pred_4[!(kept_pred_4$Experiment.Name =="Gobler-4l"),]
ggplot(kept_pred_4,aes(y=value.fit,x=Temperature,color=Experiment.Name))+geom_line()+facet_grid(.~Experiment.Name,as.table=FALSE)+theme(legend.position="bottom")

#####
# Resample one value per temperature
#####
nboot = 10000

gr_boot = seq(0,40)

#this produces an array of indicies, random experiment per temperature value 0 to 40
for (j in 1:nboot){
  ind <- sapply( unique( pred_4$Temperature ) , function(x) sample( which(kept_pred_4$Temperature==x) , 1 ) )
  boot_out <- kept_pred_4[ind,c('value.fit')]
  
  gr_boot=rbind(gr_boot,boot_out)
  }

#gr_boot is size nboot+1 by number of temperature points (41)
#remove temperature array, the attach into a dataframe of dim (temp, boot_num, growth_rate)
myGr <-expand.grid(seq(0,40),seq(1,nboot))
names(myGr) <- c("temperature","boot_num")
myGr$growth_rate <-as.vector(t(gr_boot[1:nboot+1,]))

#make a curve for each resampled datapoint
modelList_4 <- dlply(myGr, "boot_num", function(x) lm(growth_rate~poly(temperature,4,raw=TRUE), data=x))

#Create matrix of desired fits for each curve (1 per boot_num)
pred_temps <- 0:40
newData <- expand.grid(1:nboot,pred_temps)
names(newData) <- c("boot_num","temperature") 
newData <-as.data.frame(newData)
resampled_pred_4 <- ddply(newData, "boot_num", function(x)
  transform(x, value=predict(modelList_4[[paste(x$boot_num[1])]], newdata=x, interval="confidence", level=0.95)))

pred_4.df <-as.data.frame(resampled_pred_4)
median.pred_4=aggregate(pred_4.df,by=list(pred_4.df$temperature),FUN=median)[2:6]
q975.pred_4=aggregate(pred_4.df,by=list(pred_4.df$temperature),FUN=quantile, probs=0.975)[2:6]
q025.pred_4=aggregate(pred_4.df,by=list(pred_4.df$temperature),FUN=quantile, probs=0.025)[2:6]
median.pred_4$qhi=q975.pred_4$value.fit
median.pred_4$qlo=q025.pred_4$value.fit

ggplot(median.pred_4, aes(x = temperature, y = value.fit)) +
  theme_bw() +
  geom_line() +
  labs(title="Microsystis (N.America), 4th Order - Resampled")+
  geom_smooth(aes(ymin = qlo, ymax = qhi), stat = "identity") +
  ylab('growth rate') + ylim(-1,1)+
  geom_point(data = merged.sub.df, aes(x = Temperature, y = Transformed, color=Name))
  #coord_cartesian(ylim = c(pmin,pmax))

########
#Plot #1: Model Curve
########
library(ggplot2)
setwd('/Users/owen/Documents/research/hab_gl/gr_figs')



########
#Old Code
########

# pdf("nh_polynomial_fits.pdf")  
# p <- ggplot(merged.sub.df,aes(x=Temperature,y=Transformed,color=Name))
# p+geom_point()+stat_smooth(method="lm",se=FALSE, fill=NA, formula=y~poly(x,4,raw=TRUE))+ylab("transformed growth rate (per day)")+xlab('temperature (C)')
# dev.off()

# ########
# #Plot #1: Growth rate vs. Temperature (raw)
# ########
# library(ggplot2)
# setwd('/Users/owen/Documents/research/hab_gl/gr_figs')
# 
# p <- ggplot(le_in,aes(x=Temperature,y=Growth.Rate,color=Name))
# p + geom_point() + stat_smooth() + 
#   geom_point(data=microsystis.df,aes(x=Temperature,y=Growth.Rate),color="grey")+
#   ylab("raw growth ratio (per day)")+xlab('temperature (C)')+ggtitle('Raw Data')
# 
# pdf("lake_erie_raw.pdf")  
# p <- ggplot(le_in,aes(x=Temperature,y=Growth.Rate,color=Name))
# p + geom_point() + stat_smooth() + 
#   facet_grid(. ~ Nitrogen) +
#   geom_point(data=microsystis.df,aes(x=Temperature,y=Growth.Rate),color="grey")+
#   ylab("raw growth ratio (per day)")+xlab('temperature (C)')+ggtitle('Raw Data')
# dev.off()
# 
# ########
# #Plot #2: Scaled rate vs. Temperature (raw)
# ########
# p <- ggplot(le_in,aes(x=Temperature,y=Scaled.Rate,color=Name))
# p + geom_point() + stat_smooth() + 
#   geom_point(data=microsystis.df,aes(x=Temperature,y=Scaled.Rate),color="grey")+
#   ylab("scaled growth ratio (%)")+xlab('temperature (C)')+ggtitle('Scaled Data')
# 
# pdf("lake_erie_scaled.pdf")    
# p <- ggplot(le_in,aes(x=Temperature,y=Scaled.Rate,color=Name))
# p + geom_point() + stat_smooth() + 
#   facet_grid(. ~ Nitrogen) +
#   geom_point(data=microsystis.df,aes(x=Temperature,y=Scaled.Rate),color="grey")+
#   ylab("scaled growth ratio (%)")+xlab('temperature (C)')+ggtitle('Scaled Data')
# dev.off()
# 
# ########
# #Plot #3: Transformed rate vs. Temperature (raw)
# ########
# 
# p <- ggplot(le_in,aes(x=Temperature,y=Transformed,color=Name))
# p + geom_point() + stat_smooth() + 
#   #facet_grid(Experiment ~ Nitrogen) +
#   geom_point(data=microsystis.df,aes(x=Temperature,y=Transformed),color="grey")+
#   ylab("transformed growth ratio (per day)")+xlab('temperature (C)')+ggtitle('Transformed Data')
# 
# pdf("lake_erie_transformed.pdf")     
# p <- ggplot(le_in,aes(x=Temperature,y=Transformed,color=Name))
# p + geom_point() + stat_smooth() + 
#   facet_grid(. ~ Nitrogen) +
#   geom_point(data=microsystis.df,aes(x=Temperature,y=Transformed),color="grey")+
#   ylab("transformed growth ratio (per day)")+xlab('temperature (C)')+ggtitle('Transformed Data')
# dev.off()  

########
#Plot #1a: Growth rate vs. Temperature + Fitted
########
# 
# pdf("base_fitted_gr_temp.pdf")
# p <- ggplot(microsystis.short.df,aes(x=microsystis.short.df$Temp....C.,y=microsystis.short.df$Specific.growth.rate..d...,color=Study))
# p+geom_point()+stat_smooth()+ylab('growth rate (per day)')+xlab('temperature (C)')+ggtitle('microsystis growth rate by temperature with fitted curves')
# dev.off()
# 
# #####
# #Plot #2: Scaled
# #####
# pdf("scaled_gr_temp.pdf")
# p <- ggplot(microsystis.short.df,aes(x=microsystis.short.df$Temp....C.,y=microsystis.short.df$Scaled.ratio....of.max.,color=Study))
# p+geom_point()+geom_line()+ylab("max experimental growth ratio (%)")+xlab('temperature (C)')
# dev.off()
# 
# pdf("scaled_gr_temp_faceted.pdf")
# p <- ggplot(microsystis.short.df,aes(x=Temperature,y=Scaled.Rate,color=Study))
# p+geom_point()+stat_smooth()+facet_grid(. ~ Location)+ylab("max experimental growth ratio (%)")+xlab('temperature (C)')
# dev.off()
# 
# #####
# #Plot 3: Scaled
# #####    
# pdf("scaled_fitted_dropped_gr_temp2.pdf")
# p <- ggplot(microsystis.drop.df,aes(x=microsystis.drop.df$Temp....C.,y=microsystis.drop.df$Scaled.ratio....of.max.,color=Study))
# p+geom_point()+ stat_smooth()+ylab('max experimental growth ratio (%)')+xlab('temperature (C)')+ggtitle('fitted curves to scaled growth rates, outliers removed')
# dev.off()
# 
# pdf("scaled_fitted_dropped_gr_temp_na.pdf")
# p <- ggplot(microsystis.na.df,aes(x=microsystis.na.df$Temp....C.,y=microsystis.na.df$Scaled.ratio....of.max.,color=Study))
# p+geom_point()+ stat_smooth()+ylab('max experimental growth ratio (%)')+xlab('temperature (C)')+ggtitle('fitted curves to scaled growth rates, outliers removed')
# dev.off()
# #+stat_smooth(method="lm", se=FALSE, fill=NA, formula=y~poly(x,4,raw=TRUE), parse=TRUE,aes(color=Study))             
