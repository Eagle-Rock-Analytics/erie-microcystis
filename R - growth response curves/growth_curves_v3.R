#Owen Doherty - March 14, 2018
#Eagle Rock Analytics

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
microsystis.short.df <- microsystis.df[ave(microsystis.df$x, microsystis.df$Study, FUN = length) > 4, ]

#drop questionable Van der Westhizen
microsystis.drop.df <- microsystis.short.df[!(microsystis.short.df$Study =="Van der Westhuizen & Eloff 1985"),]

#drop questionable Van der Westhizen & keep only North America
microsystis.na.df <- microsystis.df[!(microsystis.df$Study =="Van der Westhuizen & Eloff 1985"),]
microsystis.na.df <- microsystis.na.df[!(microsystis.na.df$Location =="Global"),]

#Create small array, drop an experiment, merge together 
le.sub.df<-le_in[, -c(2,6)]
le.sub.df <- le.sub.df[!(le.sub.df$Name =="Gobler-2h"),]
le.sub.df <- le.sub.df[!(le.sub.df$Name =="Gobler-2l"),]
microsystis.sub.df<-microsystis.na.df[,-c(2,3,7)]
colnames(microsystis.sub.df)[1] <- "Name"

merged.sub.df <-merge(microsystis.sub.df,le.sub.df,all=TRUE)

########
#Plot #1: Transformed vs. Temperature (raw)
########
library(ggplot2)
setwd('/Users/owen/Documents/research/hab_gl/gr_figs')

pdf("nh_polynomial_fits.pdf")  
  p <- ggplot(merged.sub.df,aes(x=Temperature,y=Transformed,color=Name))
  p+geom_point()+stat_smooth(method="lm",se=FALSE, fill=NA, formula=y~poly(x,4,raw=TRUE))+ylab("transformed growth rate (per day)")+xlab('temperature (C)')
dev.off()

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
