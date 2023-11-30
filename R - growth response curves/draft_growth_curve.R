#Owen Doherty - March 8, 2018
#Eagle Rock Analytics

#############################################
#Load Data - accounting for wierd file types
#############################################

#Go to directory with raw data
setwd('/Users/owen/Documents/research/hab_gl/data')
list.files() #print list of data files

library("readxl")
m_in <- read_excel("microsystis_tagged_3.8.18.xlsx")
microsystis.df <- data.frame(m_in,x=as.numeric(1:dim(m_in)[1]))

#Simplify some names
colnames(microsystis.df)[4] <- "Temperature"
colnames(microsystis.df)[5] <- "Growth.Rate"
colnames(microsystis.df)[6] <- "Scaled.Rate"

#subset to include only experiments with more than 4 temperature points
#df[ave(df$x, df$name, FUN = length) < 3, ]
microsystis.short.df <- microsystis.df[ave(microsystis.df$x, microsystis.df$Study, FUN = length) > 4, ]

#drop questionable Van der Westhizen
microsystis.drop.df <- microsystis.short.df[!(microsystis.short.df$Study =="Van der Westhuizen & Eloff 1985"),]

#drop questionable Van der Westhizen & keep only North America
microsystis.na.df <- microsystis.df[!(microsystis.df$Study =="Van der Westhuizen & Eloff 1985"),]
microsystis.na.df <- microsystis.na.df[!(microsystis.na.df$Location =="Global"),]

########
#Plot #1: Growth rate vs. Temperature
########
library(ggplot2)
setwd('/Users/owen/Documents/research/hab_gl/gr_figs')

pdf("base_gr_temp.pdf")
  p <- ggplot(microsystis.df,aes(x=Temperature,y=Growth.Rate,color=Study))
  p+geom_line()+geom_point()+ylab('growth rate (per day)')+xlab('temperature (C)')+ggtitle('microsystis growth rate by temperature')
dev.off()

pdf("base_gr_temp_faceted.pdf")
  microsystis.df$locfac <- factor(microsystis.df$Location)
  p <- ggplot(microsystis.df,aes(x=Temperature,y=Growth.Rate,color=Study))
  p+geom_point()+facet_grid(. ~ locfac)+ylab('growth rate (per day)')+xlab('temperature (C)')+ggtitle('microsystis growth rate by temperature')
dev.off()

########
#Plot #1a: Growth rate vs. Temperature + Fitted
########

pdf("base_fitted_gr_temp.pdf")
p <- ggplot(microsystis.short.df,aes(x=microsystis.short.df$Temp....C.,y=microsystis.short.df$Specific.growth.rate..d...,color=Study))
  p+geom_point()+stat_smooth()+ylab('growth rate (per day)')+xlab('temperature (C)')+ggtitle('microsystis growth rate by temperature with fitted curves')
dev.off()

#####
#Plot #2: Scaled
#####
pdf("scaled_gr_temp.pdf")
  p <- ggplot(microsystis.short.df,aes(x=microsystis.short.df$Temp....C.,y=microsystis.short.df$Scaled.ratio....of.max.,color=Study))
  p+geom_point()+geom_line()+ylab("max experimental growth ratio (%)")+xlab('temperature (C)')
dev.off()

pdf("scaled_gr_temp_faceted.pdf")
  p <- ggplot(microsystis.short.df,aes(x=Temperature,y=Scaled.Rate,color=Study))
  p+geom_point()+stat_smooth()+facet_grid(. ~ Location)+ylab("max experimental growth ratio (%)")+xlab('temperature (C)')
dev.off()
            
#####
#Plot 3: Scaled
#####    
pdf("scaled_fitted_dropped_gr_temp2.pdf")
  p <- ggplot(microsystis.drop.df,aes(x=microsystis.drop.df$Temp....C.,y=microsystis.drop.df$Scaled.ratio....of.max.,color=Study))
  p+geom_point()+ stat_smooth()+ylab('max experimental growth ratio (%)')+xlab('temperature (C)')+ggtitle('fitted curves to scaled growth rates, outliers removed')
dev.off()

pdf("scaled_fitted_dropped_gr_temp_na.pdf")
  p <- ggplot(microsystis.na.df,aes(x=microsystis.na.df$Temp....C.,y=microsystis.na.df$Scaled.ratio....of.max.,color=Study))
  p+geom_point()+ stat_smooth()+ylab('max experimental growth ratio (%)')+xlab('temperature (C)')+ggtitle('fitted curves to scaled growth rates, outliers removed')
dev.off()
            #+stat_smooth(method="lm", se=FALSE, fill=NA, formula=y~poly(x,4,raw=TRUE), parse=TRUE,aes(color=Study))             
            