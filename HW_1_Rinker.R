# DACSS 690 V. data visualization. December -January 2025. 
# Diana Rinker  Homework 1
# Tabular data- Univariate Numerical



library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

rm(list = ls()) # clean memory
# Loading data ----------------------
location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)


#getting the data TABLE from the file in the cloud:
load(file=url(link))
dim(eduwa)
colnames(eduwa)
str(eduwa,width = 70,strict.width='cut')
str(eduwa)
eduwa$Reduced.Lunch
eduwa$Free.Lunch


length(unique(eduwa$Free.Lunch))
sum(is.na(eduwa$Free.Lunch))

summary(eduwa$Free.Lunch)


# Producing a box plot: ---------------------
  base = ggplot (eduwa, aes (y=  Free.Lunch))
  base + geom_boxplot()
  
  # get all the summary values but the count of NAs.
  (statVals=summary(eduwa$Free.Lunch,digits = 3)[1:6])
  
  
  library(magrittr)
  # the summary values as vector
  statVals=statVals %>%    
    as.vector() #notice '%>%'
  
  base = ggplot (eduwa, aes (y = Free.Lunch))
  
  b1 = base + geom_boxplot ()
  b1 = b1 + scale_y_continuous(breaks =statVals)
b1 = b1 + coord_flip()
b1  


# Upper limit of the boxplot: 
(upperT=ggplot_build(b1)$data[[1]]$ymax)

# Computing amount of outliers: 
  (numOutliers=sum(eduwa$Free.Lunch>upperT,na.rm = T))

# Annotating: 

txtOutliers=paste0('#Outlying schools: ',numOutliers)
txtUpper=paste0('Threshold:',upperT)


b1_vertical = b1 + geom_hline(yintercept = upperT,
                              color='purple',
                              linetype="dotted",
                              size=2) 
b1_annot=b1_vertical + annotate(geom = 'text',
                                label=txtUpper,
                                y = upperT+5,
                                x=0.2,
                                angle=90) # text angle

b1_annot=b1_annot + annotate(geom = 'text',
                             label=txtOutliers,
                             y = upperT+60,
                             x=0.1,
                             angle=0)
b1_annot


# Removing values in the vertical axis and customizing the grid: 

b1_annot_noX = b1_annot + 
  theme_light() +
  theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) 

b1_annot_noX

# Customaizing the text: 

b1_newGrid_axisText = b1_annot_noX+ 
                      theme(axis.text.x = element_text(angle = 60,
                                                       size = 10,
                                                       vjust = 0.5)
                            )
b1_newGrid_axisText

# Statistical calculations: ---------------
# Supplementing summary with measures of spread and dispersion: 

md=Median(eduwa$Free.Lunch,na.rm = T)
mn=Mean(eduwa$Free.Lunch,na.rm = T)

# coefficient of variation
library(DescTools)
cv=CoefVar(eduwa$Free.Lunch,na.rm = T)
sd=SD(eduwa$Free.Lunch,na.rm = T)


# confidence interval for the mean
mn.up = MeanCI(eduwa$Free.Lunch,
       na.rm = T)
mn.low=MeanCI(eduwa$Free.Lunch,
              na.rm = T)[['lwr.ci']]

# kurtosis and skew: 
Kurt(eduwa$Free.Lunch,
     na.rm = T) 
sk=Skew(eduwa$Free.Lunch,
        na.rm = T)


# Histogram: -------------------------
barWIDTH=50
library(ggplot2)
base= ggplot(eduwa)  
h1= base + 
    geom_histogram(aes(x = Free.Lunch),
                      binwidth = barWIDTH,
                      fill='brown') 
h1= h1 + 
    labs(y="count")

h1
# Adding annotations: 

# texts
txtMean=paste0('Mean:',round(mn))
txtSkew=paste0('Skewness:',round(sk,2))


h1_ann = h1+
        geom_vline(xintercept = mn,color='red') + # mean as line
  # about the mean
         annotate(geom = 'text',color='red',
                 label=txtMean, # mean as text
                 y = 400,
                 x=mn+5,
                 angle=90) + 
  # about the skewness
          annotate(geom = 'text', color='blue',
           label=txtSkew, # skewness as text
           y = 50,
           x=upperT+170,
           angle=0) 

h1_ann 

# Combining  histogram and boxplot in one visual: ---------------

install.packages ("ggpubr")
library(ggpubr)

ggarrange(h1_ann,b1_newGrid_axisText,align='v',ncol = 1,heights = 2:4)

# alternative violin plot: 
base=ggplot(eduwa, aes(x=0,y=Free.Lunch))+
  theme_classic()

vio=base+
  geom_violin(trim=FALSE, fill="grey")

viobox=vio+
  geom_boxplot(width=0.1,
               outlier.color = 'red',
               colour = 'white',
               fill='purple')


viobox=viobox + coord_flip() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate(geom = 'text',
         label=txtOutliers,
         y = upperT+60,
         x=0.1,color='red',
         angle=0)
viobox





