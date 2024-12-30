# DACSS 690 V. data visualization. December -January 2025. 
# Diana Rinker  Homework 1
# Tabular data- Univariate Numerical



library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(DescTools)
library(ggpubr)
rm(list = ls()) # clean memory
# Loading data ----------------------
location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)
write.csv(eduwa, file = "eduwa_data.csv", row.names = FALSE)

#getting the data TABLE from the file in the cloud:
load(file=url(link))
dim(eduwa)
colnames(eduwa)
str(eduwa,width = 70,strict.width='cut')
str(eduwa)

eduwa$Student.Teacher.Ratio


unique_schools = length(unique(eduwa$NCES.School.ID))
missing_data =  sum(is.na(eduwa$Student.Teacher.Ratio))

summary(eduwa$Student.Teacher.Ratio)

# Producing a box plot: ---------------------
  base = ggplot (eduwa, aes (y=  Student.Teacher.Ratio))

# Adding titles and  source :
titleText='Teacher per student ratio '
sub_titleText='Washington State - 2019'
sourceText='Source: US Department of Education'


  base =base + 
        geom_boxplot() +
        labs(title=titleText,
             subtitle = sub_titleText,
              # x =NULL,  #x.AxisText
              y = NULL, #y.AxisText
              caption = sourceText)
         

      # Two values are standing out and are clearly a data entry error. I will exclude this from the data.
    eduwa <- eduwa %>%
    filter(Student.Teacher.Ratio<100)

    
  
  # get all the summary values but the count of NAs.
  (statVals=summary(eduwa$Student.Teacher.Ratio,digits = 3)[c(1,2,5, 6)])
  
    # the summary values as vector
  statVals=statVals %>%    
    as.vector() #notice '%>%'
  
b1 = base + 
    scale_y_continuous(breaks =statVals)
b1 = b1 + 
       coord_flip()
b1  
# Adding mean as a separate line, to avoid lables overlap on x-axis: 
md=Median(eduwa$Student.Teacher.Ratio,na.rm = T)
mn=Mean(eduwa$Student.Teacher.Ratio,na.rm = T)

# Mean and median are very close, but since data has a lot of outliers,  I will display the median.


# Adding median and annotating: 
txt_md=paste0('#Median: ',md)

b1_vertical = b1 + 
  geom_hline(yintercept = md,
             color='blue',
             linetype="dashed",
             size=1
  ) 
b1_annot= b1_vertical + 
  annotate(geom = 'text',
           label=txt_md,
           y = md-7,
           x=0.5,
           angle=0
           ,colour ='blue') # text angle



# Upper limit of the boxplot: 
(upperT=ggplot_build(b1)$data[[1]]$ymax)
(lowerT=ggplot_build(b1)$data[[1]]$ymin)


# Computing amount of outliers: 

txtUpper=paste0('Threshold:\n ',upperT)
txtLower=paste0('Threshold:\n ',lowerT)

(numOutliers_h=sum(eduwa$Student.Teacher.Ratio>upperT,na.rm = T))
(numOutliers_l=sum(eduwa$Student.Teacher.Ratio<lowerT,na.rm = T))
# Annotating: 
b2_vertical = b1_annot + 
              geom_hline(yintercept = upperT,
                        color='purple',
                        linetype="dotted",
                        size=1)+
              geom_hline(yintercept = lowerT,
                         color='purple',
                         linetype="dotted",
                         size=1)



b2_annot= b2_vertical + 
          annotate(geom = 'text',
                  label=txtUpper,
                  y = upperT+5,
                  x=0.2,
                  angle=0
                  ,colour ='purple') +
           annotate(geom = 'text',
                   label=txtLower,
                   y = lowerT-5,
                   x=0.2,
                   angle=0
                   ,colour ='purple') # text angle# text angle
        

txt_Outliers_h=paste0('#Outlying schools: ',numOutliers_h)
txt_Outliers_l=paste0('#Outlying \n schools:\n ',numOutliers_l)


b2_annot= b2_annot + 
          annotate(geom = 'text',
                   label=txt_Outliers_h,
                   y = upperT+30,
                   x=0.1,
                   angle=0
                   ,size =3)+
          annotate(geom = 'text',
                   label=txt_Outliers_l,
                   y = lowerT-5,
                   x=0.1,
                   angle=0
                   ,size =3)
b2_annot

# Removing values in the vertical axis and customizing the grid: 

b2_annot_noX = b2_annot + 
  theme_light() +
  theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) 
b2_annot_noX

# Customizing the text on x-axis: 
b2_newGrid_axisText = b2_annot_noX + 
                      theme(axis.text.x = element_text(angle = 60,
                                                       size = 10,
                                                       vjust = 0.5)
                            )
b2_newGrid_axisText

# Statistical calculations: ---------------
# Supplementing summary with measures of spread and dispersion: 

md=Median(eduwa$Student.Teacher.Ratio,na.rm = T)
mn=Mean(eduwa$Student.Teacher.Ratio,na.rm = T)

# coefficient of variation

cv=CoefVar(eduwa$Student.Teacher.Ratio,na.rm = T)
sd=SD(eduwa$Student.Teacher.Ratio,na.rm = T)


# confidence interval for the mean
mn.up = MeanCI(eduwa$Student.Teacher.Ratio,
       na.rm = T)
mn.low=MeanCI(eduwa$Student.Teacher.Ratio,
              na.rm = T)[['lwr.ci']]

# kurtosis and skew: 
Kurt(eduwa$Student.Teacher.Ratio,
     na.rm = T) 
sk=Skew(eduwa$Student.Teacher.Ratio,
        na.rm = T)

# This data us hughly concentrated around the center and skewed to the right. 

# Histogram: -------------------------
range(eduwa$Student.Teacher.Ratio)
barWIDTH=2
library(ggplot2)
base= ggplot(eduwa)  
h1= base + 
    geom_histogram(aes(x = Student.Teacher.Ratio),
                      binwidth = barWIDTH,
                      fill='brown') 
h1= h1 + 
    labs(y="Count")
h1

# Adding annotations: 
# Texts
txtMean=paste0('Mean:',round(mn))
txtSkew=paste0('Skewness:',round(sk,2))

h1_ann = h1+
        geom_vline(xintercept = mn, color='black') + # mean as line
  # about the mean
         annotate(geom = 'text',color='red',
                 label=txtMean, # mean as text
                 y = 400,
                 x=mn+6,
                 angle=90) + 
  # about the skewness
          annotate(geom = 'text', color='blue',
           label=txtSkew, # skewness as text
           y = 50,
           x=upperT+20,
           angle=0) 

h1_ann 


# Combining  histogram and boxplot in one visual: ---------------

install.packages ("ggpubr")


ggarrange(b1_newGrid_axisText, h1_ann, align='v',ncol = 1,heights = 3:2)

# Alternative violin plot: --------------------

base=ggplot(eduwa, aes(x=0,y=Student.Teacher.Ratio))+
  theme_classic()

vio=base+
  geom_violin(trim=FALSE, fill="grey")+
  labs(title=titleText,
        subtitle = sub_titleText,
        # x =NULL,  #x.AxisText
        y = NULL, #y.AxisText
        caption = sourceText)

viobox=vio+
       geom_boxplot(width=0.1,
                     outlier.color = 'red',
                     colour = 'white',
                     fill='purple')

viobox_ann=viobox + 
          coord_flip() + 
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank()) +
        annotate(geom = 'text',
                label=txt_Outliers_h,
                y = upperT+60,
                x=0.1,color='red',
                angle=0)+
        annotate(geom = 'text',
                label=txt_Outliers_l,
                y = lowerT -5,
                x=0.1,color='red',
                angle=0)+
        geom_hline(yintercept = upperT,
                   color='purple',
                   linetype="dotted",
                   size=1)+
        geom_hline(yintercept = lowerT,
                   color='purple',
                   linetype="dotted",
                   size=1)+
        annotate(geom = 'text',
                 label=txtUpper,
                 y = upperT+5,
                 x=0.2,
                 angle=0
                 ,colour ='purple') +
        annotate(geom = 'text',
                 label=txtLower,
                 y = lowerT-5,
                 x=0.2,
                 angle=0
                 ,colour ='purple')+ # text angle# text angle 
        geom_hline(yintercept = md,
                     color='blue',
                     linetype="dashed",
                     size=1) + 
          annotate(geom = 'text',
                     label=txt_md,
                     y = md-7,
                     x=0.5,
                     angle=0
                     ,colour ='blue') # text angle


viobox_ann
saveRDS(viobox_ann, file = "del1Draft.rds")





