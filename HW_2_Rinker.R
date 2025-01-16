# rm(list = ls())
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(rio)
library(dplyr)
library(scales)
library(DescTools)
library(ggpmisc)
library(tibble)
# Loading data ------------------------
linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

arrests=rio::import(linkMass,which = 1)
codes = rio::import(linkMass,which = 2)
head(arrests)
str(arrests,width = 70,strict.width='cut')
names(arrests)
summary(arrests$`Arrest Date`)
# formatting:

arrests<-arrests%>%
 mutate (arrest_type = factor(case_when(`Arrest Type`== 'F' ~ 'Felony'
                                 ,`Arrest Type`== 'J' ~ 'Juvenile'
                                 ,`Arrest Type`== 'M' ~ 'Misdemeanor'
                                 ,`Arrest Type`== 'W' ~ 'Warrant'
                                 ,`Arrest Type`== 'O' ~ 'Other'
                                 ))
          )
table ( arrests$arrest_type)

## Keeping only complete data
arrests=arrests[complete.cases(arrests),]

# Exploring the data------------------


tapply(arrests$Age,arrests$arrest_type, summary)
class(arrests$Age)

BaseBox = ggplot (data =arrests, aes(x =arrest_type, y =  Age ))

boxplot = BaseBox+
          geom_boxplot(aes(x =reorder( arrest_type, Age , median  )))+
          coord_flip()
boxplot

#Density plots: ----------------------
denisty <-  ggplot(arrests) + 
              geom_density(aes(x = Age),show.legend = F) + 
              facet_grid(reorder(arrest_type,Age,median)~.) 

denisty
histogram = ggplot (data =arrests, aes( x =  Age )) +
      geom_histogram(color = 'lightgray', alpha = 0.5) + 
      facet_grid(reorder(arrest_type,Age,median)~.)

histogram

 # The histogram and density plot show that the data  is skewed, 
sk = Skew(arrests$Age, na.rm = T)

#Plotting error bars ------------------
baseMEANs=ggplot(arrests, aes(x=arrest_type,
                                    y=Age)) 

pointMEANS=baseMEANs + geom_point(stat="summary") 
pointMEANS 
ErrorPlot=pointMEANS + geom_errorbar(stat="summary") 
ErrorPlot

# Adding data points to visualize actual data 
baseMEANs= ggplot(arrests, aes(x=arrest_type,
                                     y=Age)) 
jitterMEANs= baseMEANs + 
            geom_jitter(colour="skyblue",
                        alpha=0.2 #transparency
)
jitterMEANs=jitterMEANs + 
            geom_point(stat="summary") +
            geom_errorbar(stat="summary",width=.2)
jitterMEANs 

# Violin plot -----------------------
BaseBox = ggplot (data =arrests, aes(x =arrest_type, y =  Age ))

vio = BaseBox +
      geom_violin(aes(x=arrest_type), fill="orange",trim = F)

vio + coord_flip() 

vio2= vio + 
  coord_flip() + 
  # scale_y_continuous(breaks = c(1,2,8)) + 
  theme(panel.grid.minor = element_blank())

vio2

#Adding stats to each vio ---------------------
tapply(arrests$Age, arrests$arrest_type, summary)$Misdemeanor[c(1,3,6)]%>%
  round(3) ->Misd_Min_Md_Mx

# Calculating median and max value for one category of arrest_type
Misd_Min_Md_Mx%>%as.list()%>%as.data.frame() ->Misd_Min_Md_Mx
names(Misd_Min_Md_Mx)=c('min', 'median','max')
trunc(Misd_Min_Md_Mx)

#Preparing annotation 
misd_T=tibble(x=2.1,y=75,tb=list(Misd_Min_Md_Mx))
# library(ggpmisc)
vio2 + geom_table(data=misd_T,
                  aes(x = x, 
                      y = y, 
                      label = tb))

# Creating a function for annotating each violin plot:------------
table_Annot=function(theValues,posX,posY){
  output=theValues[c(1,3,6)]%>%
          round(3) %>%
          as.list()%>%
          as.data.frame() 
  names(output)=c('min', 'median','max')
  # text_output=paste0(trunc(output),"y_",
  #                    trunc(12*output%%1),'m')
  # output[1,]=text_output
  
  tibble(x=posX,y=posY,tb=list(output))
}

Misd_input=tapply(arrests$Age,
               arrests$arrest_type,
               summary)$Misdemeanor

Fel_Input=tapply(arrests$Age,
                 arrests$arrest_type,
                 summary)$Felony
O_Input=tapply(arrests$Age,
               arrests$arrest_type,
               summary)$Other
W_Input=tapply(arrests$Age,
               arrests$arrest_type,
               summary)$Warrant


Misd_ann =table_Annot (Misd_input, 2.2,75)
Fel_annot=table_Annot (Fel_Input, 1.2,75)
Oth_annot=table_Annot (O_Input, 3.2,75)
War_annot=table_Annot (W_Input, 4.2,75)


vio2 + geom_table(data=Misd_ann,
                  aes(x = x, y = y, label = tb),vjust = 0.5) + 
  geom_table(data=Fel_annot,
             aes(x = x, y = y, label = tb),vjust = 0.5) + 
  geom_table(data=Oth_annot,
             aes(x = x, y = y, label = tb),vjust = 0.5) + 
  geom_table(data=War_annot,
             aes(x = x, y = y, label = tb),vjust = 0.5)  


#Creating a dot plot for stat values
class(arrests$Age)
summaryBy =aggregate(data=arrests,
                    Age ~arrest_type,
                    FUN = function(x) {c(min = min(x),
                                        median = median(x),
                                        max=max(x) 
                                        )
                      }
                    )

class(summaryBy)
#when several functions at play
summaryBy=do.call(data.frame,summaryBy)
summaryBy

# names(summaryBy)=c('Arrest_type','Min', 'Median','Max')


base = ggplot(data=summaryBy, aes(x=arrest_type))
str(summaryBy)

base + 
  geom_point(aes(y=Age.min),color='darkgreen') +
  geom_point(aes(y=Age.median),color='blue') +
  geom_point(aes(y=Age.max),color='red')

#turning the table in a long format :
summaryBy_long=reshape2::melt(summaryBy,variable.name = 'stats',
                              value.name = 'Age',
                              id.vars='arrest_type')

summaryBy_long <- summaryBy %>%
  pivot_longer (cols = starts_with("Age"), names_to = "stats", values_to ="stat.value")%>%
  mutate(stats =factor(stats,levels =c("Age.min", "Age.median", "Age.max" )))

class(summaryBy_long$stats)
levels(summaryBy_long$stats)
levels =c("Age.min", "Age.median", "Age.max" )

# Bar Plot: 
base=ggplot(data=summaryBy_long, aes(x=arrest_type)) + theme_light()
base + geom_point(aes(y=stat.value, color=stats))


base1=ggplot(data=summaryBy_long,
             aes(x=arrest_type , y=stat.value,
                 fill=stats)) # fill brings a legend


barDodge= base1 +  geom_bar(stat="identity",
                            position ='dodge') 

barDodge + geom_text(size = 4,
                     position = position_dodge(1),hjust=0,
                     aes(label=round(stat.value,1)))+
  coord_flip()
# Thinner bars: 

base1=ggplot(data=summaryBy_long,
             aes(x=arrest_type, y=stat.value,
                 fill=stats)) 
barDodge= base1 +  geom_bar(stat="identity",
                            position =position_dodge(.7),width = 0.4) 
barDodge + geom_text(size = 4,
                     position = position_dodge(0.7),hjust=0,
                     aes(label=round(stat.value,1))) +
  coord_flip()


# Use of faceted bars: 
bars=base +
    geom_bar(aes(y=stat.value,fill=stats),
                     stat = 'identity', width = 0.3,
                     show.legend = F) +
  geom_text(size = 6,
            position = position_dodge(0.7),hjust=1,
            aes(y=stat.value,
                label=round(stat.value,1))) 

bars + facet_grid(~stats) + coord_flip() 
  


#Segments:
bars=base + geom_point(aes(y=stat.value,fill=stats),
                       stat = 'identity',
                       show.legend = F) + 
  geom_text(size = 4,hjust=-0.5,
            aes(y=stat.value,
                label=round(stat.value,1))) +
  geom_segment(aes(y = 0, 
                   x = arrest_type, 
                   yend = stat.value, 
                   xend = arrest_type), 
               color = "grey50") 
bars =bars + facet_grid(~stats) + coord_flip() 
bars
# Moving text location: 
bars + scale_y_continuous(limits = c(0,100))

bars=base + 
  geom_text(size = 5,hjust=1,vjust=-0.1, 
            aes(y=stat.value,
                label=round(stat.value,1),
                color=stats),show.legend = F) +
  geom_segment(aes(y = 0, 
                   x = arrest_type, 
                   yend = stat.value, 
                   xend = arrest_type), 
               color = "grey50") +
  facet_grid(~stats) + coord_flip() 


#Customizing colors :  -----------------------------
bars<- bars + 
  geom_text(size = 5,hjust=1,vjust=-0.1, 
            aes(y=stat.value,
                label=round(stat.value,1),
                color=stats),
            show.legend = F, fontface='bold') +
  geom_segment(aes(y = 0, 
                   x = arrest_type, 
                   yend = stat.value, 
                   xend = arrest_type), 
               color = "grey50") +
  scale_colour_manual(values=c("black","red", "blue"))+
   facet_grid(~stats) + coord_flip() 


# Adding titles and  source :------------------------
titleText="Felonies Are Associated with Lower Age Groups"
sub_titleText='A Comparison of Arrest Types'
sourceText='Source: Massachusetts State Police arrest records'

bars =bars + 
  labs(title=titleText,
       subtitle = sub_titleText,
       # x =NULL,  #x.AxisText
       y = NULL, #y.AxisText
       caption = sourceText)

bars
#Removing redundant axis lables
final_plot <- bars+
              labs(title=titleText,
                   subtitle = sub_titleText,
                   # x =NULL,  #x.AxisText
                   y = NULL, #y.AxisText
                   caption = sourceText)+
              theme(
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())
final_plot

# Adjusting the order of arrest types:

arrests=rio::import(linkMass,which = 1)
codes = rio::import(linkMass,which = 2)
head(arrests)
str(arrests,width = 70,strict.width='cut')
names(arrests)
summary(arrests$`Arrest Date`)
# formatting:

arrests<-arrests%>%
  mutate (arrest_type = factor(case_when(`Arrest Type`== 'F' ~ 'Felony'
                                         ,`Arrest Type`== 'J' ~ 'Juvenile'
                                         ,`Arrest Type`== 'M' ~ 'Misdemeanor'
                                         ,`Arrest Type`== 'W' ~ 'Warrant'
                                         ,`Arrest Type`== 'O' ~ 'Other'
  ))
  )
table ( arrests$arrest_type)

## Keeping only complete data
arrests=arrests[complete.cases(arrests),]
summaryBy =aggregate(data=arrests,
                     Age ~arrest_type,
                     FUN = function(x) {c(min = min(x),
                                          median = median(x),
                                          max=max(x) 
                     )
                     }
)
summaryBy=do.call(data.frame,summaryBy)
summaryBy


summaryBy_long <- summaryBy %>%
  pivot_longer (cols = starts_with("Age"), names_to = "stats", values_to ="stat.value")%>%
  mutate(stats =factor(stats,levels =c("Age.min", "Age.median", "Age.max" )))
titleText="Felonies Are Associated with Lower Age Groups"
sub_titleText='A Comparison of Arrest Types'
sourceText='Source: Massachusetts State Police arrest records'



final_plot<- ggplot(data=summaryBy_long, aes(x =arrest_type))+ 
  theme_light()+ 
  geom_text(size = 5,hjust=1,vjust=-0.1,
            aes(y=stat.value,
                label=round(stat.value,1),
                color=stats),
            show.legend = F,
            fontface='bold') +
  geom_segment(aes(y = 0, 
                   x = arrest_type, 
                   yend = stat.value, 
                   xend = arrest_type), 
               color = "grey50") +
  scale_colour_manual(values=c("black","red", "blue"))+
  facet_grid(~stats) + 
  
  labs(title=titleText,
       subtitle = sub_titleText,
       x =NULL,  #x.AxisText
       y = NULL, #y.AxisText
       caption = sourceText)+
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank())+
  
  coord_flip()+
  scale_x_discrete(limits = levels(fct_reorder(summaryBy_long$arrest_type, 
                                               - summaryBy_long$stat.value, mean)))
final_plot

saveRDS(final_plot, file = "HW_2_Rinker.rds")





