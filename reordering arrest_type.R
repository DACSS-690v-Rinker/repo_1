# Trying to adjust the order of arrest_type ---------------------------
rm(list = ls())
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

