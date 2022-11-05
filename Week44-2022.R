#Nov 2022|| TidyTuesday week 44|| Horror movies
#~~~~~~~~~~ LIBRARIES AND SETTINGS
#Load libraries 
library(tidyverse); library(viridis);library(extrafont);library(sysfonts)
#Load creepy font family
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)
font_add_google(name="Creepster",family="Creepster")
#~~~~~~~~~~ DATA & FILTERING
#Load data
horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')
language_codes_csv <- read_csv("language-codes_csv.csv") #load dictionary (to get the long name of the languages)
#group and count by language
langcount<-horror_movies %>% na.omit() %>% group_by(original_language) %>% count("original_language")
langcount<-langcount[-4,] #Filter out English
colnames(langcount)<-c("alpha2","n") #reset colames

m1 <- merge(langcount, language_codes_csv, by.x = "alpha2") #Merge datafrase with the dictionary (for the language names)
m1<-m1[-1,] #remove the unidentified "cn" -> chinese 
m1[20,2]=m1[20,2]+2 # Add the 2 movies coded cn
colnames(m1)<-c("alpha2","n","lan") #rename cols

#~~~~~~~~~~ PLOT
ggplot(data=m1,aes(y=lan,x=n))+
  geom_col(aes(fill=n),alpha=0.85,color="cornsilk")+scale_size(range = c(5,20))+
  #geom_point(aes(size=n,color=n))+scale_size(range = c(5,20))+
  xlim(0,70)+ theme_classic() + scale_fill_viridis(option="inferno",begin=0.6)+ 
  scale_y_discrete(limits=rev)+
  ggtitle("\n HORROR IN A FOREIGN LANGUAGE")+
  labs(subtitle = "    Frecuencies of non-English languages in horror films.\n\n",
       caption = "\n Nov 2022 | Data: The Movie Database - #TidyTuesday 2022 week 44")+
  theme(plot.background =  element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text.x = element_text( vjust = 1, hjust=1,color="#B22222",family="Creepster",size=10),
        axis.text.y = element_text(color="cornsilk",size=15,family="Creepster"),
        legend.position ="none",plot.title = element_text(color="#B22222",family="Creepster",face="bold",size=50),
        plot.subtitle = element_text(color="cornsilk",family="Creepster",size=15),
        plot.caption = element_text(color="cornsilk"))
