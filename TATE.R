library(tidyverse);library(viridis)
#---------------
#Data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")
N<- count(artwork$year) %>% na.omit() #group by year created, create freq column.
#-------------------
#plot
ggplot(N)+ geom_area(aes(x=x,y=freq),fill="white",alpha=0.2)+geom_jitter(aes(x=x,y=freq,size=freq,color=x),alpha=0.90)+
  scale_size(range=c(0.5,9),breaks=c(0,25,50,100,250,500,1000,1500,2000,2500,3000,3500))+xlab("\nYear Created")+
  scale_color_viridis(option="magma")+theme_classic()+ ggtitle("TATE",subtitle="The size of the circles is \nproportional to the amount \nof pieces in the collection \ncreated that year.\nBeing the early 1800s \nthe most popular period.")+
  labs(caption="\n \nData Source:Tate Art Museum.Vizualitazion by @izz_m2 |Jan 2021")+
  theme(axis.line.y=element_blank(),axis.text.y=element_blank(),axis.title.y =element_blank(),axis.ticks.y = element_blank(),
        axis.text.x=element_text(face="bold",color="white"),panel.background= element_rect(fill="darkslategray"),
        plot.background = element_rect(fill="darkslategrey"),plot.margin=margin(2, 5, 2, 5, "cm"),legend.position ="none",
        axis.title.x = element_text(face="bold",size=16,color="white",hjust=1),axis.line.x = element_line(color="white"),
        title=element_text(face="bold",color="white",size=50),plot.subtitle=element_text(size=12,face="italic"),plot.caption = element_text(size=6,face="plain",color="darkgray"))+ 
  annotate("text",x=1819,y=3150,label="1819",size=1.75,fontface="bold")+annotate("text",x=1830,y=2514,label="1830",size=1.75,fontface="bold")+
  annotate("text",x=1816,y=1675,label="1816",size=1.7,fontface="bold")+annotate("text",x=1831,y=1565,label="1831",size=1.5,fontface="bold")+
  annotate("text",x=1801,y=1528,label="1801",size=1.5,fontface="bold")
