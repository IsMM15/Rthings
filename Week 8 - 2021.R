#Feb 2021// TidyTuesday Week8 // W.E.B. Du Bois Challenge
library(tidyverse)
#------
#Data
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')
freed_slaves  <- freed_slaves%>% mutate(posY=Slave) #positions for % labels
freed_slaves[9,4]=90
#-------
#Plot
ggplot(freed_slaves)+geom_area(aes(x=Year,y=Slave),fill="#000000")+
  scale_x_continuous(position = "top",breaks = freed_slaves$Year,expand = c(0,0),limits = c(1790,1870))+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  ggtitle("PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\n\n\nPROPORTION DES NÈGRES LIBRES ET  DES ESCLAVES EN  AMÉRIQUE.\n",
          subtitle="DONE BY ATLANTA UNIVERSITY \n\n\n")+ 
  theme_minimal()+ theme(panel.background = element_rect(fill="#027000",color="antiquewhite3"),
                         plot.background = element_rect(fill="antiquewhite3"),plot.margin = margin(2,1,1,1,"cm"),
                         panel.grid = element_blank(),panel.grid.major.x = element_line(colour="#000000",size=0.2),
                         axis.title = element_blank(),axis.text.y = element_blank(),axis.text.x=element_text(family="sans",colour = "#000000",face="bold",size=13),
                         plot.title = element_text(family="mono",face="bold",size=14,hjust= 0.5),
                         plot.subtitle =element_text(family="mono",face="bold",size=10,hjust= 0.5), plot.caption = element_text(colour="black",size=3,vjust=0.5,family="sans") )+
  annotate(geom="text",x=1830,y=50,label="SLAVES \nESCLAVES",colour="white",family="mono",fontface="bold",size=9)+
  annotate(geom="text",x=1830,y=95,label="FREE~LIBRE",colour="#000000",family="mono",fontface="bold",size=7)+
  annotate(geom="text",x=(freed_slaves$Year),y=(freed_slaves$posY +2),label=(paste(freed_slaves$Free,"%")),color="#000000",family="sans",fontface="bold")+
  coord_cartesian(clip = "off")+ #to write outside  plot area
  labs(caption="Data soucer & original plot:Du Bois Challenge |Recreation: @Izz_m2")

ggsave("DuBois.png", width = 7.34, height = 9.34)

