#TidyTuesday Week 14 - March 2021
library(tidyverse); library(ggridges);library(extrafont);library(extrafontdb)

#Load original data
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')

#Filter popular brands
dataFilteres<- allNumbers%>% filter(brand=="Dior"|
                                      brand=="Givenchy"|
                                      brand=="Gicci"|
                                      brand=="FENTY BEAUTY by Rihanna"|
                                      brand=="Makeup Revolution"|
                                      brand=="MAKE UP FOR EVER"|
                                      brand=="Clinique"|
                                      brand=="Lancôme"|
                                      brand=="L'Oréal"|
                                      brand=="SEPHORA COLLECTION"|
                                      brand=="bareMinerals"|
                                      brand=="It Cosmetics")
#Plot
ggplot(data=dataFilteres,aes(y=brand,x=lightness))+
  geom_density_ridges(scale = 0.75,fill="darkslategray",colour="darkslategray4")+xlim(0:1)+
  annotate("point",x=dataFilteres$lightness,y=dataFilteres$brand,color=dataFilteres$hex,size=2)+
  theme_minimal()+ggtitle("THE BEAUTY BIAS")+labs(subtitle="Shown here, foundation shades of several popular makeup brands.\n Lightness is represented in the scale of 0 (black) to 1 (white).\n\n",
                                                  caption="TidyTuesday Week 14 | Data Source:The Pudding | March 2021")+
  theme(plot.background = element_rect(fill="#F1EAE3"),panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),axis.text.y = element_text(face="bold",family="serif",color="#2D3033")
        ,axis.title.y = element_blank(),axis.title.x = element_text(face="bold",color="#2D3033",family="Helvetica"),
        axis.text.x = element_text(face="bold"),plot.margin = margin(1,1,1,1,"cm")
        ,plot.title = element_text(face="bold",size=40,color="darkslategray",hjust = 0.4,family="serif"),
        plot.subtitle = element_text(family="Helvetica",size=10,hjust=0.4,color="#2D3033",face="bold"),
        plot.caption = element_text(hjust=1,color="#2D3033",family="Helvetica",size=5))
#Save plot
ggsave(filename="Week14.jpeg",dpi=300,width=10)



