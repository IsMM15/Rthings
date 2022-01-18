#Jan 2022// TidyTuesday week 3 // Chocolate rating 
library(tidyverse);library(viridis);library(countrycode);library()

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
data$cocoa_percent<-strsplit(data$cocoa_percent,"%")
data$cocoa_percent<- as.numeric(data$cocoa_percent)

count<-as.vector(data$)
Region = countrycode(sourcevar = count, origin = "country.name",destination = "region")
data2<-cbind(data,Region)



data3<- data2%>% select("country_of_bean_origin","Region","cocoa_percent","rating")%>% drop_na()
data3$rating<- as.factor(data3$rating)

ggplot(data3) + geom_violin(aes(x=rating,y=cocoa_percent,fill= rating),color="antiquewhite",alpha=0.5,show.legend=FALSE) + 
  geom_point(aes(x=rating,y=cocoa_percent,color=Region),alpha=0.5,size=1.5,)+
  ggtitle("Chocolate Ratings",subtitle = "Where do the best chocolate bars come from and how much cacao do they have? \n \n")+
  labs(caption = "\n \n #TidyTuesday week 3 || Data source: Flavors of Cacao || Git: @isMM15")+
  annotate("text",x=1.7,y=42,label="*Point density accounts for the ammount of chocolate bars \n that share cacao percentage | Region refers to bean origin ",
           size=2.5,color="antiquewhite",fontface="italic")+
  xlab("\n Rating")+ylab("Cocoa \n [%]") +
  scale_color_viridis(option="inferno",discrete = TRUE,end = 0.75)+
  scale_fill_viridis(option="magma",discrete = TRUE,begin =.75,direction = -1)+
  theme_minimal()+
  theme( plot.background = element_rect(fill="bisque4"),
         panel.grid = element_line(linetype = "dashed",color="black",size = 0.2),
         panel.grid.major.x = element_blank(),
         axis.text.y = element_text(color="gray21",face="bold"),
         axis.text.x= element_text(color="gray21",face="bold"),
         axis.title= element_text(color="black",face="bold"),
         legend.position = "bottom",
         legend.title = element_text(face="bold",color="antiquewhite"),
         legend.text = element_text(color="antiquewhite"),
         plot.title = element_text(color="bisque",face="bold",size=45,),
         plot.caption = element_text(color="gray21",size=8,),
         plot.subtitle = element_text(color="antiquewhite2",face="bold"),
         plot.margin = margin(1,1,1,1,"cm"),
         
         )


