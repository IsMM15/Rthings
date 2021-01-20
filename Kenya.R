library(tidyverse);library(viridis);library(patchwork);library(extrafont)

#--------
#load & filter data
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
Tea.Coffee<- crops %>% select(SubCounty,Coffee,Tea,Farming)%>%  na.omit()
Tea.Coffee<- Tea.Coffee[-1,]
Tea.Coffee$SubCounty<- factor(Tea.Coffee$SubCounty,levels= Tea.Coffee$SubCounty[order(Tea.Coffee$Farming)])


B<-ggplot(Tea.Coffee)+geom_point(aes(y=SubCounty,x=Coffee),color="coral4",alpha=0.75,size=5)+geom_point(aes(y=SubCounty,x=Tea),color ="darkgreen",alpha=0.75,size=5)+
  scale_x_continuous(labels =comma_format(big.mark = ",",decimal.mark = "."))+theme_minimal()+xlab("\nPopulation growing the crop")+
  theme(plot.background = element_rect(fill="beige"),panel.grid =element_line(color="bisque"), 
        axis.text.y = element_text(colour="darkorange4",face="bold",size="8"),
        axis.title.y= element_blank(),
        axis.text.x = element_text(colour="darkgray",face="bold",size="8"),
        axis.title.x = element_text(colour="cornsilk4",face="bold",size="10"),plot.margin = margin(2,2,2,2,"cm"))

A<-ggplot(Tea.Coffee)+geom_col(aes(x=Farming,y=SubCounty,fill=SubCounty))+scale_fill_viridis(begin =.6,discrete = TRUE)+
  scale_x_continuous(labels =comma_format(big.mark = ",",decimal.mark = "."))+theme_minimal()+xlab("\nPopulation growing farming crops")+
  theme(plot.background = element_rect(fill="beige"),panel.grid =element_line(color="bisque"), 
        axis.text.y = element_text(colour="darkorange4",face="bold",size="8"),
        axis.title.y= element_blank(),
        axis.text.x = element_text(colour="darkgray",face="bold",size="8"),
        axis.title.x = element_text(colour="cornsilk4",face="bold",size="10"),legend.position = "none",
        plot.margin = margin(2,2,2,2,"cm"),plot.title = element_text(size=12,colour="darkslategray"))

A+B+plot_annotation(title="\nKenya in crops: Coffee or tea?",subtitle = "The first graph shows the total number of population growing crops in counties where both coffee and tea are grown. \nThe second graph shows a comparison between the population who grow coffee (brown) and those who grow tea (green).",
                    caption="Data: rKenyaCensus by Shel Kariuki| Vizualization: @izz_m2| Jan 2021") &
  theme(plot.background = element_rect(fill="beige",colour = "beige"),plot.title = element_text(face=4,size=30,colour="darkgoldenrod"),
        plot.subtitle = element_text(family ="Helvetica" ,color="darkslategray",size=12),plot.caption = element_text(colour="darkgray"))

            