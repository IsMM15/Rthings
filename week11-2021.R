##Week 11 - Bechdel test
library(tidyverse);library(glue)
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

movies<- movies%>% mutate(decade= case_when(year<1975~ "1970-1974",
                                            year>=1975&year<1980~ "1975-1979",
                                            year>=1980&year<1985~"1980-1984",
                                            year>=1985&year<1990~"1985-1989",
                                            year>=1990&year<1995~ "1990-1994",
                                            year>=1995&year<2000~ "1995-1999",
                                            year>=2000&year<2005~ "2000-2004",
                                            year>=2005&year<2010~ "2005-2009",
                                            year>=2010&year<2015~ "+2010"))
data2<-movies%>% count(decade,binary)
data2$decade<-ordered(data2$decade,
                      levels=c("1970-1974","1975-1979","1980-1984"
                               ,"1985-1989","1990-1994","1995-1999",
                               "2000-2004","2005-2009","+2010"))


ggplot(data2)+
  geom_point(aes(x=decade,y=n,colour=binary),size=5,alpha=0.85)+theme_minimal()+
  scale_discrete_manual(aesthetics="colour",values=c("#FFA45E","#DCD3FF"))+
  ylab("Number of movies \n ")+ ggtitle("The Bechdel Test")+ylim(0,300)+
  labs(subtitle = glue("**The Beachdel Test is used to measure gender bias in movies using three criteria:**<br>
**1.** Have more than two named female characters<br>
**2.** These characters must talk among them<br>
**3.**The conversation must  not revolve around a man<br>
<br><br>Here, movies that <span style = 'color:#DCD3FF'>**pass**</span>  or <span style = 'color:#FFA45E'>**fail**</span> the test, throughout the years.<br><br>"))+
geom_segment(aes(x=decade, xend=decade, y=0, yend=n,colour=binary,lty=binary),alpha=0.5)+
  theme(plot.background = element_rect(fill="#262254"),panel.grid = element_blank(),
        axis.text = element_text(colour="white",face="bold"),
        axis.title = element_text(colour="#e6dff0",face="bold"),
        plot.title = element_text(face="bold",size=30,colour="white"),
        plot.subtitle  = element_markdown(colour="white"), 
        legend.position = "none",axis.title.x  = element_blank(),
        plot.margin = margin(1,1,1,1,"cm"))
library("tidyverse");library("glue")





