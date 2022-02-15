#Feb 2022// TidyTuesday week 7 // DuBois Challenge
library(tidyverse);

Year<-c(1876,1886,1896)
Percent<-c(-37.59,-56.66,-87.29)
Enrolled<-c((-37.59*037.59)/100,(-56.66*056.66)/100,(-87.29*057.29)/100)
data<-as.data.frame(cbind(Year,Percent,Enrolled))

ggplot(data)+geom_col(aes(x=Year,y=Percent),fill="black")+ 
  geom_col(aes(x=Year,y=Enrolled),fill="#b11111")+
  xlim(c(1870,1902)) + 
  annotate(geom="text",y=4,x=1876,label="1876",fontface="bold",size=6,family="mono")+
  annotate(geom="text",y=4,x=1886,label="1876",fontface="bold",size=6,family="mono")+
  annotate(geom="text",y=4,x=1896,label="1896",fontface="bold",size=6,family="mono")+
  #labels inside the bars
  annotate(geom="text",y=-10,x=1876,label="37.59%",fontface="bold",size=7,family="mono")+
  annotate(geom="text",y=-25,x=1886,label="56.66%",fontface="bold",size=7,family="mono")+
  annotate(geom="text",y=-35,x=1896,label="57.29%",fontface="bold",size=7,family="mono")+
  #University of Atlanta label
  annotate(geom="text",x=1886,y=15,label="DONE BY ATLANTA UNIVERSITY.",fontface="bold",family="mono",size=1.5)+
  #Title in french
  annotate(geom="text",x=1886,y=20,size=2,fontface="bold",family="mono",label="PROPORTION DES ENFANTS NÈGRES EN ÂGE D'ÉCOLE ENREGISTRÉS DANS LES ÉCOLES PUBLIQUES.")+
  #Title in english
  annotate(geom="text",x=1886,y=25,size=2,fontface="bold",family="mono",label="PROPORTION OF TOTAL NEGRO CHILDREN OF SCHOOL AGE WHO ARE ENROLLED IN THE PUBLIC SCHOOLS.")+
  #legend squeres
  geom_segment(x=1874,xend=1876,y=-60-5,yend=-60-5,color="#b11111",size=8)+
  geom_segment(x=1874,xend=1876,y=-70-5,yend=-70-5,color="black",size=8)+
  #legend text - red
  annotate(geom="text",x=1872,y=-58.5-5,label="PROPORTION",size=1.5,family="mono")+
  annotate(geom="text",x=1872,y=-61.5-5,label="PROPORTION",size=1.5,family="mono")+
  annotate(geom="text",x=1879,y=-58.5-5,label="OF CHILDREN ENROLLED",size=1.5,family="mono")+
  annotate(geom="text",x=1879,y=-61.5-5,label="D'ENFANTS ENREGISTRÉS",size=1.5,family="mono")+
#legend text - black
  annotate(geom="text",x=1872,y=-68.5-5,label="PROPORTION",size=1.5,family="mono")+
  annotate(geom="text",x=1872,y=-71.5-5,label="PROPORTION",size=1.5,family="mono")+
  annotate(geom="text",x=1879.5,y=-68.5-5,label="OF CHILDREN NOT ENROLLED",size=1.5,family="mono")+
  annotate(geom="text",x=1879.5,y=-71.5-5,label="D'ENFANTS NON ENREGISTRÉS",size=1.5,family="mono")+
  labs(caption="#TidyTuesday Week 7||#DuBoisChallenge2022||Data source:Anthony Starcks ||Git: @IsMM15    \n\n ")+
  #theme
  theme_void()+
  theme(
    plot.background = element_rect(fill="antiquewhite2"),
    plot.caption = element_text(family="mono",size=4.5,vjust=1,)
  )
ggsave("db.jpg",width=5,height=7)

  