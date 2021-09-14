#The National - Sept 2021
library(tidytext):library(tidytext);library(glue);library(ggtext)
load("/Users/isabelmontejano/Desktop/TEC/R/TN.Rdata") #Importar el dataset
token<-TheNational %>% unnest_tokens(word,lyric) #separar por palabras
token<- token %>% count(word)#Sacar las más frecuentes
token<-token %>% anti_join(stop_words) #quitar stoppers

#filtrar   
tokencount<-token %>% count(word)      
tokencount<-tokencount[tokencount$n>=28,]
tokencount<-tokencount[-9,]
tokencount<-tokencount[-19,]
tokencount<-tokencount[-10,]
tokencount<-tokencount[1:25,]

vect<-c(1:25)#Esto es para que funcione el geom_area- agrega una columna de números 
cbind(tokencount,vect)
newtokencount<-cbind(tokencount,vect)

ggplot(newtokencount)+ geom_area(aes(x=vect,y=(sqrt(n))),fill="white",alpha=0.05)+
  geom_point(aes(x=vect,y=sqrt(n),size=n,color=sqrt(n),alpha=0.90))+ylim(0,sqrt(140))+
  annotate(geom="text",x=tokencount$word,y=sqrt(tokencount$n),label=tokencount$word,color="white",size=3,fontface="bold")+
  scale_x_discrete(breaks=vect)+ #hace discreto el eje - para que funcione el geom_area
  scale_color_viridis(begin=0.3,end = 0.65,option="A")+
  scale_size_continuous(range = c(8, 20))+
  scale_alpha_continuous(range=c(0.2,0.75))+
  ggtitle("\n WHAT'S IN A SONG",subtitle = glue("Top 25 words used in <span style = 'color:#6781C6'>**The National**</span>  songs <br>"))+
  labs(caption="The square root of each word's frequency is represented by the height and the size of the circle.") +
  annotate(geom="rect",xmin=tokencount$word,xmax=tokencount$word,ymax=(sqrt(tokencount$n)-0.4),
           ymin=0,color="white",alpha=0.1,lty="dotted")+
  theme_void()+theme(plot.background = element_rect(fill="black"),
                     plot.title = element_text(color="white",face="bold",size=45,hjust=0.2),
                     plot.subtitle = element_markdown(color="white",face="bold",size=15,hjust=0.2),
                     plot.caption = element_markdown(color="white",size=10,hjust=0.2,vjust=0.7,face="bold"),
                     legend.position = "none",plot.margin = margin(1,1,1,1,"cm"))
 
#-------
token<-token[token$n>=20,]

pairwise<- token%>% pairwise_cor(track_title,word,sort=TRUE)

set.seed(12)
pairwise%>% filter(correlation>.13)%>% graph_from_data_frame()%>% ggraph()+
  geom_edge_link(show.legend = FALSE,aes(edge_alpha=correlation,linemitre=5)) +
  geom_node_point(color="cadetblue",size=5,alpha=0.5)+geom_node_text(aes(label=name,size=5),repel=TRUE)+ theme_void()
