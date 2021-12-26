#The National - Sept 2021 // Modified Dec 2021
library(tidytext);library(glue);library(ggtext);library(tidyverse);library(viridis)
## PLOT 1: Most frecuent words across all songs.
load("/Users/isabelmontejano/Desktop/TEC/R/The_National.Rdata") #Importar el dataset
token<-The_National %>% unnest_tokens(word,lyric) #separar por palabras
token<- token %>% count(word)#Sacar las más frecuentes
token<-token %>% anti_join(stop_words) #quitar stoppers
#Filter (to do this we must check the data first)   
tokencount<-token      
tokencount<-tokencount[tokencount$n>=28,]
tokencount<-tokencount[-9,]
tokencount<-tokencount[-19,]
tokencount<-tokencount[-10,]
tokencount<-tokencount[1:25,]
vect<-c(1:25)#To make the geom area work (we need this vector to be joined to the final dataset to plot in order to mark the x position)
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
#------ 
### PLOT 2: Songs with the highest use od the words love and leave (1 and 2 most frecuent in plot 1)
token2<-The_National %>% unnest_tokens(word,lyric)
token2<-token2[,3:5]
token2<-token2[,-2]
token2<- token2 %>% count(track_title,word)

love<- token2[which(token2$word=="love"),]
love<-love[order(love$n),]
lovesel<-love[28:34,]

leave<- token2[which(token2$word=="leave"),]
leave<-leave[order(leave$n,decreasing = TRUE),]
leavesel<-leave[1:6,]

words<-rbind(lovesel,leavesel)
words<-words[-12,]

col=c("cadetblue","mediumorchid4")

set.seed(1)

ggplot(words) + geom_point(aes(x=track_title,y=sqrt(n)+runif(n=1,min=0,max=0.5),color= word,size=sqrt(n)),alpha=0.45) + ylim(0,7.5)+
  annotate(geom="text",x=words$track_title,y=sqrt(words$n)-0.38,label=words$track_title,color="white",size=3,fontface="bold")+
  scale_size_continuous(range = c(10, 25)) + theme_minimal()+ scale_color_manual(values=col)+ 
  ggtitle("About Love and Loss", subtitle = glue("<span style = 'color:#6781C6'>**The National**</span> songs with highest use of the words 
                                                 <span style = 'color:mediumorchid3'>**love**</span> and <span style = 'color:cadetblue'>**leave**</span>"))+
  labs(caption="<span style = 'color:#6781C6'>**Note:**</span> The circle size and its height are proportional to the frequency of each word.  The range goes from 30 repetitions to 4") +
  theme_void()+theme(plot.background = element_rect(fill="black"),
                     plot.title = element_text(color="white",face="bold",size=45,hjust=0.2),
                     plot.subtitle = element_markdown(color="white",face="bold",size=15,hjust=0.2),
                     plot.caption = element_markdown(color="white",size=10,hjust=0.2,vjust=0.7,face="bold"),
                     legend.position = "none",plot.margin = margin(1,1,1,1,"cm"))