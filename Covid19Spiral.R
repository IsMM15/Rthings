#Jan 08 2022 || COVID19 spiral plot 
#Nota: El de referencia esta en Twitter (Enero-6-22)
#Load data - enero 8
data<- read.csv("data2.csv")

#------------------
#MEXICO
#Filter data
Mexdata<- data[data$iso_code=="MEX",] #seleccionar solo mex 
Mexdata<-Mexdata %>% mutate(daynum=(1:738)) #numerar dias - el limite sup cambia si cambian los datos - 1: ultimodia
#Plot
ggplot(Mexdata) + 
  geom_tile(aes(x=daynum%%365, y=400*daynum + new_cases_smoothed,
                height = new_cases_smoothed, fill = new_cases_smoothed)) + 
  coord_polar()+ scale_fill_viridis(option = "inferno") +
  scale_y_continuous(limits = c(-20, NA))+
  scale_x_continuous(breaks = 30*0:11, minor_breaks = NULL,labels = month.abb)+
  labs(fill="New Covid-19 cases in Mexico \n [smoothed]",caption = "Data source: Our World in Data - Jan 08 2022") + theme_minimal()+
  theme(panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(linetype = "dotted",size=0.5),
        axis.title = element_blank(),axis.text.x = element_text(face="bold",size=12,color="black"),
        axis.text.y = element_blank(),legend.title = element_text(hjust=0.5,face="bold"),
        plot.caption = element_text(size=8,color="darkgray",hjust=0)) + 
  annotate("text",x=05,y=20000 ,label="2020", size=3,fontface="bold",color="darkgray")+  
  annotate("text",x=01,y=180000 ,label="2021", size=3,fontface="bold",color="darkgray")+
  annotate("text",x=01,y=320000 ,label="2022", size=3,fontface="bold",color="darkgray")
#nota: el coeff de daynum en y se ajusta para que la espiral no este apretada. Cambia segun los datos

#-------
#WOLDWIDE
wdata<- data[data$iso_code=="OWID_WRL",]
wdata<-wdata %>% mutate(daynum=(1:717))

#Nota 2: Necesitamos que los datos empiecen el 1 de enero o se desfasa
vacios<- Mexdata[1:21,] #El data set de Mx empieza el 1 de enero del 2020 - Extraemos los primeros 21 dias (los que faltan)
nwdata<-rbind(vacios,wdata) #juntar el dataset con los 21 dias que faltan 
nwdata<-nwdata %>% mutate(daynum=(1:738)) #numerar los dias
nwdata<-nwdata %>% mutate(iso_code="OWID_WRL") #Poner el iso code en los que faltaban
#Plot
ggplot(nwdata) + 
  geom_tile(aes(x=daynum%%365, y=20000*daynum + new_cases_smoothed,
                height = new_cases_smoothed, fill = new_cases_smoothed)) + 
  coord_polar()+ scale_fill_viridis(option = "inferno") + 
  scale_y_continuous(limits = c(-20, NA))+
  scale_x_continuous(breaks = 30*0:11, minor_breaks = NULL,labels = month.abb)+
  labs(fill="New Covid-19 cases worldwide \n [smoothed]",caption = "Data source: Our World in Data - Jan 08 2022") + theme_minimal()+
  theme(panel.grid.major.y = element_blank(),panel.grid.major.x = element_line(linetype = "dotted",size=0.5),
        axis.text.y = element_blank(), axis.title = element_blank(),axis.text.x = element_text(face="bold",size=12,color="black"),
        legend.title = element_text(hjust=0.5,face="bold"),plot.caption = element_text(size=8,color="darkgray",hjust=0)) + 
  annotate("text",x=05,y=500000 ,label="2020", size=3,fontface="bold",color="darkgray")+  
  annotate("text",x=01,y=10000000 ,label="2021", size=3,fontface="bold",color="darkgray")+
  annotate("text",x=01,y=18000000 ,label="2022", size=3,fontface="bold",color="darkgray")




