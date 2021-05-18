#May2021|| TidyTuesday week 21|| Ask a Manager Survey
library(tidyverse);library(ggridges):library(viridis)

#------------LOAD AND FILTER DATA
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')
data<- survey%>% filter(country %in% c("US","USA"))
data<- data%>% filter(gender %in% c("Woman","Man","Non-binary","Other or prefer not to answer"))
data3<- data%>%filter(highest_level_of_education_completed %in% c("High School","College degree","Master's degree","PhD","Professional degree (MD, JD, etc.)"))
#-------------RELEVEL FACTORS
#Plot 1
data3$highest_level_of_education_completed <-ordered(data3$highest_level_of_education_completed,
                      levels=c("High School","College degree","Master's degree","PhD","Professional degree (MD, JD, etc.)"))
#Plot 2
data3$gender <-ordered(data3$gender,levels=c("Non-binary","Woman","Other or prefer not to answer","Man"))
#---------------------- PLOTTING
#plot 1 - Education. Does it pay?
ggplot(data3)+
  geom_violin(aes(x=annual_salary,y=highest_level_of_education_completed, fill=highest_level_of_education_completed),
               color="darkslategray",outlier.shape = NA,alpha=0.85)+ggtitle("Education. Does it pay?")+
  scale_fill_viridis(discrete=TRUE,option="viridis",begin  = 0.65)+theme_minimal()+
  geom_boxplot(aes(x=annual_salary,y=highest_level_of_education_completed) ,
               fill="white",width=0.1,color="gray18",outlier.shape = NA)+
  labs(subtitle = "Those with a professional degree, such as an MD or JD, have the highest median salary when compared to other types of degrees.\n\n",
       caption = "May 2021| Data: Ask A Manager Survey")+
  scale_x_continuous(labels =comma,limits = c(0,300000))+xlab("\n Annual Salary \n [Dolars]")+
  theme(axis.title.x = element_text(face="bold",color="darkslategray"),axis.title.y = element_blank(),
        axis.text = element_text(face="bold"),
        plot.title = element_text(face="bold",size=35,color="darkslategray"),
        plot.background = element_rect(fill="beige"),
        panel.grid = element_line(color="gray18",linetype="dotted",size=0.3),
        legend.position = "none",plot.subtitle = element_text(color="gray18",size=10),plot.margin = margin(1,1,1,1,"cm"))

ggsave("2.png",width=12,height=5)

#Plot 2 - Miind The Gap
ggplot(data3)+
  geom_violin(aes(x=annual_salary,y=gender, fill=gender),
              color="darkslategray",outlier.shape = NA,alpha=0.85)+ggtitle("Mind The Gap")+
  scale_fill_viridis(discrete=TRUE,option="magma",begin  = 0.65)+theme_minimal()+
  geom_boxplot(aes(x=annual_salary,y=gender) ,
               fill="white",width=0.1,color="gray18",outlier.shape = NA)+
  labs(subtitle = "Shown here is the annual salary reported by gender during an Ask The Mannager Survey.\n\n",
       caption = "May 2021| Data: Ask A Manager Survey")+
  scale_x_continuous(labels =comma,limits = c(0,300000))+xlab("\n Annual Salary \n [Dolars]")+
  theme(axis.title.x = element_text(face="bold",color="darkslategray"),axis.title.y = element_blank(),
        axis.text = element_text(face="bold"),
        plot.title = element_text(face="bold",size=35,color="coral2"),
        plot.background = element_rect(fill="beige"),
        panel.grid = element_line(color="gray18",linetype="dotted",size=0.5),
        legend.position = "none",plot.subtitle = element_text(color="gray18",size=10),plot.margin = margin(1,1,1,1,"cm"))
ggsave("3.png",width=12,height=5)

