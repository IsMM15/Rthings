#Feb 2022// TidyTuesday week 5 // Dog breeds
library(tidytext);library(glue);library(ggtext);library(tidyverse);library(viridis)
#LoadTheData
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
##Select columns
BC<-breed_traits[32,] #border collie col.
BC<-BC[,-8]
BC<-BC[,-8]
##Order data
BC<- BC %>% pivot_longer(cols=!Breed, names_to = "trait",values_to = "rate") #Make traits columns
BC <- rbind(rep(5,14) , rep(0,14) , BC  )
levels<- BC$trait #Set levels - to order the bars
##PLOT
ggplot(BC)+geom_col(aes(x=trait,y=rate,fill=rate))+ ggtitle("Border Collies",
                                                    subtitle = glue("Some of their traits rated from 
                                                                    <span style = 'color:#404040'>**low**</span>  to <span style = 'color:#b8627db2'>**high**</span> "))+
  labs(caption="\n #TidyTuesday week 5 || Data source: American Kennel Club|| Git: @isMM15")+
  scale_fill_viridis(option = "magma",end = 0.5)+
  theme_minimal()+ scale_x_discrete(limits = levels)+
  scale_y_continuous(limits = c(-5, 6),expand = c(0, 0)) + coord_polar()+
  theme_minimal()+theme(
    plot.background = element_rect(fill="antiquewhite"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face="bold",color="hotpink4",size=35),
    plot.subtitle = element_markdown(color="darkslategray",size=15),
    plot.caption = element_text(color="gray21",size=6),
    axis.text.x = element_text(size=5,face="bold"),
    plot.margin = margin(1,1,1,1,"cm"),legend.title = element_blank())
  