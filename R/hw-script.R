library(tidyverse)
library(ggplot2)
library(ggrepel)

ggplot(parties, aes(x = eneg, y = enep)) + 
  geom_point()+
  facet_wrap(vars(electoral_system))

politics<-read.csv("data/politics_and_need.csv")

#geom point
ggplot(politics, aes(x = percent_uninsured, y = percent_favorable_aca, color = percent_poverty)) + 
  geom_point()+
facet_wrap(vars(gov_position))

#geom text w/ label wrapped by govt position 
ggplot(politics, aes(x = percent_uninsured, y = percent_favorable_aca, label = state_abbr)) + 
  geom_text(alpha = .5)+
  facet_wrap(vars(gov_position))
#geom label with colors, labels, wrapped by gov position
ggplot(politics, aes(x = percent_uninsured, y = percent_favorable_aca, label = state_abbr, color = state)) + 
  theme(legend.position = "none")+
  geom_label(alpha = .1)+
  facet_wrap(vars(gov_position))

#geom point with text repel
ggplot(politics, aes(x = percent_uninsured, y = percent_favorable_aca, label = state_abbr, color = state)) + 
  theme(legend.position = "none")+
  geom_point(alpha = .1)+
  geom_text_repel()+
  facet_wrap(vars(gov_position))

#geom point with label repel
  ggplot(politics, aes(x = percent_uninsured, y = percent_favorable_aca, label = state_abbr, color = state)) + 
    theme(legend.position = "none")+
    geom_point(alpha = .1)+
    geom_label_repel()+
  facet_wrap(vars(gov_position))
  
# Chapter 9 correlation and plot exercise 9.4
gamson<-read_rds("data/gamson.rds") #loading gamson
glimpse(gamson)

cor_df<-gamson%>% #creating correlation data
  summarise(cor = cor(seat_share, portfolio_share))
glimpse(cor_df)

ggplot(gamson, aes(x = seat_share, y = portfolio_share)) + 
  geom_point(alpha = .5)+
geom_label(data = cor_df, aes(x = Inf, y = Inf, label = paste0("cor = ", round(cor, 2))),
           hjust = 1.1, vjust = 1.1)+
  theme_bw() #plotting correlation data

#exercise 9.5
devtools::install_github("pos5737/pos5737data")
data(anscombe, package = "pos5737data")
glimpse(anscombe)
view(anscombe)

smry_ans <- anscombe %>%
group_by(dataset) %>%
summarize(cor = cor(x = x, 
                      y = y))%>%
glimpse()
view(smry_ans) 

ggplot(anscombe, aes(x = x, y = y)) + 
  geom_point() + 
  facet_wrap(vars(dataset))
