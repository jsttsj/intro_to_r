library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)


load("suicides.rdata")

suicides$age <- as.factor(suicides$age)

all_suicides <- copy(suicides)
suicides <- suicides %>% 
  group_by(year, age, sex) %>% 
  mutate(deaths = sum(deaths))

#  Make a line plot of suicides by 
suicide.line<-ggplot(suicides,aes(x=year,y=deaths,color=age))+
                        facet_wrap(~sex)+
                       geom_line()

# (year on the x axis, deaths on the y axis, different line for each age).
# facet by sex.


##extra credit####

one_state <- all_suicides[all_suicides$state=="Uttar Pradesh"] %>% 
  group_by(year, state, sex, age, means) %>% 
  mutate(deaths = sum(deaths))

# Make a set of density plots faceted by sex and means of suicide,
# showing distributions of suicides by age, for the state of Uttar Pradesh.
# Label appropriately.
suicide.density<-ggplot(one_state,aes(x=deaths))+
  facet_grid(means~sex,scales = "free")+
  geom_density(aes(color=age,fill=age),alpha=.5)



