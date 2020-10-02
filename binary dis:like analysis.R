setwd("~/Documents/projects/second-year project")
library(readxl)
library(xlsx)
library(ggplot2)
library(dplyr)

load("/Users/macintoshhd/Documents/projects/second-year project/anes.RData")
anes1 <- anes %>%
  select(dem_like_bi, party_id) %>%
  na.omit()



# Is there anything R likes about Democratic Party?
glm1 <- glm(dem_like_bi ~ party_id, data=anes1, family=binomial("logit"))
summary(glm1)


nd<-cbind(1, seq(1, 7, by=1))
nd %*% coef(glm1) %>% plogis()
sim_beta<-MASS::mvrnorm(1000, coef(glm1), vcov(glm1))
head(sim_beta)


lower_bound=nd %*% t(sim_beta) %>% t() %>% plogis() %>% apply(2, quantile, 0.025)
upper_bound=nd %*% t(sim_beta) %>% t() %>% plogis() %>% apply(2, quantile, 0.975)
median=nd %*% t(sim_beta) %>% t() %>% plogis() %>% apply(2, quantile, 0.5)
plot.dat<-data.frame(lower_bound, upper_bound, median, pid=seq(1,7, 1))

p <- ggplot(plot.dat, aes(x=as.factor(pid), 
                     y=median, 
                     ymax=upper_bound, 
                     ymin=lower_bound, group=1))+
  geom_point(position="jitter")+
  geom_line()+
  stat_smooth()+
  geom_ribbon(fill="blue", alpha=0.15)+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  xlab("Party ID")+
  ylab("Like/Dislike Democratic Party")

p + coord_cartesian(xlim = c(1, 7), ylim = c(0, 1))



  
  
  







