install.packages("lfe")
library(lfe)

rm(list=ls())
setwd("/Users/nakaoryouta/Desktop/lesson/2020秋_授業/Statistical analysis/課題/last_report/data")
use_data <- read.csv ("omid_T_data.csv")


#######################   win_dummy (優勝ダミー)　ロジスティック回帰　###################### 

result_1 <-lm(win_dummy ~ s_span_of_control ,data = use_data)#単回帰

result_2 <-lm(win_dummy ~ s_span_of_control + span_of_contorl_2 +height + weight + pro_player_proportion + spring_s_span 
  + K_dummy + W_dummy + M_dummy + R_dummy + H_dummy,data = use_data)#重回帰

result_3 <-lfe::felm(win_dummy ~ s_span_of_control  +height + weight+ span_of_contorl_2  + pro_player_proportion + spring_s_span | fe_id, data = use_data)#個人+年度+大学FE

result_4 <- glm(win_dummy ~ s_span_of_control , data=use_data,family=poisson)#ロジスティック回帰

result_5 <- glm(win_dummy ~ s_span_of_control + span_of_contorl_2 +height + weight + pro_player_proportion + spring_s_span 
  + K_dummy + W_dummy + M_dummy + R_dummy + H_dummy, data=use_data,family=poisson)#ロジスティック回帰


result_plain = stargazer::stargazer(result_1,result_2,result_3,result_4,result_5,type = "html" )
result_escape = stringr::str_replace_all(result_plain, "\\((.+?)\\)", "'\\(\\1\\)")
cat(result_escape,file="win_dummy.html",sep="\n")


#散布図

plot(use_data$s_span_of_control , use_data$win_dummy)
abline(lm(use_data$s_span_of_control , use_data$win_dummy))

#ロジスティック回帰　図表

ggplot(use_data,                                                  　　
  aes(x=span_of_control, y=as.numeric(win_dummy))) +
  geom_point(position=position_jitter(width=0.4,height=0.08), 　　
    alpha=.4,shape=21,size=1.5)+
  
  stat_smooth(method="glm",
    method.args = list(family="binomial"(link="logit")))+  
  xlab("number of menber")+ylab("win") + coord_cartesian(ylim=c(0,1))+　　　　　
  
  theme_classic()　　　　　　　


#回帰診断

par(mfrow=c(2,2))
plot(result_2)　
