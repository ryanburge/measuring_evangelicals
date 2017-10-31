library(survey)
library(srvyr)

cces16 <- cces16 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = white + protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "3=1; else=0"))

cces16 <- cces16 %>% 
  mutate(whtevan = white + evangelical) %>% 
  mutate(whtevan= recode(whtevan, "2=1; else=0"))
  
c16 <- cces16 %>% filter(white ==1) %>%  as_survey_design(weights = commonweight_vv)

c16e <- c16 %>%
  summarise(pct = survey_mean(whtevan, vartype = "ci")) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2016"))

c16ba <- c16 %>%
  summarise(pct = survey_mean(baprot, vartype = "ci")) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2016"))

cces14 <- cces14 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = white + protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "3=1; else=0"))

cces14 <- cces14 %>% 
  mutate(whtevan = white + evangelical) %>% 
  mutate(whtevan= recode(whtevan, "2=1; else=0"))

c14 <- cces14 %>% filter(white ==1) %>% as_survey_design(weights = weight)

c14e <- c14 %>%
  summarise(pct = survey_mean(whtevan, vartype = "ci")) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2014"))

c14ba <- c14 %>%
  summarise(pct = survey_mean(baprot, vartype = "ci")) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2014"))

cces12 <- cces12 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = white + protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "3=1; else=0"))

cces12 <- cces12 %>% 
  mutate(whtevan = white + evangelical) %>% 
  mutate(whtevan= recode(whtevan, "2=1; else=0"))

c12 <- cces12 %>% filter(white ==1) %>% as_survey_design(weights = weight_vv)

c12e <- c12 %>%
  summarise(pct = survey_mean(whtevan, vartype = "ci")) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2012"))

c12ba <- c12 %>%
  summarise(pct = survey_mean(baprot, vartype = "ci")) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2012"))

cces10 <- cces10 %>% 
  mutate(white = recode(V211, "1=1; else=0")) %>% 
  mutate(protestant = recode(V219, "1=1; else=0")) %>% 
  mutate(bagain = recode(V215, "1=1; else=0"))

cces10 <- cces10 %>% 
  mutate(baprot = white + protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "3=1; else=0")) 

cces10 <- cces10 %>% 
  mutate(whtevan = white + evangelical) %>% 
  mutate(whtevan= recode(whtevan, "2=1; else=0"))

c10 <- cces10 %>% filter(white ==1) %>% as_survey_design(weights = V101)

c10e <- c10 %>%
  summarise(pct = survey_mean(whtevan, vartype = "ci")) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2010"))

c10ba <- c10 %>%
  summarise(pct = survey_mean(baprot, vartype = "ci")) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2010"))


cces08 <- cces08 %>% 
  mutate(white = recode(V211, "1=1; else=0")) %>% 
  mutate(protestant = recode(V219, "1=1; else=0")) %>% 
  mutate(bagain = recode(V215, "1=1; else=0"))

cces08 <- cces08 %>% 
  mutate(baprot = white + protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "3=1; else=0"))

cces08 <- cces08 %>% 
  mutate(whtevan = white + evangelical) %>% 
  mutate(whtevan= recode(whtevan, "2=1; else=0"))

c08 <- cces08 %>% filter(white ==1) %>% as_survey_design(weights = V201)

c08e <- c08 %>%
  summarise(pct = survey_mean(whtevan, vartype = "ci")) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2008"))

c08ba <- c08 %>%
  summarise(pct = survey_mean(baprot, vartype = "ci")) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2008"))

cc <- bind_rows(c08ba, c08e, c10ba, c10e, c12ba, c12e, c14ba, c14e, c16ba, c16e)


gss <- gss %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(whtevan = white + evangelical) %>% 
  mutate(whtevan = recode(whtevan, "2=1; else=0"))

gss <- gss %>% 
  mutate(protestant = recode(relig, "1=1; else=0")) %>% 
  mutate(reborn = recode(reborn, "1=1; else=0")) %>% 
  mutate(baprot = white + protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "3=1; else=0"))

gs <- gss %>% filter(white ==1) %>% as_survey_design(weights = wtssall)

gsevan <- gs %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(whtevan, vartype = "ci")) %>% 
  mutate(sample = c("Affiliation"))


gsba <- gs %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(baprot, vartype = "ci")) %>% 
  mutate(sample = c("Self ID"))

gs2 <- bind_rows(gsevan, gsba)

gs2$survey <- paste("GSS", gs2$year, sep = " ")

total <- bind_rows(cc, gs2)


ggplot(total, aes(x=survey, y=pct*100, fill = sample)) + geom_col(position = "dodge")+ 
  geom_errorbar(aes(ymin = pct_low*100, ymax=pct_upp*100), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of White Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Measuring Evangelicals (White Only)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=32, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year") 

ggsave(file="reltrad_white_only_freq.png", type = "cairo-png", width = 20, height = 15)

