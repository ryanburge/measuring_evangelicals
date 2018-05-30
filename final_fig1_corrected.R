
## Before beginning, run the run_this_first.R script
## This loads all the data. Runs RELTRAD on the CCES files. 

library(survey)
library(srvyr)

cces16 <- cces16 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>%  
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))

c16 <- cces16 %>% as_survey_design(weights = commonweight_vv)

c16e <- c16 %>%
  summarise(pct = survey_mean(evangelical, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2016"))

c16ba <- c16 %>%
  summarise(pct = survey_mean(baprot2, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2016"))

cces14 <- cces14 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>% 
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))

c14 <- cces14 %>% as_survey_design(weights = weight)

c14e <- c14 %>%
  summarise(pct = survey_mean(evangelical, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2014"))

c14ba <- c14 %>%
  summarise(pct = survey_mean(baprot, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2014"))


cces12 <- cces12 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>% 
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))
 

c12 <- cces12 %>% as_survey_design(weights = weight_vv)

c12e <- c12 %>%
  summarise(pct = survey_mean(evangelical, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2012"))

c12ba <- c12 %>%
  summarise(pct = survey_mean(baprot2, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2012"))


cces10 <- cces10 %>% 
  mutate(protestant = recode(V219, "1=1; else=0")) %>% 
  mutate(bagain = recode(V215, "1=1; else=0"))

cces10 <- cces10 %>% 
  mutate(baprot = protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(black = recode(V211, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>% 
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))
  

c10 <- cces10 %>% as_survey_design(weights = V101)

c10e <- c10 %>%
  summarise(pct = survey_mean(evangelical, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2010"))

c10 <- cces10 %>% as_survey_design(weights = V101)


c10ba <- c10 %>%
  summarise(pct = survey_mean(baprot2, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2010"))


cces08 <- cces08 %>% 
  mutate(protestant = recode(V219, "1=1; else=0")) %>% 
  mutate(bagain = recode(V215, "1=1; else=0"))


cces08 <- cces08 %>% 
  mutate(baprot = protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(black = recode(V211, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>% 
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))

c08 <- cces08 %>% as_survey_design(weights = V201)

c08e <- c08 %>%
  summarise(pct = survey_mean(evangelical, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2008"))

c08 <- cces08 %>% as_survey_design(weights = V201)

c08ba <- c08 %>%
  summarise(pct = survey_mean(baprot2, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2008"))

cc <- bind_rows(c08ba, c08e, c10ba, c10e, c12ba, c12e, c14ba, c14e, c16ba, c16e)


## Use the RELTRAD scheme to create the evangelical variable

gss <- gss %>% 
  filter(year > 2007) %>% # Only 2008 waves and beyond
  mutate(protestant = recode(relig, "1=1; else=0")) %>% # Protestant dummy
  mutate(reborn = recode(reborn, "1=1; else=0")) %>% # Born-again dummy
  mutate(baprot =  protestant + reborn) %>% # Add the two dummies together
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% # Now, recode so only those who score two become baprot
  mutate(black = recode(race, "2=1; else =0")) %>% # Need to create a black variable
  mutate(baprot2 = baprot - black) %>% # Subtract the black variable from the baprot variable
  mutate(baprot2 = recode(baprot2, "1=1; else =0")) # So, if someone was both black and baprot they should be at a 0 now, some will be negative 1. I need to keep 1 as 1. 

gs <- gss %>% as_survey_design(weights = wtssall) ## Putting the dataset into the srvyr package with the proper weight

gsevan <- gs %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(evangelical, vartype = "ci", level = .84)) %>% #
  mutate(sample = c("Affiliation"))

gsba <- gs %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(baprot2, vartype = "ci", level = .84)) %>% 
  mutate(sample = c("Self ID"))

gs2 <- bind_rows(gsevan, gsba)

gs2$survey <- paste("GSS", gs2$year, sep = " ")
rm(gs)

total <- bind_rows(cc, gs2) %>% select(-year)

ggplot(total, aes(x=survey, y=pct*100, fill = sample)) + geom_col(position = "dodge")+ 
  geom_errorbar(aes(ymin = pct_low*100, ymax=pct_upp*100), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  labs(caption = "84% Confidence Intervals Displayed") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=32, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year") 

ggsave(file="fig1_not_black_final.png", type = "cairo-png", width = 20, height = 15)
