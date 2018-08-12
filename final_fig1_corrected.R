
## Before beginning, run the run_this_first.R script
## This loads all the data. Runs RELTRAD on the CCES files. 


cces08 <- cces08 %>% 
  rename(protestant = V219, bagain = V215, weight = V201, pid7 = CC307a) %>% 
  mutate(year = 2008) 

cces10 <- cces10 %>% 
  rename(protestant = V219, bagain = V215, weight = V101, pid7 = V212d) %>% 
  mutate(year = 2010)

cces12 <- cces12 %>% 
  rename(protestant = religpew, bagain = pew_bornagain, weight = weight_vv) %>% 
  mutate(year = 2012)

cces14 <- cces14 %>% 
  rename(protestant = religpew, bagain = pew_bornagain, weight = weight) %>% 
  mutate(year = 2014)

cces16 <- cces16 %>% 
  rename(protestant = religpew, bagain = pew_bornagain, weight = commonweight_vv) %>% 
  mutate(year = 2016)

## Now creating smaller versions of our dataset with only the variables are essential to analysis

c08 <- cces08 %>% 
  select(year, protestant, evangelical, bagain, race, weight)

c10 <- cces10 %>% 
  select(year, protestant, evangelical, bagain, race, weight)

c12 <- cces12 %>% 
  select(year, protestant, evangelical, bagain, race, weight)

c14 <- cces14 %>% 
  select(year, protestant, evangelical, bagain, race, weight)

c16 <- cces16 %>% 
  select(year, protestant, evangelical, bagain, race, weight)

## Bind them all together into one big dataframe called all_cc

all_cc <- bind_rows(c08, c10, c12, c14, c16) %>% as.tibble()

all_cc <- all_cc %>% 
  mutate(reborn = recode(bagain, "1=1; else=0")) %>%
  mutate(protestant = recode(protestant, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>%  
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))

count_cc1 <- all_cc %>% 
  group_by(year) %>%  
  summarise(mean = weighted.mean(evangelical, w = weight, na.rm = TRUE),
            sd = sd(evangelical, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Affiliation")

## Party ID for Self IDs

count_cc2 <- all_cc %>% 
  group_by(year) %>%  
  summarise(mean = weighted.mean(baprot2, w = weight, na.rm = TRUE),
            sd = sd(baprot2, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Self ID")

cc_count <- bind_rows(count_cc1, count_cc2)

cc_count <- cc_count %>% 
  mutate(survey = paste("CCES", year, sep = " "))



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

count_gg1 <- gss %>% 
  group_by(year) %>%  
  summarise(mean = weighted.mean(evangelical, w = wtss, na.rm = TRUE),
            sd = sd(evangelical, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Affiliation")

count_gg2 <- gss %>% 
  group_by(year) %>%  
  summarise(mean = weighted.mean(baprot2, w = wtss, na.rm = TRUE),
            sd = sd(baprot2, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Self ID")

gg_count <- bind_rows(count_gg1, count_gg2)

gg_count <- gg_count %>% 
  mutate(survey = paste("GSS", year, sep = " "))


total <- bind_rows(cc_count, gg_count) %>% select(-year)

ggplot(total, aes(x=survey, y=mean, fill = type)) + geom_col(position = "dodge")+ 
  geom_errorbar(aes(ymin = lower, ymax=upper), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  labs(caption = "95% Confidence Intervals Displayed") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=32, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black")) + 
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year") +  
  scale_y_continuous(labels = scales::percent)

ggsave(file="fig1_not_black_final.png", type = "cairo-png", width = 20, height = 15)
