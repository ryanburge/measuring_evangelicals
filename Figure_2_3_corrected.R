## This is to create consistent variable names across all waves of the CCES

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

### This is code the attendance variable so that higher values = more attendance

cces08 <- cces08 %>% 
  mutate(att = recode(V217, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else =NA"))

cces10 <- cces10 %>% 
  mutate(att = recode(V217, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else =NA"))

cces12 <- cces12 %>% 
  mutate(att = recode(pew_churatd, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else =NA"))

cces14 <- cces14 %>% 
  mutate(att = recode(pew_churatd, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else =NA"))

cces16 <- cces16 %>% 
  mutate(att = recode(pew_churatd, "6=0; 5=1; 4=2; 3=3; 2=4; 1=5; else =NA"))

## Now creating smaller versions of our dataset with only the variables are essential to analysis

c08 <- cces08 %>% 
  select(year, protestant, evangelical, bagain, race, weight, pid7, att)

c10 <- cces10 %>% 
  select(year, protestant, evangelical, bagain, race, weight, pid7, att)

c12 <- cces12 %>% 
  select(year, protestant, evangelical, bagain, race, weight, pid7, att)

c14 <- cces14 %>% 
  select(year, protestant, evangelical, bagain, race, weight, pid7, att)

c16 <- cces16 %>% 
  select(year, protestant, evangelical, bagain, race, weight, pid7, att)

## Bind them all together into one big dataframe called all_cc

all_cc <- bind_rows(c08, c10, c12, c14, c16) %>% as.tibble()

## Party ID for Evangelicals 

c1 <- all_cc %>% 
  filter(evangelical ==1) %>% 
  group_by(year) %>%  
  filter(pid7 <= 7) %>% ## exclude those who said other party id 
  summarise(mean = weighted.mean(pid7, w = weight, na.rm = TRUE),
            sd = sd(pid7, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Affiliation")

## Party ID for Self IDs

c2 <- all_cc %>% 
  filter(protestant ==1 & bagain ==1 & race != 2) %>% 
  group_by(year) %>%  
  filter(pid7 <= 7) %>% 
  summarise(mean = weighted.mean(pid7, w = weight, na.rm = TRUE),
            sd = sd(pid7, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Self ID")

## Bind together and add a survey variable 
cc <- bind_rows(c1, c2)
cc$survey <- paste("CCES", cc$year, sep = " ")

## Restrict our Analysis to 2008 to 2016
gss <- gss %>% filter(year > 2007)

g1 <- gss %>% 
  filter(evangelical ==1) %>% 
  group_by(year) %>%  
  mutate(pid = partyid + 1) %>% ## must add to pid for GSS because it's coded 0 to 6, while CCES is 1 to 7
  filter(pid <= 7) %>% ## exclude those who said other party id 
  summarise(mean = weighted.mean(pid, w = wtss, na.rm = TRUE),
            sd = sd(pid, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Affiliation")

g2 <- gss %>% 
  filter(baprot2 ==1) %>% 
  group_by(year) %>%  
  mutate(pid = partyid + 1) %>% 
  filter(pid <= 7) %>% 
  summarise(mean = weighted.mean(pid, w = wtss, na.rm = TRUE),
            sd = sd(pid, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se)  %>% 
  mutate(type = "Self ID")

gg <- bind_rows(g1, g2)

gg$survey <- paste("GSS", gg$year, sep = " ")

## Add both results into one dataset
total <- bind_rows(cc, gg)

## Graph
ggplot(total, aes(x = mean, y = survey))  +
  geom_point(shape=21, size =4, aes(fill = factor(type))) +  theme(legend.title=element_blank()) +
  geom_errorbarh(aes(xmin = lower, xmax=upper), height=.075, size = 1, show.legend = FALSE) +
  theme(legend.position = "bottom")  + scale_fill_brewer(palette = "Set2") + 
  labs(y ="Year And Survey", x ="Self Identified Party ID", caption = "95% Confidence Intervals Displayed") +
  ggtitle("Mean Partisanship of Each Measurement")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24, family="KerkisSans"))  +
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +
  scale_x_continuous(limits = c(.5,7.5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep"))

ggsave(file="figure_2.png", type = "cairo-png", width = 15, height = 10)


## Making Attendance Graph ####

cc1 <- all_cc %>% 
  filter(evangelical ==1) %>% 
  group_by(year) %>%  
  filter(att >= 0) %>% 
  summarise(mean = weighted.mean(att, w = weight, na.rm = TRUE),
            sd = sd(att, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Affiliation")

cc2 <- all_cc %>% 
  filter(protestant ==1 & bagain ==1 & race != 2) %>% 
  group_by(year) %>%  
  filter(att >= 0) %>% 
  summarise(mean = weighted.mean(att, w = weight, na.rm = TRUE),
            sd = sd(att, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Self ID")

ccc <- bind_rows(cc1, cc2)

ccc$survey <- paste("CCES", ccc$year, sep = " ")


gss$att <- Recode(gss$attend, "8=5; 6:7=4; 4:5=3; 3=2; 1:2=1; 0=0; else =NA")

gg1 <- gss %>% 
  filter(year >=2008) %>% 
  filter(evangelical ==1) %>% 
  group_by(year) %>% 
  summarise(mean = weighted.mean(att, w = wtss, na.rm = TRUE),
            sd = sd(att, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = "Affiliation")

gg2 <- gss %>% 
  filter(year >=2008) %>%
  filter(reborn ==1 & relig ==1 & race != 2) %>% 
  group_by(year) %>% 
  summarise(mean = weighted.mean(att, w = wtss, na.rm = TRUE),
            sd = sd(att, na.rm = TRUE), 
            n = n()) %>% 
  mutate(se = sd/sqrt(n),
         lower = mean - qt(1 - (0.05 /2),  n -1) * se,
         upper = mean + qt(1 - (0.05 /2),  n -1) * se) %>% 
  mutate(type = c("Self ID"))

ggg <- bind_rows(gg1, gg2)

ggg$survey <- paste("GSS", ggg$year, sep = " ")

total2 <- bind_rows(ccc, ggg) %>% select(-year)

ggplot(ggg, aes(x = mean, y = survey))  +
  geom_point(shape=21, size =4, aes(fill = factor(type))) +  theme(legend.title=element_blank()) +
  geom_errorbarh(aes(xmin = lower, xmax=upper), height=.075, size = 1, show.legend = FALSE) +
  theme(legend.position = "bottom")  +  
  labs(y= "Year And Survey", x= "Self Reported Church Attendance", caption = "95% Confidence Intervals Displayed") +
  ggtitle("Mean Church Attendance of Each Measurement")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24, family="KerkisSans"))  +
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +
  scale_x_continuous(limits = c(0,5), breaks = c(0,1,2,3,4,5), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))

ggsave(file="figure_3.png", type = "cairo-png", width = 15, height = 10)


