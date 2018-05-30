

c08 <- cces08 %>% as_survey_design(weights = V201)
c10 <- cces10 %>% as_survey_design(weights = V101)
c12 <- cces12 %>% as_survey_design(weights = weight_vv)
c14 <- cces14 %>% as_survey_design(weights = weight)
c16 <- cces16 %>% as_survey_design(weights = commonweight_vv)

p1 <- c08 %>% 
  filter(baprot ==1) %>%  filter(V211 !=2) %>% 
  summarise(mean = survey_mean(CC307a, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2008")

p2 <- c08 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(CC307a, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2008")


p3 <- c10 %>% 
  filter(baprot ==1) %>% filter(V211 !=2) %>% 
  summarise(mean = survey_mean(V212d, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2010")

p4 <- c10 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(V212d, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2010")


p5 <- c12 %>% 
  filter(baprot ==1) %>% 
  filter(race !=2) %>%
  summarise(mean = survey_mean(pid7, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2012")

p6 <- c12 %>% 
  filter(evangelical ==1) %>%
  summarise(mean = survey_mean(pid7, vartype = "ci", level = .84, na.rm =TRUE))%>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2012")


p7 <- c14 %>% 
  filter(baprot ==1) %>% 
  filter(race !=2) %>%
  summarise(mean = survey_mean(pid7, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2014")

p8 <- c14 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(pid7, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2014")


p9 <- c16 %>% 
  filter(baprot ==1) %>% 
  filter(race !=2) %>%
  summarise(mean = survey_mean(pid7, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2016")

p10 <- c16 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(pid7, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2016")

pp <- bind_rows(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

gss <- gss %>% filter(year > 2007)

gss <- gss %>% 
    mutate(pid = partyid + 1) # The GSS is coded 0 to 6. The CCES is 1 to 7. This is how I correct that. 

gs <- gss %>% as_survey_design(weights = wtssall)

g1 <- gs %>% 
  filter(evangelical ==1) %>% 
  group_by(year) %>% 
  summarise(mean = survey_mean(pid, vartype = "ci", level = .84, na.rm = TRUE)) %>% 
  mutate(type = c("Affiliation"))

g2 <- gs %>% 
  filter(reborn ==1 & relig ==1 & race != 2) %>% 
  group_by(year) %>% 
  summarise(mean = survey_mean(pid, vartype = "ci", level = .84, na.rm = TRUE)) %>% 
  mutate(type = c("Self ID"))

gg <- bind_rows(g1, g2)

gg$survey <- paste("GSS", gg$year, sep = " ")

total <- bind_rows(pp, gg) %>% select(-year)

# write.csv(total, "pid_table.csv")


ggplot(total, aes(x = mean, y = survey))  +
  geom_point(shape=21, size =4, aes(fill = factor(type))) +  theme(legend.title=element_blank()) +
  geom_errorbarh(aes(xmin = mean_low, xmax=mean_upp), height=.075, size = 1, show.legend = FALSE) +
  theme(legend.position = "bottom")  + scale_fill_brewer(palette = "Set2") + 
  labs(x ="Year And Survey", y ="Self Identified Party ID", caption = "84% Confidence Intervals Displayed") +
  ggtitle("Mean Partisanship of Each Measurement")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24, family="KerkisSans"))  +
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +
  scale_x_continuous(limits = c(.5,7.5), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep"))

ggsave(file="figure_2.png", type = "cairo-png", width = 15, height = 10)


## Making Attendance Graph ####


cces08$attend <- 7 - as.numeric(cces08$V217)
# cces08$attend <- Recode(cces08$attend, "0= 'Do not Know';
#                         1= 'Never';
#                         2= 'Seldom';
#                         3= 'Yearly';
#                         4= 'Monthly';
#                         5= 'Weekly';
#                         6= 'More than Weekly'")


cces10$attend <- 7 - as.numeric(cces10$V217)
# cces10$attend <- Recode(cces10$attend, "0= 'Do not Know';
#                         1= 'Never';
#                         2= 'Seldom';
#                         3= 'Yearly';
#                         4= 'Monthly';
#                         5= 'Weekly';
#                         6= 'More than Weekly'")

cces12$attend <- 7 - as.numeric(cces12$pew_churatd)
# cces12$attend <- Recode(cces12$attend, "0= 'Do not Know';
#                         1= 'Never';
#                         2= 'Seldom';
#                         3= 'Yearly';
#                         4= 'Monthly';
#                         5= 'Weekly';
#                         6= 'More than Weekly'")

cces14$attend <- 7 - as.numeric(cces14$pew_churatd)
# cces14$attend <- Recode(cces14$attend, "0= 'Do not Know';
#                         1= 'Never';
#                         2= 'Seldom';
#                         3= 'Yearly';
#                         4= 'Monthly';
#                         5= 'Weekly';
#                         6= 'More than Weekly'")

cces16$attend <- 7 - as.numeric(cces16$pew_churatd)
# cces16$attend <- Recode(cces16$attend, "0= 'Do not Know';
#                         1= 'Never';
#                         2= 'Seldom';
#                         3= 'Yearly';
#                         4= 'Monthly';
#                         5= 'Weekly';
#                         6= 'More than Weekly'")


c08 <- cces08 %>% as_survey_design(weights = V201)
c10 <- cces10 %>% as_survey_design(weights = V101)
c12 <- cces12 %>% as_survey_design(weights = weight_vv)
c14 <- cces14 %>% as_survey_design(weights = weight)
c16 <- cces16 %>% as_survey_design(weights = commonweight_vv)



p1 <- c08 %>% 
  filter(baprot2 ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2008")

p2 <- c08 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2008")


p3 <- c10 %>% 
  filter(baprot2 ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2010")

p4 <- c10 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2010")


p5 <- c12 %>% 
  filter(baprot2 ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2012")

p6 <- c12 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE))%>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2012")


p7 <- c14 %>% 
  filter(baprot2 ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2014")

p8 <- c14 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2014")


p9 <- c16 %>% 
  filter(baprot2 ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Self ID") %>% 
  mutate(survey = "CCES 2016")

p10 <- c16 %>% 
  filter(evangelical ==1) %>% 
  summarise(mean = survey_mean(attend, vartype = "ci", level = .84, na.rm =TRUE)) %>% 
  mutate(type = "Affiliation") %>% 
  mutate(survey = "CCES 2016")

pp <- bind_rows(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

gss$att <- Recode(gss$attend, "8=6; 6:7=5; 4:5=4; 3=3; 2=2; 1=1; 0=0")

gss <- gss %>% 
  filter(year > 2007)  # Only 2008 waves and beyond
  

gs <- gss %>% as_survey_design(weights = wtssall)

g1 <- gs %>% 
  filter(year >=2008) %>% 
  filter(evangelical ==1) %>% 
  group_by(year) %>% 
  summarise(mean = survey_mean(att, vartype = "ci", level = .84, na.rm = TRUE)) %>% 
  mutate(type = c("Affiliation"))

g2 <- gs %>% 
  filter(year >=2008) %>%
  filter(baprot2 ==1) %>% 
  group_by(year) %>% 
  summarise(mean = survey_mean(att, vartype = "ci",  level = .84, na.rm = TRUE)) %>% 
  mutate(type = c("Self ID"))

gg <- bind_rows(g1, g2)

gg$survey <- paste("GSS", gg$year, sep = " ")

total <- bind_rows(pp, gg) %>% select(-year)

ggplot(total, aes(x = mean, y = survey))  +
  geom_point(shape=21, size =4, aes(fill = factor(type))) +  theme(legend.title=element_blank()) +
  geom_errorbarh(aes(xmin = mean_low, xmax=mean_upp), height=.075, size = 1, show.legend = FALSE) +
  theme(legend.position = "bottom")  + scale_fill_brewer(palette = "Set2") + 
  labs(x= "Year And Survey", y= "Self Reported Church Attendance", caption = "84% Confidence Intervals Displayed") +
  ggtitle("Mean Church Attendance of Each Measurement")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=24, family="KerkisSans"))  +
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +
  scale_x_continuous(limits = c(.5,6.5), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))

ggsave(file="figure_3.png", type = "cairo-png", width = 15, height = 10)


