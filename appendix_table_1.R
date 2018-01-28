
# gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")
library(survey)
library(srvyr)

gss$bagain <- Recode(gss$reborn, "1=1;else=0")
gss$protestant <- Recode(gss$relig, "1=1;else=0")


gss$baprot <- gss$bagain + gss$protestant
gss <- gss %>% mutate(baprot = car::recode(baprot, "2=1; else=0"))

gs <- gss %>% filter(year >=2004) %>%  filter(race !=2) %>% as_survey_design(weights = wtssall)

g1 <- gs %>% 
  filter(baprot ==1) %>% 
  filter(partyid <8) %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(partyid, vartype = "ci", na.rm = TRUE)) %>% 
  mutate(sample = c("Self ID"))

gs <- gss %>% filter(year >=2004) %>%  as_survey_design(weights = wtssall)


g2 <- gs %>% 
  filter(year >=2004) %>% 
  filter(evangelical ==1) %>% 
  filter(partyid <8) %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(partyid, vartype = "ci", na.rm = TRUE)) %>% 
  mutate(sample = c("Affiliation"))



pid <- bind_rows(g1, g2)

ggplot(pid, aes(x=year, y=pct, color = sample, label = sample)) + geom_line(aes(group=sample), size = 1.5) + 
  geom_errorbar(aes(ymin = pct_low, ymax=pct_upp), width = .25, size = 1.25, position=position_dodge(.9), color = "azure4") +
  scale_y_continuous(limits = c(0,6), breaks = c(0,1,2,3,4,5,6), labels = c("Strong Democrat", "Not Strong Democrat", "Ind., Near Dem.", "Independent", "Lean Republican", "Moderate Republican", "Strong Republican")) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  labs(x= "Year", y = "Mean Party Identification", title = "Comparing the Two Measures on Party Affiliation", caption = "Data from the GSS (2004-2016)")+
  scale_color_manual(values = c("black", "gray", "black", "#7CAE00", "#00BE67","#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) 

ggsave(file="appendix_graph1.png", type = "cairo-png", width = 15, height = 10)