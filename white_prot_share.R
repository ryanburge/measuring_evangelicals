
gss <- gss %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(protestant = recode(relig, "1=1; else=0")) %>% 
  mutate(whtprot = white + protestant) %>% 
  mutate(whtprot = recode(whtprot,"2=1; else=0"))

gs <- gss %>% as_survey_design(weights = wtssall)

gswprot <- gs %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(whtprot, vartype = "ci")) %>% 
  mutate(sample = c("White Protestants"))

gswhite <- gs %>% 
  group_by(year) %>% 
  summarise(pct = survey_mean(white, vartype = "ci")) %>% 
  mutate(sample = c("White"))

t2 <- bind_rows(gswprot, gswhite)

t2 %>% 
  ggplot(., aes(x=year, y= pct*100, group = sample, color = sample, label = sample)) + geom_line(size = 1.5) + 
  geom_errorbar(aes(ymin = pct_low*100, ymax=pct_upp*100), width = .75, size = 1.25, position=position_dodge(.9), color = "azure4") + 
  theme(legend.position="bottom") +
  ggtitle("White Protestants as a Share of the Population") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=32, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Year") + ylab("Percent of Population") +
  labs(caption = "Data from GSS") + theme(legend.title=element_blank())

ggsave(file="white_prot_line.png", type = "cairo-png", width = 20, height = 15)
