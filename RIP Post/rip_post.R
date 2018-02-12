library(tidyverse)
library(haven)
library(car)
library(scales)
library(extrafont)


cces08 <- read_dta("D:/cces/data/cces08.dta")
cces10 <- read_dta("D:/cces/data/cces10.dta")
cces12 <- read_dta("D:/cces/data/cces12.dta")
cces14 <- read_dta("D:/cces/data/cces14.dta")
cces16 <- read_dta("D:/cces/data/cces16.dta")

gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")


cces08$white <- Recode(cces08$V211, "1=1; else=0")
cces08$bagain <- Recode(cces08$V215, "1=1; else=0")
cces08$protestant <- Recode(cces08$V219, "1=1;else=0")
cces08 <- cces08 %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

cces10$white <- Recode(cces10$V211, "1=1; else=0")
cces10$bagain <- Recode(cces10$V215, "1=1; else=0")
cces10$protestant <- Recode(cces10$V219, "1=1;else=0")
cces10 <- cces10 %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))


cces12$white <- Recode(cces12$race, "1=1; else=0")
cces12$bagain <- Recode(cces12$pew_bornagain, "1=1; else=0")
cces12$protestant <- Recode(cces12$religpew, "1=1; else=0")
cces12 <- cces12 %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

cces14$white <- Recode(cces14$race, "1=1; else=0")
cces14$bagain <- Recode(cces14$pew_bornagain, "1=1; else=0")
cces14$protestant <- Recode(cces14$religpew, "1=1; else=0")
cces14 <- cces14 %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

cces16$white <- Recode(cces16$race, "1=1; else=0")
cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")
cces16$protestant <- Recode(cces16$religpew, "1=1; else=0")
cces16 <- cces16 %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

c1 <-cces08 %>% count(whtbaprot, wt = V201) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2008"))
c2 <-cces10 %>% count(whtbaprot, wt = V101) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2010"))
c3 <-cces12 %>% count(whtbaprot, wt = weight_vv_post) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2012"))
c4 <-cces14 %>% count(whtbaprot, wt = weight) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2014"))
c5 <-cces16 %>% count(whtbaprot, wt = commonweight_vv_post) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2016"))

cgraph <- bind_rows(c1, c2, c3, c4, c5) %>% mutate(source = c("CCES")) %>% mutate(year = as.numeric(year))

gss$bagain <- Recode(gss$reborn, "1=1; else =0")
gss$white <- Recode(gss$race, "1=1; else =0")
gss$protestant <- Recode(gss$relig, "1=1; else =0")
gss <- gss %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

g1 <- gss %>% 
  group_by(year) %>% 
  count(whtbaprot, wt = wtssall) %>%
  mutate(weight = prop.table(n)) %>% 
  filter(whtbaprot ==1) %>% 
  select(year, weight) %>% 
  mutate(source = c("GSS"))

graph <- bind_rows(cgraph, g1)

ggplot(graph, aes(x=year, y=weight, group = source, color = source)) + geom_line(size = 1.25)  +
  theme(axis.ticks = element_blank()) + ylab("Percent of Sample") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Year")  + 
  scale_y_continuous(labels = scales::percent) + theme(legend.title=element_blank()) 

ggsave(file="rip_1.png", type = "cairo-png", width = 15, height = 12)

graph <- graph %>% 
  add_row(weight = .230, year = "2006", source = "PRRI") %>% 
  add_row(weight = .225, year = "2007", source = "PRRI") %>% 
  add_row(weight = .214, year = "2008", source = "PRRI") %>% 
  add_row(weight = .208, year = "2009", source = "PRRI") %>% 
  add_row(weight = .208, year = "2010", source = "PRRI") %>% 
  add_row(weight = .213, year = "2011", source = "PRRI") %>% 
  add_row(weight = .199, year = "2012", source = "PRRI") %>% 
  add_row(weight = .175, year = "2013", source = "PRRI") %>% 
  add_row(weight = .183, year = "2014", source = "PRRI") %>% 
  add_row(weight = .173, year = "2015", source = "PRRI") %>% 
  add_row(weight = .168, year = "2016", source = "PRRI") 

graph %>%  filter( year >= 2006) %>% 
ggplot(., aes(x=year, y=weight*100, group = source, color = source)) + geom_line(size = 2)  +
  theme(axis.ticks = element_blank()) + ylab("Percent of Sample") + 
  theme(legend.position="bottom") +
  ggtitle("Changes in White Evangelical Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Year")  + 
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) + theme(legend.title=element_blank()) + ylim(0,30) +
  geom_dl(aes(label = source), method = list(dl.trans(x = x + .5, y = y + .75), cex =2, "first.points"))
  
  

ggsave(file="rip_all.png", type = "cairo-png", width = 15, height = 12)




gs <-gss %>%
  group_by(year) %>%
  count(whtbaprot, wt = wtssall) %>%
  mutate(weight = prop.table(n)) %>%
  filter(whtbaprot ==1) %>%
  select(year, weight) %>%
  mutate(source = c("GSS")) %>%
  mutate(survey = paste(source, year, sep = " ")) %>%
  select(survey, weight)

gr <- gss %>% 
  group_by(year) %>% 
  count(wt = wtssall) %>% 
  right_join(gs) %>% 
  mutate(count = weight*n) %>% 
  rename(pct = weight)

gr <- gr %>% filter(year >= 2006)

gr$moe <- c(2.97, 4.47, 4.43, 4.38,  4.03, 3.48)
gr <- gr %>% mutate(pct = pct*100, sample = c("Self ID"))


gss <- gss %>% mutate(whtevan = white + evangelical) %>% mutate(whtevan = recode(whtevan, "2=1; else=0"))


gs1 <-gss %>%
  group_by(year) %>%
  count(whtevan, wt = wtssall) %>%
  mutate(weight = prop.table(n)) %>%
  filter(whtevan ==1) %>%
  select(year, weight) %>%
  mutate(source = c("GSS")) %>%
  mutate(survey = paste(source, year, sep = " ")) %>%
  select(survey, weight)

gr1 <- gss %>% 
  group_by(year) %>% 
  count(wt = wtssall) %>% 
  right_join(gs1) %>% 
  mutate(count = weight*n) %>% 
  rename(pct = weight)

gr1 <- gr1 %>% filter(year >= 2006)


gr1$moe <- c(moe = 2.92, 4.24, 4.28, 4.45, 4.06, 3.77 )
gr1 <- gr1 %>% mutate(pct = pct*100, sample = c("Affiliation"))

gs <- bind_rows(gr, gr1) %>% ungroup(year) %>% select(survey, sample, pct, moe)

total <- bind_rows(gs, cces1)

limits <- aes(ymax = total$pct + total$moe, ymin = total$pct - total$moe)


total$max <- total$pct + total$moe
total$min <- total$pct - total$moe


ggplot(t2, aes(x=year, y=pct)) + geom_line()+ 
  geom_errorbar(aes(ymin = min, ymax=max), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + 
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year")   

ggsave(file="rip_bar.png", type = "cairo-png", width = 15, height = 12)






cces1$moe <- c(1.06, 1.19, .88, .9, .86, .91, .87, .97)

limits <- aes(ymax = total$pct + total$moe, ymin = total$pct - total$moe)
total$max <- total$pct + total$moe
total$min <- total$pct - total$moe


gss %>%
  group_by(year) %>%
  summarise(mean = mean(whtbaprot, na.rm = TRUE),
            sd = sd(whtbaprot, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

gss %>%
  group_by(year) %>%
  count(whtevan, wt = wtssall) %>%
  filter(whtevan ==1) %>% 
  rename(tot = n) %>% 
  summarise(sd = sd(whtbaprot, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = tot - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = tot + qt(1 - (0.05 / 2), n - 1) * se)



###gss white reltrad
###2006 line graph
###bar graph

evan <- gss %>% group_by(year) %>% count(evangelical, wt = wtssall)  %>% mutate(weight = prop.table(n)) %>% filter(evangelical ==1) %>% mutate(group = c("Evangelicals"))

ggplot(evan, aes(x=year, y=weight*100, group = group)) + geom_line(color = "black", size = 2) +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  ggtitle("Evangelicals in the General Social Survey") +
  labs(fill="")  + xlab("Year")  + 
  scale_y_continuous(labels = scales::percent) + ylim(0, 30) +
  long_rb()

ggsave(file="rip_reltrad_gss_andy_all.png", type = "cairo-png", width = 15, height = 12)

pd <- position_dodge(width = 0.15)

ggplot(t2, aes(x=year, y= pct, group= label, color = label)) + 
  geom_line(aes(y= pct), size = 2) + 
  #geom_jitter(position = pd) +
  #geom_errorbar(aes(ymin = min, ymax=max), width = .15, size = 1.15, position = pd) +
  #geom_ribbon(aes(ymin = min, ymax=max), fill = "grey70", alpha = .3) + 
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("There is Little Change in Estimates of Evangelicals 2006-2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=40, family="KerkisSans")) + 
  scale_color_manual(values=c("indianred1", "red4",  "cornflowerblue", "darkblue" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Year") + ylim(0,30)  + theme(legend.title=element_blank()) +
  #geom_dl(aes(label = label), method = list(dl.trans(x = x + 5, y = y + .75), cex =2, "first.points"))
  
ggsave(file="rip_line_without_ci.png", type = "cairo-png", width = 15, height = 12)

