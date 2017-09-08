library(tidyverse)
library(haven)
library(car)
library(scales)
library(extrafont)


cces08 <- read_dta("D:/cces/data/cces08.dta")
cces12 <- read_dta("D:/cces/data/cces12.dta")
cces14 <- read_dta("D:/cces/data/cces14.dta")
cces16 <- read_dta("D:/cces/data/cces16.dta")

gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")


cces08$white <- Recode(cces08$V211, "1=1; else=0")
cces08$bagain <- Recode(cces08$V215, "1=1; else=0")
cces08$protestant <- Recode(cces08$V219, "1=1;else=0")
cces08 <- cces08 %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

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
c2 <-cces12 %>% count(whtbaprot, wt = weight_vv_post) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2012"))
c3 <-cces14 %>% count(whtbaprot, wt = weight) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2014"))
c4 <-cces16 %>% count(whtbaprot, wt = commonweight_vv_post) %>% mutate(weight = prop.table(n)) %>% filter(whtbaprot ==1) %>% select(weight) %>% mutate(year = c("2016"))

cgraph <- bind_rows(c1, c2, c3, c4) %>% mutate(source = c("CCES")) %>% mutate(year = as.numeric(year))

gss$bagain <- Recode(gss$reborn, "1=1; else =0")
gss$white <- Recode(gss$race, "1=1; else =0")
gss$protestant <- Recode(gss$relig, "1=1; else =0")
gss <- gss %>% mutate(whtbaprot = white + bagain + protestant) %>% mutate(whtbaprot = recode(whtbaprot, "3=1; else=0"))

g1 <- gss %>% filter(year >= 2006) %>% 
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
  theme(text=element_text(size=18, family="KerkisSans")) + 
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

graph %>%  
ggplot(., aes(x=year, y=weight, group = source, color = source)) + geom_line(size = 1.25)  +
  theme(axis.ticks = element_blank()) + ylab("Percent of Sample") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Year")  + 
  scale_y_continuous(labels = scales::percent) + theme(legend.title=element_blank()) 

ggsave(file="rip_all.png", type = "cairo-png", width = 15, height = 12)




