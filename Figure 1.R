library(tidyverse)
library(car)
library(extrafont)
library(haven)

cces16 <- read_dta("D://cces/data/cces16.dta")


cces16$evanbaptist <- Recode(cces16$religpew_baptist, "1=1; 5:90=1; else=0")
cces16$evanmeth <- Recode(cces16$religpew_methodist, "2=1; else=0")
cces16$evannd <- Recode(cces16$religpew_nondenom, "1:90=1; else=0")
cces16$evanluth <- Recode(cces16$religpew_lutheran, "2:3=1; else=0")
cces16$evanpres <- Recode(cces16$religpew_presby, "6=1; else=0")
cces16$pente <- Recode(cces16$religpew_pentecost, "1:90=1; else=0")
cces16$evanchrist <- Recode(cces16$religpew_christian, "1=1; 3:4=1; else=0")
cces16$evancong <- Recode(cces16$religpew_congreg, "2=1; else=0")
cces16$evanholy <- Recode(cces16$religpew_holiness, "1:90=1; else=0")
cces16$evanadvent <- Recode(cces16$religpew_advent, "1:90=1; else=0")

cces16$evangelical <- cces16$evanbaptist + cces16$evanmeth + cces16$evannd + cces16$evanluth + cces16$evanpres + cces16$pente + cces16$evanchrist + cces16$evancong + cces16$evanholy + cces16$evanadvent
cces16$evangelical <- Recode(cces16$evangelical, "1:4=1; else=0")
cces16$white <- recode(cces16$race, "1=1; else=0")

evan16 <- cces16 %>% 
  filter(evangelical ==1 & white ==1) %>% 
  count(wt = commonweight_vv) %>% mutate(weight = n/64600) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2016"))

ba16 <- cces16 %>% 
  filter(pew_bornagain ==1 & religpew ==1 & white ==1) %>% 
  count(wt = commonweight_vv) %>% mutate(weight = n/64600) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2016"))

cces12 <- read_dta("D://cces/data/cces12.dta")

cces12$evanbaptist <- Recode(cces12$religpew_baptist, "1=1; 5:90=1; else=0")
cces12$evanmeth <- Recode(cces12$religpew_methodist, "2=1; else=0")
cces12$evannd <- Recode(cces12$religpew_nondenom, "1:90=1; else=0")
cces12$evanluth <- Recode(cces12$religpew_lutheran, "2:3=1; else=0")
cces12$evanpres <- Recode(cces12$religpew_presby, "6=1; else=0")
cces12$pente <- Recode(cces12$religpew_pentecost, "1:90=1; else=0")
cces12$evanchrist <- Recode(cces12$religpew_christian, "1=1; 3:4=1; else=0")
cces12$evancong <- Recode(cces12$religpew_congreg, "2=1; else=0")
cces12$evanholy <- Recode(cces12$religpew_holiness, "1:90=1; else=0")
cces12$evanadvent <- Recode(cces12$religpew_advent, "1:90=1; else=0")


cces12$evangelical <- cces12$evanbaptist + cces12$evanmeth + cces12$evannd + cces12$evanluth + cces12$evanpres + cces12$pente + cces12$evanchrist + cces12$evancong + cces12$evanholy + cces12$evanadvent
cces12$evangelical <- Recode(cces12$evangelical, "1:4=1; else=0")
cces12$white <- recode(cces12$race, "1=1; else=0")

evan12 <- cces12 %>% 
  filter(evangelical ==1 & white ==1) %>% 
  count(wt = weight_vv) %>% mutate(weight = n/54535) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2012"))

ba12 <- cces12 %>% 
  filter(pew_bornagain ==1 & religpew ==1 & white ==1) %>% 
  count(wt = weight_vv) %>% mutate(weight = n/54535) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2012"))

cces08 <- read_dta("D://cces/data/cces2008.dta")


cces08$evanbaptist <- Recode(cces08$V222, "1=1; 5:90=1; else=0")
cces08$evanmeth <- Recode(cces08$V223, "2=1; else=0")
cces08$evannd <- Recode(cces08$V224, "1:90=1; else=0")
cces08$evanluth <- Recode(cces08$V225, "2:3=1; else=0")
cces08$evanpres <- Recode(cces08$V226, "6=1; else=0")
cces08$pente <- Recode(cces08$V227, "1:90=1; else=0")
cces08$evanchrist <- Recode(cces08$V229, "1=1; 3:4=1; else=0")
cces08$evancong <- Recode(cces08$V230, "2=1; else=0")
cces08$evanholy <- Recode(cces08$V231, "1:90=1; else=0")
cces08$evanreform <- Recode(cces08$V232, "2=1; else=0")
cces08$evanadvent <- Recode(cces08$V233, "1:90=1; else=0")
cces08$evangelical <- cces08$evanbaptist + cces08$evanmeth + cces08$evannd + cces08$evanluth + cces08$evanpres + cces08$pente + cces08$evanchrist + cces08$evancong + cces08$evanholy + cces08$evanadvent + cces08$evanreform
cces08$evangelical <- Recode(cces08$evangelical, "1:4=1; else=0")

cces08$white <- Recode(cces08$V211, "1=1; else=0")
cces08$bagain <- Recode(cces08$V215, "1=1; else=0")
cces08$protestant <- Recode(cces08$V219, "1=1; else=0")

evan08 <- cces08 %>% 
  filter(evangelical ==1 & white ==1) %>% 
  count(wt = V201) %>% mutate(weight = n/32800) %>% 
  mutate(sample = c("Affiliation")) %>% 
  mutate(survey = c("CCES 2008"))

ba08 <- cces08 %>% 
  filter(bagain ==1 & protestant ==1 & white ==1) %>% 
  count(wt = V201) %>% mutate(weight = n/32800) %>% 
  mutate(sample = c("Self ID")) %>% 
  mutate(survey = c("CCES 2008"))

cces1 <- bind_rows(ba08, evan08, ba12, evan12, ba16, evan16) %>% select(-n)


gss <- read_dta("C:/Users/Ryan Burge/Desktop/gss_reltrad.dta")
gss <- gss %>% filter(year >= 2008)

gssba <- gss %>% 
  filter(relig ==1 & reborn ==1 & race ==1) %>% 
  group_by(year) %>% 
  count(wt = wtssall) %>% 
  mutate(sample = c("Self ID")) %>% 
  rename(count = n)

gssba <- gss %>% 
  group_by(year) %>% 
  count(wt = wtssall) %>% 
  left_join(gssba) %>% 
  mutate(weight = count/n) %>% 
  ungroup(year) %>% 
  select(year, weight, sample) 

gssba$survey <- paste("GSS", gssba$year, sep = " ")
gssba <- gssba %>% select(-year)


gssevan <- gss %>% 
  filter(evangelical ==1 & race ==1) %>% 
  group_by(year) %>% 
  count(wt = wtssall) %>% 
  mutate(sample = c("Affiliation")) %>% 
  rename(count = n)

gssevan <- gss %>% 
  group_by(year) %>% 
  count(wt = wtssall) %>% 
  left_join(gssevan) %>% 
  mutate(weight = count/n) %>% 
  ungroup(year) %>% 
  select(year, weight, sample) 

gssevan$survey <- paste("GSS", gssevan$year, sep = " ")
gssevan <- gssevan %>% select(-year)

gss1 <- bind_rows(gssevan, gssba)

total <- bind_rows(cces1, gss1)


total <- rbind(gss1, cces1) %>% mutate(weight = weight * 100)



total$moe <- c(4.28, 4.45, 4.06, 3.77, 4.54, 4.56, 4.05, 3.68, 1.15, 1.33, .85, .89, .8, .87)

limits <- aes(ymax = total$weight + total$moe, ymin = total$weight - total$moe)

total$max <- total$weight + total$moe
total$min <- total$weight - total$moe


ggplot(total, aes(x=survey, y=weight, fill = sample)) + geom_col(position = "dodge")+ 
  geom_errorbar(aes(ymin = min, ymax=max), width = .25, position=position_dodge(.9), color = "azure4") +
  theme(axis.ticks = element_blank()) + ylab("Percent of Respondents") + 
  theme(legend.position="bottom") +
  ggtitle("Difference in Measuring Evangelicals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("grey","black", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Survey and Year") 

ggsave(file="freq_with_weights_final.png", type = "cairo-png", width = 15, height = 15)
