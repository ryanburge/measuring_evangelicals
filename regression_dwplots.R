library(dplyr)
library(haven)
library(car)
library(dotwhisker)
library(broom)


cces16 <- read_dta("C:/Users/Ryan Burge/Dropbox/data/cces.dta")
cces12 <- read_dta("C:/Users/Ryan Burge/Dropbox/data/cces12.dta")
cces08 <- read_dta("C:/Users/Ryan Burge/Dropbox/data/cces2008.dta")

## Create evangelical using other syntax. 

## White Variable

cces08$white <- Recode(cces08$V211, "1=1; else=0")
cces12$white <- Recode(cces12$race, "1=1; else=0")
cces16$white <- Recode(cces16$race, "1=1; else=0")

## Born Again

cces08$bagain <- Recode(cces08$V215, "1=1; else=0")
cces12$bagain <- Recode(cces12$pew_bornagain, "1=1; else=0")
cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=0")

## Protestant

cces08$protestant <- Recode(cces08$V219, "1=1; else=0")
cces12$protestant <- Recode(cces12$religpew, "1=1; else=0")
cces16$protestant <- Recode(cces16$religpew, "1=1; else=0")


## Abortion question coding


cces08$abort <- Recode(cces08$CC310, "4=1; else=0")
cces12$abort <- Recode(cces12$CC324, "4=1; else=0")
cces16$abort <- Recode(cces16$CC16_332a, "1=1; else=0")


## Cleaning up Controls

cces08$age <- 2008- cces08$V207
cces08$age <- cces08$age/100

cces12$age <- 2012- cces12$birthyr
cces12$age <- cces12$age/94

cces16$age <- 2016- cces16$birthyr
cces16$age <- cces16$age/99

cces08$educ2 <- cces08$V213/6
cces12$educ2 <- cces12$educ/6
cces16$educ2 <- cces16$educ/6

cces08$male <- Recode(cces08$V208, "1=1; else=0")
cces12$male <- Recode(cces12$gender, "1=1; else=0")
cces16$male <- Recode(cces16$gender, "1=1; else=0")

cces08$pid7 <- cces08$CC307a
cces08$pid7[cces08$pid7==8] <- NA
cces12$pid7[cces12$pid7==8] <- NA
cces16$pid7[cces16$pid7==8] <- NA

cces08$pid <- cces08$pid7/7
cces12$pid <- cces12$pid7/7
cces16$pid <- cces16$pid7/7


baprot16 <- filter(cces16, protestant ==1 & bagain ==1)
evan16 <- filter(cces16, evangelical ==1)

baprot12 <- filter(cces12, protestant ==1 & bagain ==1)
evan12 <- filter(cces12, evangelical ==1)

baprot08 <- filter(cces08, protestant ==1 & bagain ==1)
evan08 <- filter(cces08, evangelical ==1)

reg1 <- glm(abort ~ educ2 + male + age + pid, data = baprot16)
reg2 <- glm(abort ~ educ2 + male + age + pid, data = evan16)

reg1 <- tidy(reg1) %>% mutate(model = "Self ID")
reg2 <- tidy(reg2) %>% mutate(model = "Affiliation")


reg3 <- glm(abort ~ educ2 + male + age + pid, data = baprot12)
reg4 <- glm(abort ~ educ2 + male + age + pid, data = evan12)

reg3 <- tidy(reg3) %>% mutate(model = "Self ID")
reg4 <- tidy(reg4) %>% mutate(model = "Affiliation")

reg5 <- glm(abort ~ educ2 + male + age + pid, data = baprot08)
reg6 <- glm(abort ~ educ2 + male + age + pid, data = evan08)

reg5 <- tidy(reg5) %>% mutate(model = "Self ID")
reg6 <- tidy(reg6) %>% mutate(model = "Affiliation")

model16 <- rbind(reg1, reg2)
model12 <- rbind(reg3, reg4)
model08 <- rbind(reg5, reg6)

cc16 <- dwplot(model16, dodge_size = .15) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age = "Age", pid = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
   scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=14, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", title="Predicting Support for Abortion ", caption="Data from CCES 2016") 


ggsave(file="c16_dwplot.png", type = "cairo-png", width = 12, height = 8)


cc12 <- dwplot(model12, dodge_size = .15) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age = "Age", pid = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Support for Abortion ",
                                                                        caption="Data from CCES 2012") 
ggsave(file="c12_dwplot.png", type = "cairo-png", width = 12, height = 8)


cc08 <- dwplot(model08, dodge_size = .05) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age = "Age", pid = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Support for Abortion ",
                                                                        caption="Data from CCES 2008") 
ggsave(file="c08_dwplot.png", type = "cairo-png", width = 12, height = 8)


#### Doing the same with GSS data

gss10 <- read_dta("C:/Users/Ryan Burge/Dropbox/data/gss10.dta")
gss12 <- read_dta("C:/Users/Ryan Burge/Dropbox/data/gss12.dta")
gss14 <- read_dta("C:/Users/Ryan Burge/Dropbox/data/gss14.dta")
gss16 <- read_dta("D:/cces/data/gss16.dta")

gss$white <- Recode(gss$race, "1=1;else=0")
gss10$white <- Recode(gss10$race, "1=1;else=0")
gss12$white <- Recode(gss12$race, "1=1;else=0")
gss14$white <- Recode(gss14$race, "1=1;else=0")
gss16$white <- Recode(gss16$race, "1=1;else=0")

gss$bagain <- Recode(gss$bagain, "1=1;else=0")
gss10$bagain <- Recode(gss10$reborn, "1=1;else=0")
gss12$bagain <- Recode(gss12$reborn, "1=1;else=0")
gss14$bagain <- Recode(gss14$reborn, "1=1;else=0")
gss16$bagain <- Recode(gss16$reborn, "1=1;else=0")

gss$protestant <- Recode(gss$protestant, "1=1;else=0")
gss10$protestant <- Recode(gss10$relig, "1=1;else=0")
gss12$protestant <- Recode(gss12$relig, "1=1;else=0")
gss14$protestant <- Recode(gss14$relig, "1=1;else=0")
gss16$protestant <- Recode(gss16$relig, "1=1;else=0")

gss$abort <- Recode(gss$abany, "1=1;else=0")
gss10$abort <- Recode(gss10$abany, "1=1; else=0")
gss12$abort <- Recode(gss12$abany, "1=1; else=0")
gss14$abort <- Recode(gss14$abany, "1=1; else=0")
gss16$abort <- Recode(gss16$abany, "1=1; else=0")

gss$age2 <- gss$age/89
gss10$age2 <- gss10$age/89
gss12$age2 <- gss12$age/89
gss14$age2 <- gss14$age/89
gss16$age2 <- gss16$age/89

gss$educ2 <- Recode(gss$educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")
gss10$educ2 <- Recode(gss10$educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")
gss12$educ2 <- Recode(gss12$educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")
gss14$educ2 <- Recode(gss14$educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")
gss16$educ2 <- Recode(gss16$educ, "1:11 =1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6")

gss$educ2 <- gss10$educ2/6
gss10$educ2 <- gss10$educ2/6
gss12$educ2 <- gss12$educ2/6
gss14$educ2 <- gss14$educ2/6
gss16$educ2 <- gss16$educ2/6

gss$male <- Recode(gss$sex, "1=1; else=0")
gss10$male <- Recode(gss10$sex, "1=1; else=0")
gss12$male <- Recode(gss12$sex, "1=1; else=0")
gss14$male <- Recode(gss14$sex, "1=1; else=0")
gss16$male <- Recode(gss16$sex, "1=1; else=0")


gss$pid7 <- gss$partyid + 1 
gss$pid7[gss$pid7==8] <- NA

gss10$pid7 <- gss10$partyid + 1 
gss10$pid7[gss10$pid7==8] <- NA
gss12$pid7 <- gss12$partyid + 1 
gss12$pid7[gss12$pid7==8] <- NA
gss14$pid7 <- gss14$partyid + 1 
gss14$pid7[gss14$pid7==8] <- NA
gss16$pid7 <- gss16$partyid + 1 
gss16$pid7[gss16$pid7==8] <- NA

gss$pid7 <- gss$pid7/7
gss10$pid7 <- gss10$pid7/7
gss12$pid7 <- gss12$pid7/7
gss14$pid7 <- gss14$pid7/7
gss16$pid7 <- gss16$pid7/7

baprot16 <- filter(gss, protestant==1 & bagain==1 & year ==2016)
baprot14 <- filter(gss, protestant==1 & bagain==1 & year ==2014)
baprot12 <- filter(gss, protestant==1 & bagain==1 & year ==2012)
baprot10 <- filter(gss, protestant==1 & bagain==1 & year ==2010)
baprot08 <- filter(gss, protestant==1 & bagain==1 & year ==2008)

evan16 <- filter(gss, evangelical==1 & year ==2016)
evan14 <- filter(gss, evangelical==1 & year ==2014)
evan12 <- filter(gss, evangelical==1 & year ==2012)
evan10 <- filter(gss, evangelical==1 & year ==2010)
evan08 <- filter(gss, evangelical==1 & year ==2008)


reg1 <- glm(abort ~ educ2 + male + age2 + pid7, data = baprot08)
reg2 <- glm(abort ~ educ2 + male + age2 + pid7, data = evan08)

reg1 <- tidy(reg1) %>% mutate(model = "Self ID")
reg2 <- tidy(reg2) %>% mutate(model = "Affiliation")

reg3 <- glm(abort ~ educ2 + male + age2 + pid7, data = baprot10)
reg4 <- glm(abort ~ educ2 + male + age2 + pid7, data = evan10)

reg3 <- tidy(reg3) %>% mutate(model = "Self ID")
reg4 <- tidy(reg4) %>% mutate(model = "Affiliation")

reg5 <- glm(abort ~ educ2 + male + age2 + pid7, data = baprot12)
reg6 <- glm(abort ~ educ2 + male + age2 + pid7, data = evan12)

reg5 <- tidy(reg5) %>% mutate(model = "Self ID")
reg6 <- tidy(reg6) %>% mutate(model = "Affiliation")

reg7 <- glm(abort ~ educ2 + male + age2 + pid7, data = baprot14)
reg8 <- glm(abort ~ educ2 + male + age2 + pid7, data = evan14)

reg7 <- tidy(reg7) %>% mutate(model = "Self ID")
reg8 <- tidy(reg8) %>% mutate(model = "Affiliation")

reg9 <- glm(abort ~ educ2 + male + age2 + pid7, data = baprot16)
reg10 <- glm(abort ~ educ2 + male + age2 + pid7, data = evan16)

reg9 <- tidy(reg9) %>% mutate(model = "Self ID")
reg10 <- tidy(reg10) %>% mutate(model = "Affiliation")

model08 <- rbind(reg1, reg2)
model10 <- rbind(reg3, reg4)
model12 <- rbind(reg5, reg6)
model14 <- rbind(reg7, reg8)
model16 <- rbind(reg9, reg10)

g08 <- dwplot(model08, dodge_size = .05) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age2 = "Age", pid7 = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
                                                                        title="Predicting Support for Abortion ",
                                                                        caption="Data from GSS 2008")  
ggsave(file="g08_dwplot.png", type = "cairo-png", width = 12, height = 8)

g10 <- dwplot(model10, dodge_size = .05) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age2 = "Age", pid7 = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
       title="Predicting Support for Abortion ",
       caption="Data from GSS 2010")  
ggsave(file="g10_dwplot.png", type = "cairo-png", width = 12, height = 8)


g12 <- dwplot(model12, dodge_size = .05) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age2 = "Age", pid7 = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
       title="Predicting Support for Abortion ",
       caption="Data from GSS 2012")  
ggsave(file="g12_dwplot.png", type = "cairo-png", width = 12, height = 8)


g14 <- dwplot(model14, dodge_size = .05) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age2 = "Age", pid7 = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=16, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
       title="Predicting Support for Abortion ",
       caption="Data from GSS 2014")  
ggsave(file="g14_dwplot.png", type = "cairo-png", width = 12, height = 8)


g16 <- dwplot(model16, dodge_size = .05) %>% 
  relabel_predictors(c(educ2 ="Education", male = "Male", age2 = "Age", pid7 = "Republican ID")) +
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(-0.01, -5.7), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey()  +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=14, family="KerkisSans")) +
  labs(x="Coefficient Estimate", y="", 
       title="Predicting Support for Abortion ",
       caption="Data from GSS 2016")  

ggsave(file="g16_dwplot.png", type = "cairo-png", width = 12, height = 8)


grid.arrange(g16, cc16, ncol =2)


