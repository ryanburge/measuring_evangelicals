
## Age

gss %>% filter(baprot ==1) %>% group_by(year) %>% summarise(mean = mean(age, na.rm = TRUE))

cces08 %>% filter(baprot ==1) %>%  mutate(age = 2008-V207) %>% summarise(mean = mean(age, na.rm = TRUE))
cces10 %>% filter(baprot ==1) %>%  mutate(age = 2010-V207) %>% summarise(mean = mean(age, na.rm = TRUE))
cces12 %>% filter(baprot ==1) %>%  mutate(age = 2012-birthyr) %>% summarise(mean = mean(age, na.rm = TRUE))
cces14 %>% filter(baprot ==1) %>%  mutate(age = 2014-birthyr) %>% summarise(mean = mean(age, na.rm = TRUE))
cces16 %>% filter(baprot ==1) %>%  mutate(age = 2016-birthyr) %>% summarise(mean = mean(age, na.rm = TRUE))


gss %>% filter(evangelical ==1) %>% group_by(year) %>% summarise(mean = mean(age, na.rm = TRUE))


cces08 %>% filter(evangelical ==1) %>%  mutate(age = 2008-V207) %>% summarise(mean = mean(age, na.rm = TRUE))
cces10 %>% filter(evangelical ==1) %>%  mutate(age = 2010-V207) %>% summarise(mean = mean(age, na.rm = TRUE))
cces12 %>% filter(evangelical ==1) %>%  mutate(age = 2012-birthyr) %>% summarise(mean = mean(age, na.rm = TRUE))
cces14 %>% filter(evangelical ==1) %>%  mutate(age = 2014-birthyr) %>% summarise(mean = mean(age, na.rm = TRUE))
cces16 %>% filter(evangelical ==1) %>%  mutate(age = 2016-birthyr) %>% summarise(mean = mean(age, na.rm = TRUE))

## Gender 

gss %>% filter(baprot ==1) %>% mutate(male = recode(sex, "1=1; else=0")) %>% group_by(year) %>% summarise(mean = mean(male, na.rm = TRUE))

cces08 %>% filter(baprot ==1) %>%  mutate(male = recode(V208, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces10 %>% filter(baprot ==1) %>%  mutate(male = recode(V208, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces12 %>% filter(baprot ==1) %>%  mutate(male = recode(gender, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces14 %>% filter(baprot ==1) %>%  mutate(male = recode(gender, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces16 %>% filter(baprot ==1) %>%  mutate(male = recode(gender, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))


gss %>% filter(evangelical ==1) %>% mutate(male = recode(sex, "1=1; else=0")) %>% group_by(year) %>% summarise(mean = mean(male, na.rm = TRUE))

cces08 %>% filter(evangelical ==1) %>%  mutate(male = recode(V208, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces10 %>% filter(evangelical ==1) %>%  mutate(male = recode(V208, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces12 %>% filter(evangelical ==1) %>%  mutate(male = recode(gender, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces14 %>% filter(evangelical ==1) %>%  mutate(male = recode(gender, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))
cces16 %>% filter(evangelical ==1) %>%  mutate(male = recode(gender, "1=1; else=0")) %>% summarise(mean = mean(male, na.rm = TRUE))



## Education

gss <- gss %>% mutate(ed2 = recode(educ, "1:11=1; 12=2; 13:14=3; 15=4; 16=5; 17:20=6; else=99 "))

gss %>% filter(baprot ==1) %>% filter(ed2 != 99) %>%  group_by(year) %>% summarise(mean = mean(ed2, na.rm = TRUE))

cces08 %>% filter(baprot ==1) %>% summarise(mean = mean(V213, na.rm = TRUE))
cces10 %>% filter(baprot ==1) %>% summarise(mean = mean(V213, na.rm = TRUE))
cces12 %>% filter(baprot ==1) %>% summarise(mean = mean(educ, na.rm = TRUE))
cces14 %>% filter(baprot ==1) %>% summarise(mean = mean(educ, na.rm = TRUE))
cces16 %>% filter(baprot ==1) %>% summarise(mean = mean(educ, na.rm = TRUE))

gss %>% filter(evangelical ==1) %>% mutate(male = recode(sex, "1=1; else=0")) %>% group_by(year) %>% summarise(mean = mean(male, na.rm = TRUE))

cces08 %>% filter(evangelical ==1) %>% summarise(mean = mean(V213, na.rm = TRUE))
cces10 %>% filter(evangelical ==1) %>% summarise(mean = mean(V213, na.rm = TRUE))
cces12 %>% filter(evangelical ==1) %>% summarise(mean = mean(educ, na.rm = TRUE))
cces14 %>% filter(evangelical ==1) %>% summarise(mean = mean(educ, na.rm = TRUE))
cces16 %>% filter(evangelical ==1) %>% summarise(mean = mean(educ, na.rm = TRUE))