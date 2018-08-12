gss <- gss %>% 
  mutate(protestant = recode(relig, "1=1; else =0")) %>% 
  mutate(bagain = recode(reborn, "1=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else =0")) %>% 
  mutate(baprot = protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>% 
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))

gss <- gss %>% 
  mutate(att = recode(attend, "8=5; 6:7=4; 4:5=3; 3=2; 1:2=1; 0=0; else =NA"))

gss <- gss %>% 
  mutate(pid = recode(partyid, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; else= NA"))

gss %>%
  filter(year >= 2008) %>% 
  group_by(year) %>% 
  summarise_each(funs(t.test(.[baprot2 == 1], .[evangelical == 1])$p.value),  att, pid) 

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


all_cc <- all_cc %>% 
  mutate(protestant = recode(protestant, "1=1; else =0")) %>% 
  mutate(bagain = recode(bagain, "1=1; else =0")) %>% 
  mutate(black = recode(race, "2=1; else=0")) %>% 
  mutate(baprot = protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "2=1; else =0")) %>% 
  mutate(baprot2 = baprot - black) %>% 
  mutate(baprot2 = recode(baprot2, "1=1; else =0"))

all_cc <- all_cc %>% 
  mutate(pid = recode(pid7, "8:99= NA"))

all_cc %>%
  group_by(year) %>% 
  summarise_each(funs(t.test(.[baprot2 == 1], .[evangelical == 1])$p.value), vars = att,pid) %>% 
  mutate(vars = round(vars, 10)) %>% 
  mutate(pid = round(pid, 10)) %>% 
  rename(att = vars)





