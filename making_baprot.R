cces16 <- cces16 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(nba = recode(pew_bornagain, "2=1; else=0")) %>% 
  mutate(nbaprot = nba + protestant) %>% 
  mutate(nbaprot = recode(nbaprot, "2=1; else=0"))

cces14 <- cces14 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))


cces12 <- cces12 %>% 
  mutate(reborn = recode(pew_bornagain, "1=1; else=0")) %>%
  mutate(protestant = recode(religpew, "1=1; else=0")) %>% 
  mutate(baprot = protestant + reborn) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))


cces10 <- cces10 %>% 
  mutate(protestant = recode(V219, "1=1; else=0")) %>% 
  mutate(bagain = recode(V215, "1=1; else=0"))

cces10 <- cces10 %>% 
  mutate(baprot = protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))

cces08 <- cces08 %>% 
  mutate(white = recode(V211, "1=1; else=0")) %>% 
  mutate(protestant = recode(V219, "1=1; else=0")) %>% 
  mutate(bagain = recode(V215, "1=1; else=0"))

cces08 <- cces08 %>% 
  mutate(baprot = protestant + bagain) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(nba = recode(V215, "2=1; else=0")) %>% 
  mutate(nbaprot = nba + protestant) %>% 
  mutate(nbaprot = recode(nbaprot, "2=1; else=0"))

gss <- gss %>% 
  mutate(protestant = recode(relig, "1=1; else=0")) %>% 
  mutate(ba = recode(reborn, "1=1; else=0")) %>% 
  mutate(baprot =  protestant + ba) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0")) %>% 
  mutate(nba = recode(reborn, "2=1; else=0")) %>% 
  mutate(nbaprot = protestant + nba) %>% 
  mutate(nbaprot = recode(nbaprot, "2=1; else=0"))
