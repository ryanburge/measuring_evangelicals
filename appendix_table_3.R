
gss$notblack <- ifelse(gss$race !=2, 1, 0)

gss <- gss %>% 
  mutate(baprot = baprot + notblack) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))


gss %>% 
  filter(year ==2008) %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


gss %>% 
  filter(year ==2010) %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

gss %>% 
  filter(year ==2012) %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

gss %>% 
  filter(year ==2014) %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

gss %>% 
  filter(year ==2016) %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


cces08$notblack <- ifelse(cces08$V211 !=2, 1, 0)

cces08 <- cces08 %>% 
  mutate(baprot = baprot + notblack) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))

cces10$notblack <- ifelse(cces10$V211 !=2, 1, 0)

cces10 <- cces10 %>% 
  mutate(baprot = baprot + notblack) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))

cces12$notblack <- ifelse(cces12$race !=2, 1, 0)

cces12 <- cces12 %>% 
  mutate(baprot = baprot + notblack) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))

cces14$notblack <- ifelse(cces14$race !=2, 1, 0)

cces14 <- cces14 %>% 
  mutate(baprot = baprot + notblack) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))


cces16$notblack <- ifelse(cces16$race !=2, 1, 0)

cces16 <- cces16 %>% 
  mutate(baprot = baprot + notblack) %>% 
  mutate(baprot = recode(baprot, "2=1; else=0"))


cces08 %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


cces10 %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


cces12 %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


cces14 %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()



cces16 %>% 
  tabyl(evangelical, baprot) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()
