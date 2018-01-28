# cces12 <- read_dta("D://cces/data/cces12.dta")


cces12 <- cces12 %>% 
  mutate(white = recode(race, "1=1; else=0"))

## Baptist

cces12 <- cces12 %>%
  mutate(sbc = recode(cces12$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(abc = recode(cces12$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(ibc = recode(cces12$religpew_baptist, "5=1; else=0")) 

cces12 <- cces12 %>%
  mutate(bgc = recode(cces12$religpew_baptist, "6=1; else=0")) 

cces12 <- cces12 %>%
  mutate(mbc = recode(cces12$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(cb = recode(cces12$religpew_baptist, "8=1; else=0")) 

cces12 <- cces12 %>%
  mutate(fwb = recode(cces12$religpew_baptist, "9=1; else=0")) 

cces12 <- cces12 %>%
  mutate(gabb = recode(cces12$religpew_baptist, "10=1; else=0")) 

cces12 <- cces12 %>%
  mutate(obc = recode(cces12$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces12 <- cces12 %>%
  mutate(fmc = recode(cces12$religpew_methodist, "2=1; else=0")) 

cces12 <- cces12 %>%
  mutate(omc = recode(cces12$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces12 <- cces12 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces12 <- cces12 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces12 <- cces12 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces12 <- cces12 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces12 <- cces12 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces12 <- cces12 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces12 <- cces12 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))