
# cces14 <- read_dta("D://cces/data/cces14.dta")


cces14 <- cces14 %>% 
  mutate(white = recode(race, "1=1; else=0"))

## Baptist

cces14 <- cces14 %>%
  mutate(sbc = recode(cces14$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces14 <- cces14 %>%
  mutate(abc = recode(cces14$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces14 <- cces14 %>%
  mutate(ibc = recode(cces14$religpew_baptist, "5=1; else=0")) 

cces14 <- cces14 %>%
  mutate(bgc = recode(cces14$religpew_baptist, "6=1; else=0")) 

cces14 <- cces14 %>%
  mutate(mbc = recode(cces14$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces14 <- cces14 %>%
  mutate(cb = recode(cces14$religpew_baptist, "8=1; else=0")) 

cces14 <- cces14 %>%
  mutate(fwb = recode(cces14$religpew_baptist, "9=1; else=0")) 

cces14 <- cces14 %>%
  mutate(gabb = recode(cces14$religpew_baptist, "10=1; else=0")) 

cces14 <- cces14 %>%
  mutate(obc = recode(cces14$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces14 <- cces14 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces14 <- cces14 %>%
  mutate(fmc = recode(cces14$religpew_methodist, "2=1; else=0")) 

cces14 <- cces14 %>%
  mutate(omc = recode(cces14$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces14 <- cces14 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces14 <- cces14 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces14 <- cces14 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces14 <- cces14 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces14 <- cces14 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces14 <- cces14 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces14 <- cces14 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces14 <- cces14 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))