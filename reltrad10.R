
cces10 <- read_dta("D://cces/data/cces10.dta")


cces10 <- cces10 %>% 
  mutate(white = recode(V211, "1=1; else=0"))


## Baptist

cces10 <- cces10 %>%
  mutate(sbc = recode(cces10$V222, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(abc = recode(cces10$V222, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(ibc = recode(cces10$V222, "5=1; else=0")) 

cces10 <- cces10 %>%
  mutate(bgc = recode(cces10$V222, "6=1; else=0")) 

cces10 <- cces10 %>%
  mutate(mbc = recode(cces10$V222, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(cb = recode(cces10$V222, "8=1; else=0")) 

cces10 <- cces10 %>%
  mutate(fwb = recode(cces10$V222, "9=1; else=0")) 

cces10 <- cces10 %>%
  mutate(gabb = recode(cces10$V222, "10=1; else=0")) 

cces10 <- cces10 %>%
  mutate(obc = recode(cces10$V222, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces10 <- cces10 %>%
  mutate(fmc = recode(cces10$V223, "2=1; else=0")) 

cces10 <- cces10 %>%
  mutate(omc = recode(cces10$V223, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces10 <- cces10 %>% 
  mutate(hiatt = recode(V217, "1:3=1; else=0")) %>% 
  mutate(nd = recode(V224, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces10 <- cces10 %>% 
  mutate(mz = recode(V225, "2=1; else=0")) %>% 
  mutate(wi = recode(V225, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces10 <- cces10 %>% 
  mutate(pca = recode(V226, "2=1; else=0")) %>% 
  mutate(epc = recode(V226, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces10 <- cces10 %>% 
  mutate(evanpent = recode(V227, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces10 <- cces10 %>% 
  mutate(evancong = recode(V230, "2=1; else=0"))

## Holiness
cces10 <- cces10 %>% 
  mutate(evanholy = recode(V231, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces10 <- cces10 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))