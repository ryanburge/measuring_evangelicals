
# cces10 <- read_dta("D://cces/data/cces10.dta")



cces10 <- cces10 %>% 
  mutate(pew_churatd = V217, religpew = V219, religpew_protestant= V220, religpew_baptist = V222, religpew_method = V223, religpew_nondenom = V224, religpew_lutheran = V225, religpew_presby = V226, religpew_pentecost = V227, religpew_episcop = V228, religpew_christian = V229, religpew_congreg = V230, religpew_holiness = V231, religpew_reformed = V232, religpew_advent = V233, religpew_catholic = V234, religpew_mormon = V235, religpew_orthodox = V236, religpew_jewish = V237, religpew_muslim = V238, religpew_buddhist = V239, religpew_hindu = V240)

cces10 <- cces10 %>% 
  mutate(race = V211)



cces10 <- cces10 %>% 
  mutate(white = recode(race, "1=1; else=0"))

## Baptist

cces10 <- cces10 %>%
  mutate(sbc = recode(cces10$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(abc = recode(cces10$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(ibc = recode(cces10$religpew_baptist, "5=1; else=0")) 

cces10 <- cces10 %>%
  mutate(bgc = recode(cces10$religpew_baptist, "6=1; else=0")) 

cces10 <- cces10 %>%
  mutate(mbc = recode(cces10$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(cb = recode(cces10$religpew_baptist, "8=1; else=0")) 

cces10 <- cces10 %>%
  mutate(fwb = recode(cces10$religpew_baptist, "9=1; else=0")) 

cces10 <- cces10 %>%
  mutate(gabb = recode(cces10$religpew_baptist, "10=1; else=0")) 

cces10 <- cces10 %>%
  mutate(obc = recode(cces10$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces10 <- cces10 %>%
  mutate(fmc = recode(cces10$religpew_methodist, "2=1; else=0")) 

cces10 <- cces10 %>%
  mutate(omc = recode(cces10$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces10 <- cces10 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces10 <- cces10 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces10 <- cces10 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces10 <- cces10 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces10 <- cces10 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces10 <- cces10 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces10 <- cces10 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces10 <- cces10 %>% 
  mutate(abc = recode(cces10$religpew_baptist, "2=1; 4=1; else=0"))

cces10 <- cces10 %>% 
  mutate(epis = recode(cces10$religpew_episcop, "1:90=1; else=0"))

cces10 <- cces10 %>% 
  mutate(luth = recode(cces10$religpew_lutheran, "1=1; 4=1; else=0"))

cces10 <- cces10 %>% 
  mutate(meth = recode(cces10$religpew_methodist, "1=1; 90=1; else=0"))

cces10 <- cces10 %>% 
  mutate(pres = recode(cces10$religpew_presby, "1=1; 90=1; else=0"))

cces10 <- cces10 %>% 
  mutate(cong = recode(cces10$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces10 <- cces10 %>% 
  mutate(doc = recode(cces10$religpew_protestant, "8=1; else=0"))

cces10 <- cces10 %>% 
  mutate(reform = recode(cces10$religpew_protestant, "11=1; else=0"))

cces10 <- cces10 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces10 <- cces10 %>% 
  mutate(black = recode(race, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(meth = recode(cces10$religpew_methodist, "3:4=1; else=0"))

cces10 <- cces10 %>%
  mutate(sbc = recode(cces10$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(nbap = recode(cces10$religpew_baptist, "3=1; else=0"))

cces10 <- cces10 %>%
  mutate(abc = recode(cces10$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(miss = recode(cces10$religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(obap = recode(cces10$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(ometh = recode(cces10$religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(apos = recode(cces10$religpew_pentecost, "6=1; 7=1; else=0"))

cces10 <- cces10 %>%
  mutate(open = recode(cces10$religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces10 <- cces10 %>%
  mutate(holy = recode(cces10$religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces10 <- cces10 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces10 <- cces10 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

cces10 <- cces10 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

cces10 <- cces10 %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

cces10 <- cces10 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))
