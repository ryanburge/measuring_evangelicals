library(tidyverse)
library(car)

## 2016 Results

cces16 <- read_dta("D://cces/data/cces16.dta")

cces16$vote16<-Recode(cces16$CC16_410a,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")

## This is White Born Again

cces16 %>%  filter(pew_bornagain ==1 & race ==1 & complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))

# A tibble: 8 x 3
vote16          n       weight
<chr+lbl>      <dbl>        <dbl>
1    Donald Trump 7860.49080 0.7577498731
2 Hillary Clinton 1954.66491 0.1884293387
3    Gary Johnson  232.37631 0.0224010340
4           Other  226.00167 0.0217865191
5   Evan McMullin   43.08202 0.0041530987
6      Jill Stein   36.15182 0.0034850288
7        Not Sure   10.35290 0.0009980174
8        Not Vote   10.34328 0.0009970902

## This is White Born Again Protestants

cces16 %>%  filter(pew_bornagain ==1 & race ==1 & religpew ==1 & complete.cases(vote16)) %>% 
  count(vote16, wt = commonweight_vv_post) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))

# A tibble: 8 x 3
vote16           n       weight
<chr+lbl>       <dbl>        <dbl>
1    Donald Trump 6274.856140 0.7872096314
2 Hillary Clinton 1263.158384 0.1584690428
3    Gary Johnson  193.368213 0.0242589337
4           Other  172.047906 0.0215842030
5   Evan McMullin   30.055464 0.0037705965
6      Jill Stein   22.500098 0.0028227410
7        Not Vote    8.028349 0.0010071934
8        Not Sure    6.995822 0.0008776581

## 2012 Results

cces12 <- read_dta("D://cces/data/cces12.dta")

cces12$vote12<-Recode(cces12$CC410a,"1='Barack Obama';
                      2='Mitt Romney';
                      4='Other';
                      5= 'Not Vote';
                      6= 'Not Vote';
                      7= 'Not Sure';
                      8= 'Skipped'; else = NA")

## This is White Born Again

cces12 %>%  filter(pew_bornagain ==1 & race ==1 & complete.cases(vote12)) %>% 
  count(vote12, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))

# A tibble: 5 x 3
vote12          n       weight
<chr+lbl>      <dbl>        <dbl>
1  Mitt Romney 7132.44689 0.7707735149
2 Barack Obama 1969.73804 0.2128612998
3        Other  128.64601 0.0139022324
4     Not Sure   15.21019 0.0016437007
5     Not Vote    7.58105 0.0008192522

## This is White Born Again Protestants

cces12 %>%  filter(pew_bornagain ==1 & race ==1 & religpew ==1 & complete.cases(vote12)) %>% 
  count(vote12, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))

# A tibble: 5 x 3
vote12           n       weight
<chr+lbl>       <dbl>        <dbl>
1  Mitt Romney 5921.526045 0.7810322754
2 Barack Obama 1527.581557 0.2014836193
3        Other  113.508241 0.0149714110
4     Not Sure   11.737096 0.0015480892
5     Not Vote    7.313314 0.0009646051

cces08 <- read_dta("D://cces/data/cces2008.dta")

cces08$vote08<-Recode(cces08$CC410,"1='John McCain';
                      2='Barack Obama';
                      3='Robert Barr';
                      4='Cynthia McKinney';
                      5= 'Ralph Nader';
                      6= 'Ron Paul';
                      7= 'Other';
                      8= 'Not Vote'; else = NA")

## This is White Born Again

cces08 %>%  filter(V215 ==1 & V211 ==1 & complete.cases(vote08)) %>% 
  count(vote08, wt = V201) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))

# A tibble: 8 x 3
vote08           n       weight
<chr+lbl>       <dbl>        <dbl>
1      John McCain 4128.860715 0.7644267597
2     Barack Obama 1202.176898 0.2225737932
3            Other   26.433326 0.0048939267
4      Robert Barr   18.535183 0.0034316463
5         Ron Paul   11.123121 0.0020593602
6      Ralph Nader    7.401571 0.0013703439
7         Not Vote    3.496524 0.0006473545
8 Cynthia McKinney    3.223550 0.0005968155

## This is White Born Again Protestants

cces08 %>%  filter(V215 ==1 & V211 ==1 & V219 ==1 & complete.cases(vote08)) %>% 
  count(vote08, wt = V201) %>% 
  mutate(weight = prop.table(n)) %>% arrange(desc(weight))

# A tibble: 8 x 3
vote08           n       weight
<chr+lbl>       <dbl>        <dbl>
1      John McCain 2755.221099 0.7820148780
2     Barack Obama  719.228915 0.2041388665
3      Robert Barr   17.800233 0.0050522433
4            Other   16.386649 0.0046510255
5         Ron Paul    7.582767 0.0021522181
6      Ralph Nader    3.440957 0.0009766473
7         Not Vote    1.963349 0.0005572578
8 Cynthia McKinney    1.609637 0.0004568635

