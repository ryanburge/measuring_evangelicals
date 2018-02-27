library(fst)
library(tidyverse)
library(car)
library(survey)
library(srvyr)

cces08 <- read.fst("C://cces08.fst")
cces10 <- read.fst("C://cces10.fst")
cces12 <- read.fst("C://cces12.fst")
cces14 <- read.fst("C://cces14.fst")
cces16 <- read.fst("C://cces16.fst")

gss <- read.fst("C://gss.fst")

source("D://measuring_evangelicals/reltrad08.R")
source("D://measuring_evangelicals/reltrad10.R")
source("D://measuring_evangelicals/reltrad12.R")
source("D://measuring_evangelicals/reltrad14.R")
source("D://measuring_evangelicals/reltrad16.R")


source("D://measuring_evangelicals/making_baprot.R")