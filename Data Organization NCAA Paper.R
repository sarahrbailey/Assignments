combo <- read.csv("~/Mathematics/Research/Rankings/combo.csv")
View(combo)
top <- combo[,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]

top <- combo[top]
bottom <- c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")
bottom <- combo[bottom]
View(top)
meanst <- rowMeans(top)
meansb <- rowMeans(bottom)
t.test(meanst,meansb, paired=TRUE)
t.test(meanst,meansb, alternative="greater", paired=TRUE)
topm1 <-combo[1,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt1 <- rowMeans(topm1)
botm1 <-combo[1,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb1 <-rowMeans(botm1)
topm2 <-combo[2,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt2 <-rowMeans(topm2)
botm2 <-combo[2,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb2 <-rowMeans(botm2)
topm3 <-combo[3,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt3 <-rowMeans(topm3)
botm3<-combo[3,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb3 <-rowMeans(botm3)
topm4 <-combo[4,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt4 <-rowMeans(topm4)
botm4<-combo[4,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb4 <-rowMeans(botm4)
topm5 <-combo[5,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt5 <-rowMeans(topm5)
botm5<-combo[5,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb5 <-rowMeans(botm5)
topm6 <-combo[6,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt6 <-rowMeans(topm6)
botm6<-combo[6,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb6 <-rowMeans(botm6)
topm7 <-combo[7,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt7 <-rowMeans(topm7)
botm7<-combo[7,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb7 <-rowMeans(botm7)
topm8 <-combo[8,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
meansmt8 <-rowMeans(topm8)
botm8<-combo[8,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
meansmb8 <-rowMeans(botm8)
mentop<-combo[1:17,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
menbot<-combo[1:17,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
mentopmean <-rowMeans(mentop)
menbotmean<- rowMeans(menbot)
t.test(mentopmean, menbotmean, paired=TRUE) #mens mean t.test
t.test(mentopmean, womentopmean,paired=FALSE)
t.test(mentopmean, womentopmean,alternative= "less", paired=FALSE)
t.test(menbotmean, womenbotmean, paired=FALSE)
womentop<-combo[18:34,c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
womenbot<-combo[18:34,c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
womentopmean<-rowMeans(womentop)
womenbotmean<- rowMeans(womenbot)
t.test(womentopmean, womenbotmean, paired=TRUE) #womens mean t.test
t.test(meanst~combo$M.W, alternative="less", paired=FALSE) #independent t.test between top men and women
MostWins <- read.csv("~/Mathematics/Research/Rankings/MostWins.csv", header=TRUE)
NTop<-MostWins[,c("NTOPS")]
AVGAPR<-MostWins[,c("AVGAPR")]
MostLoss <- read.csv("~/Mathematics/Research/Rankings/MostLoss.csv", header=TRUE)
NBot<-MostLoss[,c("Nbots")]
Group = ifelse((MostWins$NTOPS <= 1), "L", "H")
t.test(AVGAPR~Group, alternative="greater", paired=FALSE)
Group2 = ifelse((MostLoss$Nbots <=1), "L","H")
t.test(MostLoss$AVGAPR~Group2, alternative="less", paired=FALSE)
SpringT<-combo[c(1,2,6:9,11:15,17,18,21:26,28:30,34),c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
FallT<-combo[c(3,4,5,10,16,19:20,27,31:33), c("CT1_APR", "CT2_APR", "CT3_APR", "CT4_APR", "CT5_APR", "CT6_APR", "CT7_APR", "CT8_APR")]
SpringB<-combo[c(1,2,6:9,11:15,17,18,21:26,28:30,34),c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
FallB<-combo[c(3,4,5,10,16,19:20,27,31:33), c("CB1_APR", "CB2_APR", "CB3_APR", "CB4_APR", "CB5_APR", "CB6_APR", "CB7_APR", "CB8_APR")]
SpringTmean<-rowMeans(SpringT)
SpringBmean<-rowMeans(SpringB)
FallTmean<-rowMeans(FallT)
FallBmean<-rowMeans(FallB)
t.test(SpringTmean,SpringBmean, paired=TRUE)
t.test(FallTmean,FallBmean, paired=TRUE)
t.test(SpringTmean,FallTmean, paired=FALSE)
t.test(SpringBmean, FallBmean, paired=FALSE)
table(combo$CT1_N)
test<-combo[,c("CT1_N","CT2_N","CT3_N","CT4_N","CT5_N","CT6_N","CT7_N","CT8_N")]
View(test)
apply(test, table)
