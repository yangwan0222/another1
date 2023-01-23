################# Roche data ######################

d <- read.csv("C://Users//wanya//Desktop//Roche//RD002685_RD_TECH_PROD_20220630153118 2022 dataset.csv")
d1 <- d[!is.na(d$VID),]
d1 <- d1[d1$RESULT != "",]

for(i in seq(nrow(d1)))
{
  if(d1[i,14] != "")
  {
    d1[i,10] <- unlist(strsplit(d1[i,14], split = " "))[4]
    d1[i,14] <- "Abeta42>1700"
  }
}

d1 <- d1[,c(2,3,4,8,9,10,14)]

x <- d1[,c(1:4)]
x <- unique(x)

for(i in seq(1029))
{
  x$AB40[i] <- d1$RESULT[(i-1)*4+1]
  x$ABETA[i] <- d1$RESULT[(i-1)*4+2]
  x$PTAU[i] <- d1$RESULT[(i-1)*4+3]
  x$TAU[i] <- d1$RESULT[(i-1)*4+4]
}

x1 <- read.csv("C://Users//wanya//Desktop//unblind//adni_inventory.csv")
x1 <- x1[,c(3:6)]
x$EXAMDATE <- dmy(x$EXAMDATE)
x1$EXAMDATE <- mdy(x1$EXAMDATE)
x1 <- unique(x1)

xx <- merge(x,x1,by = c("RID","EXAMDATE"))

#######################################

d <- read.csv("C://Users//wanya//Desktop//tau_pet//available+.csv")
d <- d[,c(1,2,22)]
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//2021//Decemeber//03//FNIH//Copy of Unblinded Abeta round robin study Mike D with added columns 06_15_2021.xlsx"))
d1$DRAWDTE <- as.character(d1$DRAWDTE)
d1$DRAWDTE <- as.Date(d1$DRAWDTE, format = '%Y%m%d')
names(d1)[5] <- "EXAMDATE"

t <- merge(d1,d,by = c("RID","EXAMDATE"))
t2 <- d1[!d1$GUSPECID%in%t$GUSPECID,]

################# NTK Summary ######################

library(readxl)
d <- data.frame(read_excel("C://Users//wanya//Desktop//NTK plot//AB4240.xlsx"))

t.test(d$RESULT[d$Dx=="HC"])$conf.int
t.test(d$RESULT[d$Dx=="MCI"])$conf.int
t.test(d$RESULT[d$Dx=="FTLD"])$conf.int
t.test(d$RESULT[d$Dx=="PD"])$conf.int
t.test(d$RESULT[d$Dx=="AD"])$conf.int

d1 <- d[d$Dx =="HC" | d$Dx=="MCI",]
wilcox.test(d1$RESULT ~ d1$Dx)
d1 <- d[d$Dx =="HC" | d$Dx=="FTLD",]
wilcox.test(d1$RESULT ~ d1$Dx)
d1 <- d[d$Dx =="HC" | d$Dx=="PD",]
wilcox.test(d1$RESULT ~ d1$Dx)
d1 <- d[d$Dx =="HC" | d$Dx=="AD",]
wilcox.test(d1$RESULT ~ d1$Dx)

d1 <- na.omit(d)
  
mean(d1$RESULT[d1$Dx=="HC"])
sd(d1$RESULT[d1$Dx=="HC"])
sum(d1$Dx=="HC")

mean(d1$RESULT[d1$Dx=="MCI"])
sd(d1$RESULT[d1$Dx=="MCI"])
sum(d1$Dx=="MCI")

mean(d1$RESULT[d1$Dx=="FTLD"])
sd(d1$RESULT[d1$Dx=="FTLD"])
sum(d1$Dx=="FTLD")

mean(d1$RESULT[d1$Dx=="PD"])
sd(d1$RESULT[d1$Dx=="PD"])
sum(d1$Dx=="PD")

mean(d1$RESULT[d1$Dx=="AD"])
sd(d1$RESULT[d1$Dx=="AD"])
sum(d1$Dx=="AD")

################# Omit NAs by columns ######################

data_subset <- data[ , c("x1")]
data_by_column <- data[complete.cases(data_subset), ]
data_by_column


################# Yassine ######################

library(readxl)
library(lubridate)
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//v1.xlsx"))
d2 <- read.csv("C://Users//wanya//Desktop//unblind//adni_inventory.csv")
d2 <- d2[,c(3:7)]
d2 <- unique(d2)
d2 <- d2[d2$VISCODE2=="m12",]
d2 <- na.omit(d2)

d <- merge(d1,d2,by="RID")

################# Report duplicate row appear times ######################

a = c(1, 1, 1, 2, 2, 3, 4, 4)
b = c(3.5, 3.5, 2.5, 2, 2, 1, 2.2, 7)
df <-data.frame(a,b)
library(plyr)
ddply(df,.(a,b),nrow)

#######################################

d <- read.csv("C://Users//wanya//Desktop//Roche//v2.csv")
d1 <- read.csv("C://Users//wanya//Desktop//unblind//adni_inventory.csv")
d1 <- d1[,c(3:7)]
d1 <- unique(d1)

t <- merge(d,d1,by="RID")

#######################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Roche//v3.xlsx"))
d1 <- read.csv("C://Users//wanya//Desktop//Roche//adni_inventory.csv")
d1 <- d1[d1$EVENT== "TAU PET" | d1$EVENT == "Amyloid PET",]
d1 <- d1[,c(3:7)]
d1 <- d1[d1$EXAMDATE != "",]
d1 <- unique(d1)

#######################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Magda//lumi.xlsx"))
par(mfrow=c(1,2))
d3 <- data.frame(x=c(0,3,10,30,60),y = c(512,4952,14625,43235,81482))

p1 <- ggplot(d,aes("",y=d$Concentration..pg.mL.)) + 
  geom_boxplot() + 
  scale_x_discrete( ) +
  labs(title = "Boxplot (n = 130)", x = "pT181P", y = "pg/mL") +
  geom_jitter()
p2 <- ggplot(d, aes(d$Concentration..pg.mL.)) + geom_histogram(aes(y=..density..),color="black", fill="white") + 
  labs(x = "pg/mL", y = "Count", title = "Histogram (n = 130)") +
  geom_density(alpha=.2, fill="#FF6666") +
  annotate("text", x = 4, y = 0.6, label = "Mean: 2.614615", hjust = 0) +
  annotate("text", x = 4, y = 0.55, label = "SD: 0.9967615", hjust = 0) +
  annotate("text", x = 4, y = 0.5, label = "95% CI: [2.441649, 2.787581]", hjust = 0)
p3 <- ggplot(data=d3, aes(x, y)) + geom_line(color="red")+ geom_point() + 
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20) +
  labs(x = "pg/mL", y = "Count", title = "Calibrator analysis")

library(patchwork)
(p1 | p2) / p3

#######################################

d <- na.omit(read.csv("C://Users//wanya//Desktop//Roche//v2.csv"))

d1 <- data.frame(matrix(nrow = 1,ncol = 11))
names(d1) <- c("RID","AB40_original","ABETA_original","PTAU_original","TAU_original","AB4240_original",
               "AB40_latest","ABETA_latest","PTAU_latest","TAU_latest","AB4240_latest")

for(i in seq(unique(d$RID)))
{
  t <- d[d$RID == unique(d$RID)[i],]
  if(nrow(t)==1) next
  
  d1[i,1] <- unique(d$RID)[i]
  
  d1[i,2] <- mean(t$AB40[t$RUNDATE==min(t$RUNDATE)])
  d1[i,3] <- mean(t$ABETA[t$RUNDATE==min(t$RUNDATE)])
  d1[i,4] <- mean(t$PTAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,5] <- mean(t$TAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,6] <- mean(t$AB4240[t$RUNDATE==min(t$RUNDATE)])
  
  d1[i,7] <- mean(t$AB40[t$RUNDATE==max(t$RUNDATE)])
  d1[i,8] <- mean(t$ABETA[t$RUNDATE==max(t$RUNDATE)])
  d1[i,9] <- mean(t$PTAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,10] <- mean(t$TAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,11] <- mean(t$AB4240[t$RUNDATE==max(t$RUNDATE)])
}

model1 <- lm(d1[,7]~d1[,2],data = d1)
model2 <- lm(d1[,8]~d1[,3],data = d1)
model3 <- lm(d1[,9]~d1[,4],data = d1)
model4 <- lm(d1[,10]~d1[,5],data = d1)
model5 <- lm(d1[,11]~d1[,6],data = d1)

p1 <- ggplot(d1,aes(AB40_original, AB40_latest)) + geom_point() + geom_smooth(method='lm') + 
  annotate("text", x = 25000, y = 12500, label = paste("R-squared:",round(summary(model1)$r.squared,4)), hjust = 0)

p2 <- ggplot(d1,aes(ABETA_original, ABETA_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 2000, y = 1000, label = paste("R-squared:",round(summary(model2)$r.squared,4)), hjust = 0)
  
p3 <- ggplot(d1,aes(PTAU_original, PTAU_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 50, y = 20, label = paste("R-squared:",round(summary(model3)$r.squared,4)), hjust = 0)

p4 <- ggplot(d1,aes(TAU_original, TAU_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 500, y = 200, label = paste("R-squared:",round(summary(model4)$r.squared,4)), hjust = 0)

p5 <- ggplot(d1,aes(AB4240_original, AB4240_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 0.1, y = 0.05, label = paste("R-squared:",round(summary(model5)$r.squared,4)), hjust = 0)

(p1 | p2 | p3) / (p4 | p5)

#######################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Roche//New folder//batch3.xlsx"))
d <- d[,-c(10)]
d$AB42 <- as.double(d$ABETA)
d$PTAU <- as.double(d$PTAU)
d$TAU <- as.double(d$TAU)
d$AB4240 <- as.double(d$AB4240)
d <- na.omit(d)

d1 <- data.frame(matrix(nrow = 1,ncol = 13))
names(d1) <- c("RID","AB40_original","AB42_original","PTAU_original","TAU_original","AB4240_original",
               "AB40_latest","AB42_latest","PTAU_latest","TAU_latest","AB4240_latest","original","latest")

for(i in seq(nrow(d)))
{
  t <- d[d$RID == d$RID[i] & d$EXAMDATE == d$EXAMDATE[i],]
  if(nrow(t)==1) next
  
  d1[i,1] <- t$RID[1]
  
  d1[i,2] <- mean(t$AB40[t$RUNDATE==min(t$RUNDATE)])
  d1[i,3] <- mean(t$AB42[t$RUNDATE==min(t$RUNDATE)])
  d1[i,4] <- mean(t$PTAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,5] <- mean(t$TAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,6] <- mean(t$AB4240[t$RUNDATE==min(t$RUNDATE)])
  
  d1[i,7] <- mean(t$AB40[t$RUNDATE==max(t$RUNDATE)])
  d1[i,8] <- mean(t$AB42[t$RUNDATE==max(t$RUNDATE)])
  d1[i,9] <- mean(t$PTAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,10] <- mean(t$TAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,11] <- mean(t$AB4240[t$RUNDATE==max(t$RUNDATE)])
  
  d1[i,12] <- min(t$RUNDATE)
  d1[i,13] <- max(t$RUNDATE)
}
d1 <- na.omit(d1)
d1 <- unique(d1)
#d1$original <- structure(d1$original,class=c('POSIXt','POSIXct'))
#d1$latest <- structure(d1$latest,class=c('POSIXt','POSIXct'))

timeDate <- as.POSIXct("2021-01-01")   # convert date to large number
t <- unclass(timeDate)[[1]]

d1 <- d1[d1$original < t & d1$latest > t,] # only 1st rundate before 2021 and last rundate after 2021 are selected

model1 <- lm(d1[,7]~d1[,2],data = d1)
model2 <- lm(d1[,8]~d1[,3],data = d1)
model3 <- lm(d1[,9]~d1[,4],data = d1)
model4 <- lm(d1[,10]~d1[,5],data = d1)
model5 <- lm(d1[,11]~d1[,6],data = d1)

p1 <- ggplot(d1,aes(AB40_original, AB40_latest)) + geom_point() + geom_smooth(method='lm') + 
  annotate("text", x = 7.5, y = 30, label = "AB40_2021 = 0.94*AB40_original + 0.88", hjust = 0) +
  annotate("text", x = 7.5, y = 28, label = paste("R-squared:",round(summary(model1)$r.squared,4)), hjust = 0)

p2 <- ggplot(d1,aes(AB42_original, AB42_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 400, y = 3000, label = "AB42_2021 = 1.17*AB42_original - 75.59", hjust = 0) +
  annotate("text", x = 400, y = 2750, label = paste("R-squared:",round(summary(model2)$r.squared,4)), hjust = 0)

p3 <- ggplot(d1,aes(PTAU_original, PTAU_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 12.5, y = 70, label = "PTAU_2021 = 0.97*PTAU_original - 0.44", hjust = 0) +
  annotate("text", x = 12.5, y = 65, label = paste("R-squared:",round(summary(model3)$r.squared,4)), hjust = 0)

p4 <- ggplot(d1,aes(TAU_original, TAU_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 125, y = 600, label = "TAU_2021 = 0.99*TAU_original + 0.4", hjust = 0) +
  annotate("text", x = 125, y = 550, label = paste("R-squared:",round(summary(model4)$r.squared,4)), hjust = 0)

p5 <- ggplot(d1,aes(AB4240_original, AB4240_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 0.025, y = 0.13, label = "AB4240_2021 = 1.12*AB4240_original - 0.001", hjust = 0) +
  annotate("text", x = 0.025, y = 0.12, label = paste("R-squared:",round(summary(model5)$r.squared,4)), hjust = 0)

# patchwork: https://patchwork.data-imaginist.com/articles/guides/annotation.html
p <- (p1 | p2 | p3) / (p4 | p5)

p + plot_annotation(title = 'Batch 3 (n = 50)')

#######################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Roche//New folder//batch4.xlsx"))
d <- d[,-c(10)]
d$AB42 <- as.double(d$ABETA)
d$PTAU <- as.double(d$PTAU)
d$TAU <- as.double(d$TAU)
d$AB4240 <- as.double(d$AB4240)
d <- na.omit(d)

d1 <- data.frame(matrix(nrow = 1,ncol = 13))
names(d1) <- c("RID","AB40_original","AB42_original","PTAU_original","TAU_original","AB4240_original",
               "AB40_latest","AB42_latest","PTAU_latest","TAU_latest","AB4240_latest","original","latest")

for(i in seq(nrow(d)))
{
  t <- d[d$RID == d$RID[i] & d$EXAMDATE == d$EXAMDATE[i],]
  if(nrow(t)==1) next
  
  d1[i,1] <- t$RID[1]
  
  d1[i,2] <- mean(t$AB40[t$RUNDATE==min(t$RUNDATE)])
  d1[i,3] <- mean(t$AB42[t$RUNDATE==min(t$RUNDATE)])
  d1[i,4] <- mean(t$PTAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,5] <- mean(t$TAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,6] <- mean(t$AB4240[t$RUNDATE==min(t$RUNDATE)])
  
  d1[i,7] <- mean(t$AB40[t$RUNDATE==max(t$RUNDATE)])
  d1[i,8] <- mean(t$AB42[t$RUNDATE==max(t$RUNDATE)])
  d1[i,9] <- mean(t$PTAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,10] <- mean(t$TAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,11] <- mean(t$AB4240[t$RUNDATE==max(t$RUNDATE)])
  
  d1[i,12] <- min(t$RUNDATE)
  d1[i,13] <- max(t$RUNDATE)
}
d1 <- na.omit(d1)
d1 <- unique(d1)
#d1$original <- structure(d1$original,class=c('POSIXt','POSIXct'))
#d1$latest <- structure(d1$latest,class=c('POSIXt','POSIXct'))

timeDate1 <- as.POSIXct("2021-01-01")   # or use as.numeric(timeDate1) to conver to big number
t1 <- unclass(timeDate1)[[1]]

timeDate2 <- as.POSIXct("2022-01-01")   
t2 <- unclass(timeDate2)[[1]]

d1 <- d1[d1$original < t1 & d1$latest > t2,]

model1 <- lm(d1[,7]~d1[,2],data = d1)
model2 <- lm(d1[,8]~d1[,3],data = d1)
model3 <- lm(d1[,9]~d1[,4],data = d1)
model4 <- lm(d1[,10]~d1[,5],data = d1)
model5 <- lm(d1[,11]~d1[,6],data = d1)

p1 <- ggplot(d1,aes(AB40_original, AB40_latest)) + geom_point() + geom_smooth(method='lm') + 
  annotate("text", x = 12.5, y = 32, label = "AB40_2022 = 1.01*AB40_original - 0.61", hjust = 0) +
  annotate("text", x = 12.5, y = 30, label = paste("R-squared:",round(summary(model1)$r.squared,4)), hjust = 0)

p2 <- ggplot(d1,aes(AB42_original, AB42_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 500, y = 3500, label = "AB42_2022 = 1.01*AB42_original + 35.18", hjust = 0) +
  annotate("text", x = 500, y = 3250, label = paste("R-squared:",round(summary(model2)$r.squared,4)), hjust = 0)

p3 <- ggplot(d1,aes(PTAU_original, PTAU_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 10, y = 80, label = "PTAU_2022 = 1*PTAU_original - 1.92", hjust = 0) +
  annotate("text", x = 10, y = 75, label = paste("R-squared:",round(summary(model3)$r.squared,4)), hjust = 0)

p4 <- ggplot(d1,aes(TAU_original, TAU_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 150, y = 600, label = "TAU_2022 = 0.98*TAU_original - 4.49", hjust = 0) +
  annotate("text", x = 150, y = 550, label = paste("R-squared:",round(summary(model4)$r.squared,4)), hjust = 0)

p5 <- ggplot(d1,aes(AB4240_original, AB4240_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 0.02, y = 0.12, label = "AB4240_2022 = 1.07*AB4240_original - 0.0001", hjust = 0) +
  annotate("text", x = 0.02, y = 0.11, label = paste("R-squared:",round(summary(model5)$r.squared,4)), hjust = 0)

# patchwork: https://patchwork.data-imaginist.com/articles/guides/annotation.html
p <- (p1 | p2 | p3) / (p4 | p5)

p + plot_annotation(title = 'Batch 4 (n = 23)')

#######################################

d1 <- read.csv("C://Users//wanya//Desktop//emory unblind//EMORY_PEPTIDERATIOS.csv")
d2 <- read.csv("C://Users//wanya//Desktop//emory unblind//ADNI_PeptideRatios_Emory.csv")
d1$Replicate <- paste(d1$REPLICATE1,"_",d1$REPLICATE2,"_",d1$LINK_ID,sep = "")
names(d1)[6] <- "Peptide"
names(d1)[7] <- "Protein"

d <- merge(d1,d2,by = c("Peptide","Protein","Replicate"))

dd <- merge(d1,d2,by = c("Peptide","Protein","Replicate"), all.x = TRUE)

#######################################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//pla csf GSID&PID1//csf_clinic.xlsx"))
d1$Clinic[d1$Clinic == "D068" | d1$Clinic == "D168"] <- "D389"
d1 <- na.omit(unique(d1))
d1 <- d1[d1$Primary == "CSF               Cerebro-Spinal Fluid",]
d1 <- d1[,-c(3)]
d2 <- read.csv("C://Users//wanya//Desktop//emory unblind//Unblinded.csv")

d <- merge(d2,d1,by = "RID")
d$ADNI_IDs <- paste(d$Clinic,"_S_",d$RID,sep = "")
d$ADNI_IDs <- substr(d$ADNI_IDs,2,nchar(d$ADNI_IDs))

d <- d[order(d$X), ]

#######################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//ATRI//csf.xlsx"))
d <- na.omit(d)
d <- unique(d)
d$PID.ID1 <- as.integer(d$PID.ID1)

d <- d[d$PID.ID1>=6000,]

################## Diadem #####################

d <- read.csv("C://Users//wanya//Desktop//diadem_2nd//DXSUM_PDXCONV_ADNIALL.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
library(Epi) 
d$DX      = Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))


d1 <- data.frame(read_excel("C://Users//wanya//Desktop//diadem//DIADEM.xls"))

d <- d[,c(2:4,9)]
d <- d[!d$RID %in% d1$Subject,]
d <- d[d$VISCODE == "bl",]

d_nl <- d[d$DX == "NL",]
d_mci <- d[d$DX == "MCI",]
d_ad <- d[d$DX == "AD",]

library(lubridate)

d2 <- data.frame(read_excel("C://Users//wanya//Desktop//diadem_2nd//global.xlsx"))
d2 <- na.omit(d2)
d2$Global.Specimen.ID <- substr(d2$Global.Specimen.ID,1,nchar(d2$Global.Specimen.ID)-3)
d2 <- unique(d2)
d2$Specimen.Date <- ymd(as.character(d2$Specimen.Date))
names(d2) <- c("RID","EXAMDATE","Global.Specimen.ID")
d2$EXAMDATE <- as.character(d2$EXAMDATE)

d_nl1 <- merge(d_nl,d2,by = c("RID","EXAMDATE"))
d_mci1 <- merge(d_mci,d2,by = c("RID","EXAMDATE"))
d_ad1 <- merge(d_ad,d2,by = c("RID","EXAMDATE"))

d_nl1$Global.Specimen.ID <- paste(d_nl1$Global.Specimen.ID,"-01",sep = "")
d_mci1$Global.Specimen.ID <- paste(d_mci1$Global.Specimen.ID,"-01",sep = "")
d_ad1$Global.Specimen.ID <- paste(d_ad1$Global.Specimen.ID,"-01",sep = "")

d3 <- read.csv("C://Users//wanya//Desktop//diadem_2nd//UCBERKELEYAV45_04_26_22.csv")[, c("RID","VISCODE","SUMMARYSUVR_WHOLECEREBNORM","EXAMDATE")]
names(d3) = c("RID","VISCODE","COMP.AV45","EXAMDATE")
d3 <- d3[d3$VISCODE=="bl",]


d_nl1 <- d_nl1[!duplicated(d_nl1$RID),]
d_mci1 <- d_mci1[!duplicated(d_mci1$RID),]
d_ad1 <- d_ad1[!duplicated(d_ad1$RID),]

#######################################

d <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//gender.xlsx"))

d <- na.omit(d)

f1 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//New folder//4th.xlsx"))
names(f1)[1] <- "Global.Specimen.ID"
t1 <- merge(f1,d,by="Global.Specimen.ID")

#######################################

d <- read.csv("C://Users//wanya//Desktop//diadem//DXSUM_PDXCONV_ADNIALL.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
library(Epi) 
d$DX      = Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d <- d[,c(2,4,9)]
d <- unique(d)

t <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//match.xlsx"))
t <- na.omit(t)

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//m1.xlsx"))
t1 <- merge(d1,t,by="GID")
t1$EXAMDATE <- as.character(ymd(as.character(t1$EXAMDATE)))
g1 <- merge(t1,d,by=c("RID","EXAMDATE"),all.x = TRUE)
write.csv(g1,"C://Users//wanya//Desktop//fischer_shipment//4 manifest//g1.csv")

d2 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//m2.xlsx"))
t2 <- merge(d2,t,by="GID")
t2$EXAMDATE <- as.character(ymd(as.character(t2$EXAMDATE)))
g2 <- merge(t2,d,by=c("RID","EXAMDATE"),all.x = TRUE)
write.csv(g2,"C://Users//wanya//Desktop//fischer_shipment//4 manifest//g2.csv")

d3 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//m3.xlsx"))
t3 <- merge(d3,t,by="GID")
t3$EXAMDATE <- as.character(ymd(as.character(t3$EXAMDATE)))
g3 <- merge(t3,d,by=c("RID","EXAMDATE"),all.x = TRUE)
write.csv(g3,"C://Users//wanya//Desktop//fischer_shipment//4 manifest//g3.csv")

d4 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//m4.xlsx"))
t4 <- merge(d4,t,by="GID")
t4$EXAMDATE <- as.character(ymd(as.character(t4$EXAMDATE)))
g4 <- merge(t4,d,by=c("RID","EXAMDATE"),all.x = TRUE)
write.csv(g4,"C://Users//wanya//Desktop//fischer_shipment//4 manifest//g4.csv")

################## Convert date from "2017-02-15" to "20170215" #####################

start_numeric <- as.Date('20170215', format = '%Y%m%d')
start_numeric
format(start_numeric, "%Y%m%d")

# check for normality and Wilcoxcon test #
# https://statsandr.com/blog/wilcoxon-test-in-r-how-to-compare-2-groups-under-the-non-normality-assumption/#independent-samples-1
shapiro.test(subset(dat, Sex == "Girl")$Grade)
shapiro.test(subset(dat, Sex == "Boy")$Grade)

################## Convert LDMS storage output #####################

d <- data.frame(read_excel("C://Users//wanya//Desktop//diadem_2nd//summary//StorageSearchResults_all.xls"))

d1 <- d[,8][!is.na(d[,8])]
d2 <- d1[! d1 %in% "Global \nSpecimenID"]
d3 <- data.frame(x = "MCI_Negative",y=d2[1:37])
d3$y <- substr(d3$y,1,nchar(d3$y)-1)
write.csv(d3,"C://Users//wanya//Desktop//diadem_2nd//summary//mci_pos.csv")

################## QC pools 1 #####################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//QC pool//Sweden Internal Storage Manifest.xlsx"))
d1 <- na.omit(d1[,c(1,2)])
d1$Draw.Date <- as.Date(d1$Draw.Date)

d2 <- read.csv("C://Users//wanya//Desktop//QC pool//UCBERKELEYAV45_04_26_22.csv")
d2 <- na.omit(d2[,c(1,4,17,18)])
d2$EXAMDATE <- as.Date(d2$EXAMDATE)

d3 <- d1
d3$diff <- NA
d3$SUVR <- NA
d3$SUVR_CUT <- NA

for(i in seq(nrow(d3)))
{
  t <- d2[d2$RID==d3$RID[i],]
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(d3[i,2]-t[,2])
  if(min(t$diff)<=20)
  {
    x <- t[t$diff == min(t$diff),]
    d3$diff[i] <- x$diff
    d3$SUVR[i] <- x$SUMMARYSUVR_WHOLECEREBNORM
    d3$SUVR_CUT[i] <- x$SUMMARYSUVR_WHOLECEREBNORM_1.11CUTOFF
  }
  else
  {
    next
  }
}

d3 <- na.omit(d3)
d3$Draw.Date <- format(d3$Draw.Date,"%Y%m%d")

d <- data.frame(read_excel("C://Users//wanya//Desktop//QC pool//comment.xlsx"))
d$Specimen.Date <- as.character(d$Specimen.Date)

d4 <- d3
d4$aliquot_left <- NA

for(i in seq(nrow(d3)))
{
  t <- d[d$PID.ID1 == d4$RID[i] & d$Specimen.Date == d4$Draw.Date[i],]
  d4$aliquot_left[i] <- nrow(t) - sum(grepl("sent|pool|pTau", t$Specimen.Comments,ignore.case=TRUE))
}

d4 <- d4[d4$aliquot_left >=11,]

################## QC pools 2 #####################

dx <- unique(d2[,c(1,4)])
dx1 <- dx[duplicated(dx$RID),]

for(i in seq(nrow(dx)))
{
  if(dx$RID[i]%in%dx1$RID)
  {
    dx$SUMMARYSUVR_WHOLECEREBNORM_1.11CUTOFF[i] <- NA
  }
}
dx <- na.omit(dx)
d1$Draw.Date <- NA
d1 <- unique(d1)
dx2 <- merge(d1,dx,all.x = TRUE)




################## FNIH BC #####################
library(lubridate)


d <- data.frame(read_excel("C://Users//wanya//Desktop//FNIH BC//global.xlsx"))
names(d) <- c("RID","GUSPECID","EXAMDATE")
a <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")
a$EXAMDATE <- mdy(a$EXAMDATE)
a <- a[,c(1:6)]

d1 <- read.csv("C://Users//wanya//Desktop//FNIH BC//FNIH Biomarkers Consortium Plasma Abeta Project_ADx VUmc plasma pTau-181.csv")
d1 <- merge(d1,d,by=c("RID","GUSPECID"))
d1$EXAMDATE <- ymd(d1$EXAMDATE)

t <- merge(d1,a,by = c("RID","EXAMDATE"))
t <- unique(t)

for(i in seq(nrow(t)))
{
  x <- t[t$RID == t$RID[i],]
  if(nrow(x) == 1)
  {
    next
  }
  else if (sum(duplicated(x$EXAMDATE))==0)
  {
    next
  }
  else
  {
    y <- x[x$EXAMDATE == x$EXAMDATE[duplicated(x$EXAMDATE)],]
    if(y$RID==467)
    {
      y <- y[y$VISCODE == "init",]
    }
    else
    {
      y <- y[y$VISCODE == "init"|y$VISCODE == "y1",]
    }
    t$VISCODE[t$RID == y$RID & t$EXAMDATE == y$EXAMDATE] <- y$VISCODE
    t$VISCODE2[t$RID == y$RID & t$EXAMDATE == y$EXAMDATE] <- y$VISCODE2
  }
}
t <- unique(t)
t <- t[,c(-9)]

################## Biofluids received #####################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//Biofluids received//pla1.xlsx"))
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//Biofluids received//pla2.xlsx"))

d1$Global.Specimen.ID <- substr(d1$Global.Specimen.ID,1,nchar(d1$Global.Specimen.ID)-3)
d2$Global.Specimen.ID <- substr(d2$Global.Specimen.ID,1,nchar(d2$Global.Specimen.ID)-3)

d1 <- unique(d1)
d2 <- unique(d2)

d1[!d1$Global.Specimen.ID%in%d2$Global.Specimen.ID,]

d <- data.frame(read_excel("C://Users//wanya//Desktop//Biofluids received//csf_all.xlsx"))
d <- unique(d)

d <- data.frame(read_excel("C://Users//wanya//Desktop//Biofluids received//newslice//csf.xlsx"))

################## Update R version #####################

library(installr)
updateR()

################## User defined function #####################

a <- function(t=5)
{
  for(i in 1:t)
  {
    print(i^2)
  }
}

################## Apply function #####################

d <- matrix(c(1:10, 11:20, 21:30), nrow = 10, ncol = 3)

apply(d,2,length)

apply(d,1,function(x) x[1]^2+x[2])

st.err <- function(x){
  sd(x)/sqrt(length(x))
}
apply(d, 2, st.err)

# https://ademos.people.uic.edu/Chapter4.html

vec <- c(1:10)
A<-c(1:9)
B<-c(1:12)
C<-c(1:15)
lst<-list(A,B,C)
vapply(lst, function(x) x[1:2], numeric(2))

d <- function(x)
{
  print(c(min(x),max(x),mean(x)))
}
t <- vapply(lst, d, numeric(3))

## tapply using condition
my.matrx <- matrix(c(1:10, 11:20, 21:30), nrow = 10, ncol = 3)
tdata <- as.data.frame(cbind(c(1,1,1,1,1,2,2,2,2,2), my.matrx))
tapply(tdata$V2, tdata$V1, mean)

summary <- tapply(tdata$V2, tdata$V1, function(x) c(mean(x), sd(x)))

# lapply: result is list
# sapply: result is vector
# vapply: result type is specified
# tapply: https://r-coder.com/tapply-r/ (use secound argument as index (factor usually) to apply function)
# mapply: https://www.statology.org/r-mapply/ https://www.educative.io/answers/what-is-the-mapply-function-in-r 

################## Add standard #####################
library(readxl)
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")
d <- d[,c(1:8)]
d <- d[d$EXAMDATE != "", ]
d <- d[d$EVENT=="Blood" & d$TABLE == "biomark",]
d$EXAMDATE <- mdy(d$EXAMDATE)

d1 <- read.csv("C://Users//wanya//Desktop//add standard//BATEMANLAB_ADNI_Plasma_Abeta4240_20221118.csv")
d1$order <- 1:nrow(d1)
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//add standard//global.xlsx"))
d2$Global.Specimen.ID <- substr(d2$Global.Specimen.ID,1,nchar(d2$Global.Specimen.ID)-3)
d2 <- unique(d2)
names(d2) <- c("RID","Sample_ID","EXAMDATE")

d3 <- merge(d1,d2,by = "Sample_ID",all.x = TRUE)
d3$EXAMDATE <- ymd(d3$EXAMDATE)
d3$VISCODE <- NA
d3$VISCODE2 <- NA
d3$EXAMDATE2 <- NA
d3$PROT <- NA

for(i in seq(nrow(d3)))
{
  if (is.na(d3[i,27]))
  {
    next
  }
  t <- d[d$RID == d3[i,27],]
  t$td <- abs(t$EXAMDATE - d3[i,28])
  t <- t[t$td <= 60,]
  if(nrow(t)==0)
  {
    next
  }
  if(nrow(t)==2)
  {
    t <- t[t$VISCODE=="init",]
  }
  if(nrow(t)>2)
  {
    print(i)
  }
  if(nrow(t)==1)
  {
    d3$VISCODE[i] <- t$VISCODE
    d3$VISCODE2[i] <- t$VISCODE2
    d3$EXAMDATE2[i] <- t$EXAMDATE
    d3$PROT[i] <- t$ORIGPROT
  }
}

d3 <- d3[order(d3$order), ]

################## Fischer verification #####################

library(Epi)
library(readxl)
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//fischer_shipment//4 manifest//DXSUM_PDXCONV_ADNIALL.csv")[,c("Phase","RID",
              "VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
d$DX      = Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d <- d[,c(2,4,9)]
d$EXAMDATE <- ymd(d$EXAMDATE)
d$DX <- as.character(d$DX)

d2 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//global.xlsx"))
names(d2) <- c("RID","ADNI_ID","EXAMDATE","gender")

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//New folder//batches_1.xlsx"))

d1 <- read.csv("C://Users//wanya//Desktop//fischer_shipment//4 manifest//New folder//batches.csv")
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000960"] <- "FA806S8C-03"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000961"] <- "FA806SJS-09"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000962"] <- "HA806TVG-11"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000963"] <- "FA806V2D-05"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000964"] <- "CA8074LF-02"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000965"] <- "CA8074KY-04"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000966"] <- "KA8074ZS-04"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000967"] <- "JA809PS3-03"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000968"] <- "GA809VMP-04"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000969"] <- "CA809WBK-04"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1020"] <- "CA806RMR-03"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1021"] <- "AA806TDH-03"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1022"] <- "HA807HZL-03"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1023"] <- "KA807J58-03"
d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1024"] <- "GA807NFP-03"

d3 <- merge(d1,d2,by="ADNI_ID")

d3$EXAMDATE <- ymd(d3$EXAMDATE)

d3$DX <- NA


for(i in 1:862)
{
  t <- d[d$RID == d3$RID[i],]
  t <- na.omit(t)
  if(nrow(t)==0)
  {
    print(i)
    next
  }
  t$diff <- t$EXAMDATE - d3$EXAMDATE[i]
  t$diff <- abs(t$diff)
  t <- unique(t)
  d3$DX[i] <- t$DX[t$diff==min(t$diff)]
  if(min(t$diff)>180)
  {
    print(i)
  }
}

for(i in 1:9) # batch 1:9, batch 10 don't have shipment 3
{
  for(j in 1:4)
  {
    if(j == 3)
    {
      print(c("MCI") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 1])
      print(c("MCI") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 2])
    }
    else
    {
      print(c("MCI","AD","NL") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 1])
      print(c("MCI","AD","NL") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 2]) 
    }
  }
}



################## Emory Plasma #####################

library(readxl)
library(lubridate)

d <- data.frame(read_excel("C://Users//wanya//Desktop//emory//emory - shipping manifest.xls"))
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//emory//global.xlsx"))

d2 <- merge(d,d1,by="Global.Specimen.ID")
d2 <- d2[,-c(3:5)]
names(d2)[1] <- "CSF_GID"
d2$Specimen.Date <- ymd(d2$Specimen.Date)

d3 <- data.frame(read_excel("C://Users//wanya//Desktop//emory//global_pla.xlsx"))
d3$Global.Specimen.ID <- substr(d3$Global.Specimen.ID,1,nchar(d3$Global.Specimen.ID)-3)
d3 <- unique(d3)
d3$Specimen.Date <- ymd(d3$Specimen.Date)

d4 <- merge(d2,d3,by = c("PID.ID1","Specimen.Date"),all.x = TRUE)

#bl <- read.csv("C://Users//wanya//Desktop//Roche//adni_inventory.csv")
#bl <- bl[,c(1,3:8)]
#bl <- bl[bl$EVENT == "Blood" & bl$TABLE == "biomark",]
#bl <- bl[,-c(3)]
#bl <- bl[bl$VISCODE2 == "bl",]

for(i in 1:833)
{
  if(!is.na(d4$Global.Specimen.ID[i]))
  {
    next
  }
  t <- na.omit(d3[d3$PID.ID1 == d4$PID.ID1[i],])
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(d4$Specimen.Date[i] - t$Specimen.Date)
  if(min(t$diff)>180)
  {
    next
  }
  d4$Global.Specimen.ID[i] <- t$Global.Specimen.ID[t$diff == min(t$diff)]
}
d4 <- na.omit(d4)

t06 <- read.csv("C://Users//wanya//Desktop//emory//New folder//06.csv")
t06 <- head(t06, -1)
t06$batch <- substr(t06$Batch.No.,1,nchar(t06$Batch.No.)-3)
t06$no <- substr(t06$Batch.No.,nchar(t06$Batch.No.)-2,nchar(t06$Batch.No.))
t06 <- t06[,-c(1:20)]
names(t06)[2] <- "06"

f <- function(x,y)
{
  t <- read.csv(paste("C://Users//wanya//Desktop//emory//New folder//",x,sep = ""))
  t <- head(t, -1)
  t$batch <- substr(t$Batch.No.,1,nchar(t$Batch.No.)-3)
  t$no <- substr(t$Batch.No.,nchar(t$Batch.No.)-2,nchar(t$Batch.No.))
  t <- t[,-c(1:20)]
  names(t)[2] <- y
  merge(t06,t,by = "batch", all = TRUE)
}

t06 <- f("01.csv","01")
t06 <- f("02.csv","02")
t06 <- f("03.csv","03")
#t06 <- f("04.csv","04")
t06 <- f("05.csv","05")
t06 <- f("07.csv","07")
t06 <- f("08.csv","08")
t06 <- f("09.csv","09")
t06 <- f("10.csv","10")
t06 <- f("11.csv","11")
t06 <- f("12.csv","12")
t06 <- f("13.csv","13")
t06 <- f("14.csv","14")
t06 <- f("15.csv","15")
t06 <- f("16.csv","16")

t06$Batch <- NA
for(i in seq(nrow(t06)))
{
  t06$Batch[i] <- paste(t06$batch[i],na.omit(t(t06[i,2:16]))[1],sep = "")
}

x <- data.frame(read_excel("C://Users//wanya//Desktop//emory//New folder//StorageSearchResults.xls"))

x1 <- x[,8][!is.na(x[,8])]
x2 <- x1[! x1 %in% "Global \nSpecimenID"]

################## Plot ROC #####################
# https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5470053/

library(readxl)
library(plotROC)
library(OptimalCutpoints)

# for ptau, the more the worse
# for abeta, the less the worse

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//ROC//abeta42.xlsx"))
d2 <- data.frame(read_excel("C://Users//wanya//Desktop//ROC//abeta40.xlsx"))

d3 <- cbind(d1,d2)
d3 <- d3[,-c(3)]

t <- data.frame(read_excel("C://Users//wanya//Desktop//2021//Decemeber//03//FNIH//Copy of Unblinded Abeta round robin study Mike D with added columns 06_15_2021.xlsx"))
t <- t[,c(1,9)]

d3$GUSPECID <- substr(d3$GUSPECID,1,nchar(d3$GUSPECID)-3)
t$GUSPECID <- substr(t$GUSPECID,1,nchar(t$GUSPECID)-3)
dt <- merge(t,d3,by = "GUSPECID")
dt$FBP[dt$FBP<1.11] <- 1
dt$FBP[dt$FBP>=1.11] <- 0

dt$AB4240 <- dt$AB42/dt$AB40
longtest <- melt_roc(dt, "FBP", c("AB42","AB40","AB4240"))

p <- ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc() +
  ggtitle(gsub("  ", 130, "Abeta (n=  )")) + 
  geom_abline(slope=1, intercept = 0, linetype = "dashed") + scale_color_manual(values=c("#CC6666","#9999CC","#1d91c0")) + 
  scale_x_continuous("1 - Specificity",breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity",breaks = seq(0, 1, by = .1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

results1 <- optimal.cutpoints(X = "AB42", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)
results2 <- optimal.cutpoints(X = "AB40", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)
results3 <- optimal.cutpoints(X = "AB4240", status = "FBP",tag.healthy = 0, method = "Youden", data = dt)

p+geom_point(aes(x=1-data.frame(summary(results1)[5])[3,1], y=data.frame(summary(results1)[5])[2,1]),shape = 18,size = 3,color="brown") + # manually change
  geom_point(aes(x=1-data.frame(summary(results2)[5])[3,4], y=data.frame(summary(results2)[5])[2,4]),shape = 18,size = 3,color="green") +
  geom_point(aes(x=1-data.frame(summary(results3)[5])[3,1], y=data.frame(summary(results3)[5])[2,1]),shape = 18,size = 3,color="red") + # manually change
  geom_label(label=paste("95%CI AB42: ",data.frame(summary(results1)[5])[1,2],
                         "\n95%CI AB40: ",data.frame(summary(results2)[5])[1,5],
                         "\n95%CI AB4240: ",data.frame(summary(results3)[5])[1,4]),
             x=0.7,
             y=0.15,
             label.padding = unit(0.5, "lines"), # Rectangle size around 
             label.size = 0.3,
             color = "black",
             fill="white",
             hjust = 0
  )

################## Change aliquot number #####################

library(readxl)

d <- data.frame(read_excel("C://Users//wanya//Desktop//emory//New folder//StorageSearchResults.xls"))
d$gid <- substr(d$gid,1,nchar(d$gid)-3)

for (i in seq(nrow(d)))
{
  d$gid[i] <- paste(d$gid[i],'-',toString(sprintf("%02d",d$aliquot)[i]),sep = "")
}

d1 <- read.csv("C://Users//wanya//Desktop//emory//New folder//can_find.csv")
d1 <- head(d1,-1)








t1 <- data.frame(read_excel("C://Users//wanya//Desktop//emory//New folder//manifest.xls"))
t2 <- read.csv("C://Users//wanya//Desktop//emory//New folder//can_find.csv")
t1$id  <- 1:nrow(t1)
t <- merge(t1,t2, by = "Global.Spec.ID",all.x = TRUE)
t <- t[order(t$id), ]

################## Yassine CSF & Plasma #####################

library(readxl)
d1 <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//sample_list.xlsx"))

dx <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")
dx <- dx[,c(3:8)]
dx <- dx[dx$VISCODE2=="bl",]
dx <- dx[dx$EVENT=="CSF",]

d1$mark <- d1$RID%in%dx$RID

######
library(readxl)
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")

d <- d[,c(3:8)]
d <- unique(d[d$VISCODE2=="bl" & d$EVENT=="CSF",])

d1 <- read.csv("C://Users//wanya//Desktop//Yassine//Sample lists.csv")
d2 <- merge(d1,d,by="RID",all.x = TRUE)
d2 <- d2[!is.na(d2$VISCODE2),]
d2$EXAMDATE <- mdy(d2$EXAMDATE)

d3 <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//pla_date.xlsx"))
d3 <- unique(d3)
d3$Specimen.Date <- ymd(as.character(d3$Specimen.Date))

d4 <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//csf_date.xlsx"))
d4 <- unique(d4)
d4$Specimen.Date <- ymd(as.character(d4$Specimen.Date))

for(i in seq(nrow(d2)))
{
  t<-d4[d4$PID.ID1==d2$RID[i],]
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(t$Specimen.Date-d2$EXAMDATE[i])
  d2$csf_date[i] <- as.character(t$Specimen.Date[t$diff==min(t$diff)])
  if(min(t$diff)>=90)
  {
    print(i)
  }
}
d2$csf_date <- ymd(d2$csf_date)
for(i in seq(nrow(d2)))
{
  t<-na.omit(d3[d3$PID.ID1==d2$RID[i],])
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(t$Specimen.Date-d2$csf_date[i])
  if(min(t$diff)>=90)
  {
    print(i)
  }
  d2$pla_date[i] <- as.character(t$Specimen.Date[t$diff==min(t$diff)])
}
d2$pla_date <- ymd(d2$pla_date)

all_csf <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//all_csf.xlsx"))
all_pla <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//all_pla.xlsx"))
names(all_csf) <- c("RID","csf_date","GID_csf")
names(all_pla) <- c("RID","pla_date","GID_pla")
all_csf$GID_csf <- substr(all_csf$GID_csf,1,nchar(all_csf$GID_csf)-3)
all_pla$GID_pla <- substr(all_pla$GID_pla,1,nchar(all_pla$GID_pla)-3)
all_csf <- unique(all_csf)
all_pla <- unique(all_pla)
all_csf$csf_date <- ymd(as.character(all_csf$csf_date))
all_pla$pla_date <- ymd(as.character(all_pla$pla_date))



t <- subset(d2, d2$Type=="LMCI" & d2$Change_type=="stable" & d2$APOE4_bl==0)
t1 <- subset(d2, d2$Type=="LMCI" & d2$Change_type=="stable" & d2$APOE4_bl==2)

t <- merge(t,all_csf,by=c("RID","csf_date"))
t <- merge(t,all_pla,by=c("RID","pla_date"))

t <- data.frame(read_excel("C://Users//wanya//Desktop//Yassine//results//temp.xlsx"))[,-c(2)]
t <- merge(t,d2,by = c("RID","Change_type"))

t <- read.csv("C://Users//wanya//Desktop//Yassine//results//LMCI_stable_apoe4-.csv")
t$csf_date <- mdy(t$csf_date)
t$pla_date <- mdy(t$pla_date)

######################## Autopsy ########################

library(readxl)

d <- read.csv("C://Users//wanya//Desktop//Autopsy//NEUROPATH_11_14_22.csv")

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//Autopsy//csf_date_comment.xlsx"))

d <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")
d <- d[,c(3:8)]
d <- d[d$VISCODE2=="bl",]
d <- d[d$EVENT=="CSF",]

d <- data.frame(read_excel("C://Users//wanya//Desktop//Autopsy//results.xlsx"))
d$Dead <- paste(d$NPFORMYR,"-",d$NPFORMMO,"-",d$NPFORMDY,sep = "")

########################

d <- data.frame(read_excel("C://Users//wanya//Desktop//temp//New Microsoft Excel Worksheet.xlsx"))
d <- unique(d)
d$PID.ID1 <- as.double(d$PID.ID1)
d <- na.omit(d)
d <- d[d$PID.ID1 == round(d$PID.ID1),]
write.csv(d,"C://Users//wanya//Desktop//temp//result.csv")

########################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Fuji unblind//FUJIREBIOABETAPLASMA.xlsx"))
names(d)[1] <- "Global.Specimen.ID"

s <- data.frame(read_excel("C://Users//wanya//Desktop//Fuji unblind//New Microsoft Excel Worksheet.xlsx"))
t1 <- merge(d,s,by = "Global.Specimen.ID")
t1$Specimen.Date <- ymd(t1$Specimen.Date)

x <- read.csv("C://Users//wanya//Desktop//FNIH BC//adni_inventory.csv")
x <- x[x$EVENT=="Blood" & x$TABLE == "biomark",]
x <- x[,c(3:6)]
x$EXAMDATE <- mdy(x$EXAMDATE)

for(i in 1:515)
{
  t <- x[x$RID == t1$PID.ID1[i],]
  t$diff <- abs(t$EXAMDATE - t1$Specimen.Date[i])
  t <- t[t$diff==min(t$diff),]
  if("init" %in% t$VISCODE)
  {
    t1$VISCODE[i] <- "init"
    t1$VISCODE2[i] <- t$VISCODE2[1]
    t1$EXAMDATE[i] <- as.character(t$EXAMDATE[1])
  }
  else
  {
    t1$VISCODE[i] <- t$VISCODE
    t1$VISCODE2[i] <- t$VISCODE2
    t1$EXAMDATE[i] <- as.character(t$EXAMDATE)
  }
  
  if(t$diff[1]>60)
  {
    print(i)
  }
  
}

# https://gitforwindows.org/

######################## Aliquot list ########################



