#tbl <- read.csv("CitesforSara(1).csv")
#tbl

#AssasinationsData <- load("AssassinationsData.RData")

#summary(AssassinationsData$npolity2dummy[AssassinationsData$year == 1900])#pct of countries that are democracies in 1900
#summary(AssassinationsData$npolity2dummy[AssassinationsData$year == 2000])
#summary(AssassinationsData$absnpolity2dummy11[AssassinationsData$year== 1945])#pct of countries htat switch between autocracy and democracy
#summary(AssassinationsData$absnpolity2dummy11[AssassinationsData$year== 1980])

#data.a <- AssassinationsData

#keepIX <- data.a$seriousattempt==1

#data.a <- data.a[keepIX,]
#estimates.a <- lm(absnpolity2dummy11 ~success+weapondum2+weapondum3,data=data.a)
#summary(estimates.a)

#unit 3
AJRData <- load.Rdata2(path='C://Users//subar//Downloads//',filename="AJRData.RData")
estimates.b <- lm("avexpr∼logem4",data=AJRData[which(AJRData$baseco=="1"),])

summary(estimates.b)


library(lmtest)
useData <- load.Rdata2(path='C://Users//subar//Downloads//',filename="mitaData.RData")
#useData <- load("C://Users//subar//Downloads//mitaData.RData")
#useData
baseFormula <- "lhhequiv ~ pothuan_mita + elv_sh +slope +infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3"
#formula <- paste(baseFormula, controls.geo)
formula <- as.formula(baseFormula)
x <- useData$lon
#x
x <- x**2
#x <- mean(x)
#x
y <- useData$lat
y3 <- y**3
#y<- mean(y)
y
#xy <- mean(x*y**2)
#xy
#xy <- mean(xy)
#run the regression with different bandwidths
useData$d_bnd
useData %>%
  filter(d_bnd >= 50)
for(bw in c(100,75,50)){
    keepIX <- useData["d_bnd",]<bw
    estimates.b <- lm(baseFormula,data=useData)
    ses.b <- cluster.vcov(estimates.b,useData$district)

    #cat(paste("Regression FOR BW=", bw,"****",sep=""))
    print(coeftest(estimates.b,ses.b))
}

#useData <- load.Rdata2(filename="MitaData(1).RData")
useData %>%
  filter(d_bnd <= 0.5)
baseFormula <- "lhhequiv ~ pothuan_mita + elv_sh +slope +infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3 + dpot"
#for(bw in c(100,75,50)){
#keepIX <- mitaData[keepIX,]
estimates.b <- lm(baseFormula,data=useData)
ses.b <- cluster.vcov(estimates.b,useData$district)

#cat(paste("Regression FOR BW=", bw,"****",sep=""))
print(coeftest(estimates.b,ses.b))