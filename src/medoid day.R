#set library paths
libPath <-"H:/Hans/R Libraries"
.libPaths(libPath) #set the library path

#set WD
wd <- "G:/QUIST/Production/Nayef-QUIST/2019-13-15_vgh_mdc-capacity-planning-including-OPAT/results/wip"
#wd <- "H:/Hans/2019 corporate requests/Medical Day Care Simulation"
setwd(wd)

#load libraies; open xlsx is required but I'm not sure about reshape 2. I pulled this from old code.
require(openxlsx)
require(reshape2)

#load in data
volData <- read.xlsx("MDC.xlsx",sheet="vols", detectDates = TRUE)
hourData <- read.xlsx("MDC.xlsx",sheet="hours", detectDates = TRUE)

#strip weekends as the MDC isn't open
volData2 <- volData[volData$IsWeekend %in% 2:6 & volData$Total>0,]
hourData2 <- hourData[hourData$IsWeekend %in% 2:6 & hourData$Total>0,]

#remove attributes
volData3  <- volData2[, -which(names(volData2) %in% c("IsWeekend","Date"))] #strip out text and date attributes
hourData3<- hourData2[, -which(names(hourData2) %in% c("IsWeekend","Date"))] #strip out text and date attributes

#assign row names
row.names(volData3)   <- volData2$Date
row.names(hourData3)  <- hourData2$Date

#find pair wise manhattan distances to find medoids
distVol <-dist(volData3, method = "manhattan", diag = FALSE, upper = FALSE)
distVol <-as.matrix(distVol)
distVolTotal <- colSums(distVol)

distHour <-dist(hourData3, method = "manhattan", diag = FALSE, upper = FALSE)
distHour <-as.matrix(distHour)
distHourTotal <- colSums(distHour)

#find which rows have the least total pairwise distance
mIndexVol <- which(distVolTotal == min(distVolTotal))
mIndexHour <- which(distHourTotal == min(distHourTotal))

msg1 <- paste0("The representative dates when looking at volumes are ", names(mIndexVol) )
msg2 <- paste0("The representative dates when looking at hours are ", names(mIndexHour) )

print(msg1)
print(msg2)

#find which dates were the most different
mIndexVol <- which(distVolTotal == max(distVolTotal))
mIndexHour <- which(distHourTotal == max(distHourTotal))

msg3 <- paste0("The most atypical dates when looking at volumes are ", names(mIndexVol) )
msg4 <- paste0("The most atypical dates when looking at hours are ", names(mIndexHour) )

print(msg3)
print(msg4)

#find which dates had the highest volumes and the highest hours
hIndexVol <- which(volData3$Total == max(volData3$Total))
hIndexHour <- which(hourData3$Total == max(hourData3$Total))

msg5 <- paste0("The dates with the highest volumes are ", rownames(volData3)[hIndexVol] )
msg6 <- paste0("The dates with the highest hours are ", rownames(hourData3)[hIndexHour] )

print(msg5)
print(msg6)

#find which dates had the lowest volumes and the highest hours
hIndexVol <- which(volData3$Total == min(volData3$Total))
hIndexHour <- which(hourData3$Total == min(hourData3$Total))

msg7 <- paste0("The dates with the lowest volumes are ", rownames(volData3)[hIndexVol] )
msg8 <- paste0("The dates with the lowest hours are ", rownames(hourData3)[hIndexHour] )

print(msg7)
print(msg8)



