fileList <- list.files(path="data/",full.names=T)

run <- 125

runData <- list()

for (file in fileList) {
  runData[[file]] <- (read.csv(file))[run,]
}

save(runData,file="condensedData")
