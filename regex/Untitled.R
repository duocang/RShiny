library(data.table)
library(stringr)
library(plyr)
#data <- read.csv2("/Users/song/Downloads/Mappe1.csv", sep = ",")
data <- fread("/Users/song/Downloads/Mappe1.csv")
dataCopy <- data
dataCopy1 <- data
uniqueX <- as.list(unique(data$xV))


# method 1
uniqueXWithNumber <- sapply(uniqueX, function(patent){
    number <- 0
    i <- 1
    while(i != dim(data)[1] ){
        if(patent == data$xV[[i]])
            number <- number + data$fillV[[i]]
        i <- i + 1
    }
    number
})

xx <- paste0(uniqueX," (", uniqueXWithNumber,")")
data$xV <- as.character(data$xV)

for(patent in xx){
    data$xV[data$xV == str_split_fixed(patent," ",2)[1]] <- patent
    data$yV[data$yV == str_split_fixed(patent," ",2)[1]] <- patent
}



# method 2

a <- sapply(uniqueX, function(patent){
    aList <- grep(patent, data$xV, perl = TRUE, value = FALSE)
    sum(dataCopy$fillV[aList])
})


patentNum <- paste0(uniqueX, " (", a, ")")

mapply(function(i, j){
    print(i)
    print(j)
    dataCopy$xV[dataCopy$xV %in% i] <<- j
},uniqueX, patentNum)



# method 3
a <- sapply(uniqueX, function(patent){
    aList <- grep(patent, data$xV, perl = TRUE, value = FALSE)
    s <- sum(dataCopy1$fillV[aList])
    dataCopy1$xV[dataCopy1$xV %in% patent] <<- paste0(patent, " (", s, ")")
})