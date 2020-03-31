#wlaczene bibliotek
library(tm)
library(stringr)

#zmiana katalogu roboczego
workDir <- "C:\\PW\\PJN\\"
setwd(workDir) 

#definicja katalogÃ³w projektu
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"

#utworzenie katalogu wyjsciowego
dir.create(outputDir, showWarnings = FALSE)

#utworzenie korpusu dokumentow
corpusDir <- paste(inputDir, "\\", "Literatura - streszczenia - przetworzone", sep = "")
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#usuniecie rozszerzen z nazw dokumentow
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))
  return(document)
}

corpus <- tm_map(corpus, cutExtensions)

#utworzenie macirzy czestosci
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
tdmTfidfAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdmBinAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfidfBoundsMatrix <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBoundsMatrix)

#export macierzy do pliku .csv
#matrixFile <- paste(
#  outputDir,
#  "\\",
#  "tdmTfidfBounds.csv",
# sep = ""
#)

#write.table(tdmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

#skalowanie wielowymiarowe 
d <- dist(dtmTfidfBoundsMatrix)
fit <- cmdscale(d,eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(dtmTfidfBoundsMatrix), cex=.7)