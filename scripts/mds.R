#włączenie bibliotek
library(proxy)

#zmiana katalogu roboczego
workDir <- "C:\\PW\\PJN\\"
setwd(workDir)

#lokalizacja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir,
  "\\",
  "frequency_matrix.R",
  sep = ""
)
source(sourceFile)

#skalowanie wielowymiarowe
distCos <- dist(dtmTfidfBoundsMatrix, method = "cosine")
mds <- cmdscale(distCos,eig=TRUE, k=2)
x <- mds$points[,1]
y <- mds$points[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""),
  rownames(dtmTfidfBoundsMatrix),
  sep = "<-"
)

#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x,
  y,
  #xlim = c(-0.5,-0.2),
  #ylim = c(-0.2,0.1),
  xlab="Współrzędna syntetyczna 1", 
  ylab="Współrzędna syntetyczna 2",
  main="Skalowanie wielowymiarowe", 
  col = "orange"
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = "orange"
)
legend("bottom", legend, cex=.5, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "\\",
  "mds.png",
  sep = ""
)
png(file = plotFile)
plot(
  x,
  y,
  xlab="Współrzędna syntetyczna 1", 
  ylab="Współrzędna syntetyczna 2",
  main="Skalowanie wielowymiarowe", 
  col = "orange"
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = "orange"
)
legend("bottom", legend, cex=.65, text.col = "orange")
dev.off()
