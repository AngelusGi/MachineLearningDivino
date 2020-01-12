DeleteRows <- function(startRow, endRow, dataSet) {
  print("Righe prima ")
  nrow(dataSet)
  dataSet <- dataSet[-c(startRow, endRow),]
  summary(dataSet)
  #norow(dataSet)
  print("Righe dopo")
  nrow(dataSet)
}