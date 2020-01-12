MeanSD <- function(x) {
  result <- c(mean(x), sd(x))
  names(result) <- c("Media", "StandardDerivation")
  result
}