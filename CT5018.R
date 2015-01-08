#'Normalising Function
#'
#'Takes in any numeric value and converts it into a value which is between 0-1
#'Lantz, B. (2013). Machine learning with R. Birmingham, Mumbai: Packt Publishing, pp.79
#'@param x a numeric value to be normalised
#'@return The normalised value of the input
#'@export
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#'FScore Function
#'
#'Takes in multiple values and returns a standard numeric value between 0-1, which can easily rate your data prediction model
#'Lantz, B. (2013). Machine learning with R. Birmingham, Mumbai: Packt Publishing, pp.310
#'@param TP True Positive, correctly predicted positve values
#'@param FP False Positive, correctly predicted negative values
#'@param FN False negative, wrongly predicted negative values
#'@return The fscore value of the inputs
#'@export
FScore <- function(TP, FP, FN) {
  TPS <- 2*TP
  return ((TPS) / ((TPS)+FP+FN))
}

#'Accuracy Function
#'
#'Takes in multiple values and returns the accuracy of your model, a value between 0-1
#'@param TP True Positive, correctly predicted positve values
#'@param FP False Positive, correctly predicted negative values
#'@param TN True negative, correctly predicted negative values
#'@param FN False negative, wrongly predicted negative values
#'@return The accuracy value of the inputs
#'@export
Accuracy <- function(TP, FP, TN, FN) {
  return ((TP+TN)/(TP+TN+FP+FN))
}

#'Error Rate Function
#'
#'Takes in multiple values and returns the error rate of your model, a value between 0-1
#'Lantz, B. (2013). Machine learning with R. Birmingham, Mumbai: Packt Publishing, pp.309
#'@param TP True Positive, correctly predicted positve values
#'@param FP False Positive, correctly predicted negative values
#'@param TN True negative, correctly predicted negative values
#'@param FN False negative, wrongly predicted negative values
#'@return The error rate value of the inputs
#'@export
ErrorRate <- function(TP, FP, TN, FN) {
  return (1-((TP+TN)/(TP+TN+FP+FN)))
}

#'Precision Function
#'
#'Takes in multiple values and returns the precision of your model, a value between 0-1
#'Lantz, B. (2013). Machine learning with R. Birmingham, Mumbai: Packt Publishing, pp.309
#'@param TP True Positive, correctly predicted positve values
#'@param FP False Positive, correctly predicted negative values
#'@return The precision of the inputs
#'@export
Precision <- function(TP, FP) {
  return (TP/(TP+FP))
}

#'Recall Function
#'
#'Takes in multiple values and returns the recall rate of your model, a value between 0-1
#'@param TP True Positive, correctly predicted positve values
#'@param FN False negative, wrongly predicted negative values
#'@return The recall of the inputs
#'@export
Recall <- function(TP, FN) {
  return (TP/(TP+FN))
}