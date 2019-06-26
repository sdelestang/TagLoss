#'Function to return the probability of recapturing an animal with one tag remaing after being released with two of the same tags.
#'
#'Used by the TagLoss model when animals have been tagged with two of the same tags which are expected to have the same probability of retension.
#'
#'@param pin  A vector of input parameters (length two):
#'     Instant tag retension (proportion)
#'     Chronic tag loss (proportion)
#'@param t A vector of recapture liberties.
#'@return The probability of two tags remaining after time t.
#'@examples PA(c(0.5,0.1),1:10)
#'@export
#'

PA  <-  function(pin,t) 2.0*(pin[1]*exp(-pin[2]*t))*(1-(pin[1]*exp(-pin[2]*t)))*1
