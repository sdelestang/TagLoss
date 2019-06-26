#'Function used by the TagLos model.
#'
#'Used by the TagLoss model when animals have been tagged with two tags which are expected to have diffeent probabilities of retension.
#'
#'@param pin  A vector of input parameters (length two):
#'     Instant tag retension (proportion)
#'     Chronic tag loss (proportion)
#'@param t A vector of recapture liberties.
#'@return The probability of both tags remaining after time t.
#'@examples Pab(c(0.5,0.1,1.0,0.2),1:10)
#'@export
#'

Pab <- function(pin,t)  (pin[1]*exp(-pin[2]*t)) * (pin[3]*exp(-pin[4]*t))
