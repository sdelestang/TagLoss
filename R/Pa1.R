#'Function used by the TagLos model.
#'
#'Used by the TagLoss model when animals have been tagged with two tags which are expected to have different probabilities of retension.
#'
#'@param pin  A vector of input parameters (length four):
#'     Instant tag retension of 'a' tag (proportion)
#'     Chronic tag loss of 'a' tag (proportion)
#'     Instant tag retension of 'b' tag (proportion)
#'     Chronic tag loss of 'b' tag (proportion)
#'@param t A vector of recapture liberties.
#'@return The probability of tag 'a' remaining after time t.
#'@examples Pa.(c(0.5,0.1,1.0,0.2),1:10)
#'@export
#'

Pa. <-  function(pin,t)  (pin[3]*exp(-pin[4]*t))   *  (1-(pin[1]*exp(-pin[2]*t)))
