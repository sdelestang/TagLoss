#'Function used by the TagLoss model.
#'
#'Used by the TagLoss model when animals have been tagged with two tags which are expected to have different probabilities of retension.
#'
#'@param pin  A vector of input parameters (length four):
#'     Instant tag retension of 'a' tag (proportion)
#'     Chronic tag loss of 'a' tag (proportion)
#'     Instant tag retension of 'b' tag (proportion)
#'     Chronic tag loss of 'b' tag (proportion)
#'@param t A vector of recapture liberties.
#'@return The probability of tag 'b' remaining after time t.
#'@examples Pb(c(0.5,0.1,1.0,0.2),1:10)
#'@export
#'

Pb <-  function(pin,t)  (pin[1]*exp(-pin[2]*t))   *  (1-(pin[3]*exp(-pin[4]*t)))
