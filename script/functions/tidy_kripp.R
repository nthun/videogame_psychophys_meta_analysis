# Tidy function for Krippendorff's alpha
# INPUT: A list, produced by irr::kripp.alpha()
# OUTPUT: a tibble containg the values of the Krippendorf's apha output
# EXAMPLE: tidy_kripp(screener_12_kripp)

library(tibble)

tidy_kripp <- function(kripp){
    stopifnot(is.list(kripp),
              has_name(kripp, c("method","data.level","raters","subjects","value")))
    
    tibble(
        method = kripp$method,
        level = kripp$data.level,
        raters = kripp$raters,
        items = kripp$subjects,
        value = kripp$value
    )
}