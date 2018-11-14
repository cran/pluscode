#' @title Pluscode Encoder
#'
#' @description This package retrieves a pluscode by inputting latitude and longitude
#'
#' @param lats A valid latitude within the range -90 to 90
#'
#' @param lngs A valid latitude within the range -180 to 180
#'
#' @param precision Options are 2, 4, 8, or 10
#'
#' @return NULL
#'
#' @examples pluscode_encode(52.501450, -6.567180, 8)
#'
#' @export

pluscode_encode <- function(lats, lngs, precision) {
  precision<-if(missing(precision)) {10} else precision
  if(lats < -90 | lats > 90) {
    stop(paste0("The latitude is not valid, please enter value within the range -90 to 90"))
  }
  if(lngs < -180 | lngs > 180) {
    stop(paste0("The longitude is not valid, please enter value within the range -180 to 180"))
  }
  pluscode<-jsonlite::fromJSON(rawToChar(httr::GET(paste0("https://plus.codes/api?address=",lats,",",lngs))$content))$plus_code$global_code
  if(precision == 2) return(substr(pluscode,1,2))
  else if(precision == 4) return(substr(pluscode,1,4))
  else if(precision == 8) return(substr(pluscode,1,9))
  else if(precision == 10) return(substr(pluscode,1,11))
  else return(print("Invalid Precision - Please choose 2, 4, 8, or 10"))
}
