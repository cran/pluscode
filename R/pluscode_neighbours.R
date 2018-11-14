#' @title Pluscode Neighbours
#'
#' @description This function retrieves the neighbouring pluscodes of a valid pluscode, with a precision of 2, 4, 8, or 10 excluding the plus sign (which can be included)
#'
#' @param pluscode A valid pluscod 2, 4, 8, or 10 characters in length excluding the plus sign (which can be included)
#'
#' @return NULL
#'
#' @examples pluscode_neighbours("9C4MGC2M+H4")
#'
#' @export

pluscode_neighbours <- function(pluscode) {
  neighbours<-list("north" = pluscode_northneighbour(pluscode),
                   "northeast" = pluscode_northeastneighbour(pluscode),
                   "east" = pluscode_eastneighbour(pluscode),
                   "southeast" = pluscode_southeastneighbour(pluscode),
                   "south" = pluscode_southneighbour(pluscode),
                   "southwest" = pluscode_southwestneighbour(pluscode),
                   "west" = pluscode_westneighbour(pluscode),
                   "northwest" = pluscode_northwestneighbour(pluscode))
  return(neighbours)
}
