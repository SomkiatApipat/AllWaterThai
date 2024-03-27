#' Thai River basin name
#'
#' Finding the main River basin name using Addresses in xlsx
#'
#' @param address Character, data frame of address
#' @param address[,1] Character, Subdistrict
#' @param address[,2] Character, District
#' @param address[,3] Character, Province
#'
#' @return Main River basin name, vector
#' @export
#'
#' @examples data(address)
#' @examples address
#' @examples Address2BasinTbl(address)
#'
#' @import openxlsx
#' @import dplyr
#' @export

Address2BasinTbl <- function(address) {

  nadd <- nrow(address)
  basin_name <- rep("",nadd)
  add <- rep("",3)

  for (i in 1:nadd) {
    for (j in 1:3) {
      add[j] <- address[i,j]
    }
    basin_name[i] <- Address2Basin(add)
  }

  return(basin_name)
}

