#' Thai River basin name
#'
#' Finding the main River basin name using Addresses in xlsx
#'
#' @param address_table Character, a Vector of address-table
#' @param address_table[,1] Character, Subdistrict
#' @param address_table[,2] Character, District
#' @param address_table[,3] Character, Province
#'
#' @return Main River basin name, vector
#' @export
#'
#' @examples address_table <- system.file("extdata","Address_table.xlsx", package = "AllWaterThai")
#' @examples addressv <- read.xlsx(address_table)
#' @examples head(addressv)
#' @examples Basin <- Address2BasinTbl(address_table)
#'
#' @import dplyr
#' @import openxlsx
#'

Address2BasinTbl <- function(address_table) {

  address_t <- read.xlsx(address_table)

  nadd <- nrow(address_t)
  basin_name <- rep("",nadd)
  add <- rep("",3)

  for (i in 1:nadd) {
    for (j in 1:3) {
      add[j] <- address_t[i,j]
    }
    basin_name[i] <- Address2Basin(add)
  }

  return(basin_name)
}

