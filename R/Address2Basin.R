#' Thai River basin name
#'
#' Finding the main River basin name using Address
#'
#' @param address Character, a Vector of address
#' @param address[1] Character, Subdistrict
#' @param address[2] Character, District
#' @param address[3] Character, Province
#'
#' @return Main River basin name
#' @export
#'
#' @examples Address2Basin(c("ลำปลาทิว","ลาดกระบัง","กรุงเทพมหานคร"))
#' @examples Address2Basin(c("บางบอนเหนือ","บางบอน","กรุงเทพมหานคร"))
#' @examples Address2Basin(c("บางบอน","บางบอน","กรุงเทพมหานคร"))
#'
#' @import dplyr


Address2Basin <- function(address) {

  temp <- MainBasin_Address %>%
    filter(Subdistrict == address[1] &
           District == address[2] &
           Province == address[3]) %>%
    select(Basin_name)

  ntemp <- nrow(temp)

  if (ntemp == 0) {
      b_name <- NA
  } else if (ntemp == 1) {
      b_name <- temp[1,1]
  } else if (ntemp == 2) {
      b_name <- paste0(temp[1,1],"/",temp[2,1])
  } else if (ntemp == 3) {
      b_name <- paste0(temp[1,1],"/",temp[2,1],"/",temp[3,1])
  }

  return(b_name)
}
