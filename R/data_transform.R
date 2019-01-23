#' Transforms data into usable format.
#'
#'@param data input data
#'
#' @return transformed data
#'
#' @author Goutham Swaminathan \email{goutham1220@gmail.com}
#'
#' @examples
#'
#' \dontrun{
#' test_data = load_bones_data()
#' test_data = t(test_data)
#'transform_data(test_data)
#' }
#'

data_transform <- function(data){

  return(t(data))
}
