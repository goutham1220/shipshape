#' split a matrix into desired 3-D array at set interval.
#'
#'@param matrix input matrix
#'@param interval interval for splitting of matrix
#'
#' @return 3-D array
#'
#' @author Goutham Swaminathan \email{goutham1220@gmail.com}
#'
#' @examples
#'
#' \dontrun{
#' test_data = bone_list
#' bones = data_split(test_data, 100)
#' }
#'

data_split <- function(matrix, interval){

  mat = matrix(data = 1:24, nrow = 12, ncol = 2)
  interval = 3

  z = nrow(mat)/interval

  arr = array(dim = c(interval, ncol(mat), z))

  arr[,,1] = mat[1:interval,]

  for(i in 2:z){

    start = (i-1) * interval + 1
    end = i * interval
    arr[,,i] = mat[start:end,]

  }

  return(arr)

}
