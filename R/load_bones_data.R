#' loads test dataframe of bone shapes
#'
#' @return a dataframe of bone shapes (7 bones).
#'
#' @author Goutham Swaminathan \email{goutham1220@gmail.com}
#'
#' @examples
#'
#' \dontrun{
#'load_bones_data()
#' }
#'

load_bones_data <- function(){
  bones = bone_list
  bone_shapes = cbind(bone_list$x, bone_list$y)

  return(bone_shapes)
}


