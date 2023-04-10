#' @title 
#'
#' @description 
#'
#' @param data
#' @param uniModel
#'
#' @return
#' @author Shengjie Xiu and Yifan Yu
#' @references
#' 
#' @seealso 
#' @examples
#' @export
uniModelFilter <- function(data, uniModel) {
  # error control
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data.frame.")
  if (anyNA(data)) stop("data must have no NA.")
  is_uniModel(uniModel, nrow(data))

  # check if fit is necessary
  if (Reduce("+", uniModel$fit_request) != 0) {
    stop("All parameters must be fixed.\n")
  }

  # filter using MARSS
  components <- marss_filter(data, uniModel)

  return(components)
}

marss_filter <- function(data, uniModel) {
  data <- as.matrix(data)
  args <- list(
    data = data,
    uniModel = uniModel
  )
  marss_obj <- do.call(specify_marss, args)
  Kf <- MARSS::MARSSkfas(marss_obj)
  result <- list(
    "daily" = Kf$xtT[1, ],
    "dynamic" = Kf$xtT[2, ],
    "seasonal" = as.vector(marss_obj$model$fixed[["A"]])
  )

  return(result)
}
