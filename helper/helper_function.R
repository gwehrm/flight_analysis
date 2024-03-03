helper_example <- function(parameter = "") {
  #description of function
  tryCatch(
    expr = {
      y = 3 * 4

      return(y)
    },
    error = function(e) {
      cat("error: ", e)
    }
  )

}
