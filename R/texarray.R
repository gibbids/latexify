#' Convert a matrix to LaTeX table format.
#'
#' Converts a matrix to a LaTeX table format using the \code{array} environment.
#'
#' @param mat A matrix to convert to LaTeX format.
#' @param barpos The number of columns from the right where to place the vertical line. Defaults to 0 (no vertical line).
#' @param bracket The type of bracket to use for the array. Defaults to none.
#' @param small Boolean flag indicating whether to use the small matrix format for inline use. Defaults to FALSE.
#'
#' @return A character vector containing the LaTeX code for the matrix in \code{array} environment format.
#'
#' @examples
#' mat <- matrix(1:6, nrow = 2)
#' texarray(mat)
#'
#' @author Gabe Osmo
#' @export
#' 
texarray <- function(mat, barpos = 0, bracket = "", small = FALSE) {
  
  # Check input
  if (!is.matrix(mat)) stop("Input must be a matrix.")
  if (barpos < 0 || barpos >= ncol(mat)) stop("barpos must be between 0 and the number of columns in the matrix minus 1.")
  if (bracket != "" && bracket != "(" && bracket != "[" && bracket != "{" && bracket != "|" && bracket != "\\lceil" && bracket != "\\lfloor") {
    stop("Invalid bracket type. Choose from '', '(', '[', '{', '|', \\lceil, or \\lfloor.")
  }
  
  # Determine bracket type
  if (bracket == "") {
    open_bracket <- ""
    close_bracket <- ""
  } else if (bracket == "|" && barpos == 0) {
    open_bracket <- "\\left"
    close_bracket <- "\\right|"
  } else {
    open_bracket <- paste0("\\left", bracket)
    close_bracket <- paste0("\\right", ifelse(bracket %in% c("|", "\\lceil", "\\lfloor"), "", bracket))
  }
  
  # Determine column formatting
  col_format <- ifelse(barpos == 0, "c", paste0(rep("c|", barpos), collapse = "", "c", paste0(rep("|c", ncol(mat) - barpos - 1), collapse = "")))
  
  # Determine matrix formatting
  if (small) {
    mat_format <- paste0("\\begin{smallmatrix}", "\n")
    end_format <- "\\end{smallmatrix}"
  } else {
    mat_format <- "\\begin{array}"
    end_format <- "\\end{array}"
  }
  
  # Create LaTeX code
  output <- paste0(open_bracket, mat_format, "{", col_format, "}", "\n")
  for (i in 1:nrow(mat)) {
    row_string <- paste0(mat[i, ], collapse = " & ")
    output <- paste0(output, row_string, " \\\\ \n")
  }
  output <- paste0(output, end_format, close_bracket, "\n")
  
  return(output)
}