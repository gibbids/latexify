#' Convert a matrix to LaTeX table format.
#'
#' Converts a matrix to a LaTeX table format using the \code{array} environment.
#'
#' @param mat A matrix to convert to LaTeX format.
#' @param barpos The number of columns from the right where to place the vertical line. 
#'               Defaults to 0 (no vertical line).
#' @param bracket The type of bracket to use for the array. Defaults to none.
#'                 Valid options include "", "(", ")", "[", "]", "{", "}", "|", \lceil, and \lfloor.
#' @param small Boolean flag indicating whether to use the small matrix format for inline use. 
#'              Defaults to FALSE.
#'
#' @return A character vector containing the LaTeX code for the matrix in \code{array} environment format.
#'
#' @examples
#' # Create a 2x3 matrix
#' mat <- matrix(1:6, nrow = 2)
#' 
#' # Convert matrix to LaTeX format without vertical line
#' texarray(mat)
#' 
#' # Convert matrix to LaTeX format with vertical line at second column
#' texarray(mat, barpos = 1)
#' 
#' # Convert matrix to LaTeX format with square brackets
#' texarray(mat, bracket = "[")
#' 
#' # Convert matrix to LaTeX format with small matrix formatting
#' texarray(mat, small = TRUE)
#' 
#' # Attempt to convert a non-matrix input
#' texarray(1:6) # will throw an error
#' 
#' # Attempt to set barpos greater than the number of columns minus one
#' texarray(mat, barpos = 3) # will throw an error
#' 
#' # Attempt to set an invalid bracket type
#' texarray(mat, bracket = "{|}") # will throw an error
#'
#' @seealso
#' \code{\link{matrix}}, \code{\link{paste}}, \code{\link{substr}}, \code{\link{strrep}}
#' 
#' @author Gabe Osmo
#' @export

texarray <- function(mat, barpos = 0, bracket = "", small = FALSE) {
  
  #Create NROW and NCOL variables
  NCOL <- ncol(mat)
  NROW <- nrow(mat)
  
  # Check input
  if (!is.matrix(mat)) stop("Input must be a matrix.")
  if (barpos < 0 || barpos > (NCOL-1)) stop("barpos must be between 0 and the number of columns in the matrix minus 1.")
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
    close_bracket <- paste0("\\right",
                            ifelse(bracket %in% c("|",
                                                  "\\lceil",
                                                  "\\lfloor"),
                                   "",
                                   bracket))
  }
  
  # Create the column formatting
  col_format = strrep('c', NCOL)
  if(barpos != 0){
    col_format <- paste(substr(col_format, 1, NCOL-barpos), "|",
                        substr(col_format, NCOL-barpos+1, NCOL),
                        sep = "")
  }
  
  # Determine matrix formatting
  if (small) {
    mat_format <- paste0("\\begin{smallmatrix}", " \n")
    end_format <- "\\end{smallmatrix}"
  } else {
    mat_format <- "\\begin{array}"
    end_format <- "\\end{array}"
  }
  
  # Create LaTeX code
  output <- paste0(open_bracket, mat_format, "{", col_format, "}", " \n")
  for (i in 1:NROW) {
    row_string <- paste0(mat[i, ], collapse = " & ")
    output <- paste0(output, row_string, " \\\\ \n")
  }
  output <- paste0(output, end_format, close_bracket, " \n")
  
  return(output)
}