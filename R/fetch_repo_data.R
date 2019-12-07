#' Fetch Repo Data
#' 
#' This function fetches a csv from the desired GitHub repo to be used in match and player analysis
#' 
#' @param file File path of desired csv
#' 
#' @return A csv file read in as desired
#'
#' @export
#' 
fetch_repo_data <- function(file, ...){
	file <- file.path("https://raw.githubusercontent.com", str_remove(file, "^/"))
	file <- str_remove(file, ".blob")
read_csv(file, ...)
}
