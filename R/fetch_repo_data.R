#' Fetch Repo Data
#' 
#' This function fetches a csv from the desired GitHub repo to be used in match and player analysis
#' 
#' @param file file path of desired csv
#' @param ... other options passed to fetch_repo_data
#' 
#' @return A csv file read in as desired
#'
#' @export
#' 
fetch_repo_data <- function(file, ...){
	file <- file.path("https://raw.githubusercontent.com", stringr::str_remove(file, "^/"))
	file <- stringr::str_remove(file, ".blob")
read.csv(file, ...)
}
