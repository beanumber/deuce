#' Scrapes CSV files from GITHUB repository
#' 
#' @param url The repo URL address
#' 
#' @export
#' 
fetch_repo_files <- function(url){
	
	page <- xml2::read_html(url) 

	files <- page %>%
	  rvest::html_nodes("td.content") %>%
	  rvest::html_nodes("a") %>%
	  rvest::html_attr("href")
	
	# Returns CSV
grep("\\.csv", files, value = T)
}
