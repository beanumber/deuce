#' Tidy Rankings Data
#' 
#' This tidy's the ranknings from the imported data
#' 
#' @param data A data table containing player ranking data  
#'  
#' @export
tidy_rankings_data <- function(data){
	
	names(data) <- c(
		"ranking_date",
		"ranking",
		"player_id",
		"ranking_points"
	)
	
	data$ranking_points[grepl("[[:punct:]]", data$ranking_points)] <- NA
	data$ranking[grepl("[[:punct:]]", data$ranking)] <- NA
	
	data$date <- ymd(data$ranking_date)


data
}

