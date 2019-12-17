#' Fetch Odds Data for Slam Matches
#'
#' This function extracts official match data from Tennis-data.co.uk
#' 
#' @param atp Logical; True if Men's ATP match, False if not
#' 
#' @examples 
#'  atp_odds <- fetch_odds(atp = T)
#'  wta_odds <- fetch_odds(atp = F)
#'
#' @export
#' 
fetch_odds <- function(atp = T){
	
	events <- c(
		"http://www.tennis-data.co.uk/ausopen.php",
		"http://www.tennis-data.co.uk/frenchopen.php",
		"http://www.tennis-data.co.uk/usopen.php",
		"http://www.tennis-data.co.uk/wimbledon.php"
	)
	
	
	pages <- lapply(events, xml2::read_html)
	
	csv_files <- lapply(pages, function(x){
		refs <- x %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
	file.path("http://www.tennis-data.co.uk/", grep("[0-9][0-9][0-9][0-9].*csv", refs, value = T))
	})
	
	csv_files <- unlist(csv_files)
	
	if(atp)
		files <- csv_files[!grepl("[0-9][0-9][0-9][0-9]w", csv_files)]
	else
		files <- csv_files[grepl("[0-9][0-9][0-9][0-9]w", csv_files)]
	
	
	data <-  lapply(files, function(x) read.csv(x, stringsAsFactors = F, header = T))
	
	data_names <- unique(unlist(lapply(data, names)))
	
	data <- do.call("rbind", lapply(data, function(x){
		for(i in data_names)
			if(!(i %in% names(x)))
				x[,i] <- NA
		x
	}))
	
	data$Date <- dmy(data$Date)
	data$Location <- sub(" $", "", data$Location)
	data$Tournament <- sub(" $", "", data$Tournament)
	
		
	data$id <- paste(
		data$Location, 
		data$Date,
		data$Winner,
		data$Loser,
		sep = "-"
	)
	
data
}
