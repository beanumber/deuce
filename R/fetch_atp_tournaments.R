#' Download Current ATP Calendar
#'
#' This function downloads the ATP calendar
#'
#' @param challenger Logical indicator if fetching challenger or World Tour calendar
#'
#'
#' @export
fetch_atp_tournaments <- function(challenger = FALSE){

	if(challenger)
		url <- "http://www.atpworldtour.com/en/atp-challenger-tour/calendar"
	else
		url <- "http://www.atpworldtour.com/en/tournaments"
		
	page <- xml2::read_html(url)
	
	tournaments <- page %>% rvest::html_nodes(".tourney-title") %>%
		rvest::html_attr("data-ga-label")
		
	locations <- page %>% rvest::html_nodes(".tourney-location") %>%
		rvest::html_text(trim = T)

		prize_extract <- function(x){
			numbers <- stringr::str_extract_all(x, "[0-9]")
			numbers <- collapse(numbers[[1]])
			location <- stringr::str_locate(x, "[0-9]")[1]
			currency <- substr(x, location - 1, location - 1)
			if(currency == "#") currency <- "£"
			if(grepl("A\\$", x)) currency <- "A$"
		collapse(currency, numbers)
		}
		
	dates <- page %>% rvest::html_nodes(".tourney-dates") %>%
	  rvest::html_text(trim = T)
		
	start_date <- stringr::str_extract(dates, "^[0-9][0-9][0-9][0-9].[0-9][0-9].[0-9][0-9]")
	end_date <- stringr::str_extract(dates, "[0-9][0-9][0-9][0-9].[0-9][0-9].[0-9][0-9]$")
	
	details <- page %>% rvest::html_nodes(".tourney-result") 
	
	item_details <- lapply(details, function(x){
		x %>%
		rvest::html_nodes(".tourney-details") %>%
		rvest::html_nodes(".item-value") %>%
		rvest::html_text(trim = T)		
		})
		
	draw_size <- as.numeric(sapply(item_details, function(x) x[1])) # Take first number
	
	surface <- sapply(item_details, function(x) ifelse(any(x == "Clay"), "Clay", ifelse(any(x == "Grass"), "Grass", "Hard")))
	
	prize <- sapply(item_details, function(x) ifelse(grepl("[0-9]", x[length(x)]), x[length(x)], NA))

	data.frame(
		name = tournaments,
		location = locations,
		start_date = as.Date(start_date, format = "%Y.%m.%d"),
		end_date = as.Date(end_date, format = "%Y.%m.%d"),	
		draw = as.numeric(draw_size),
		matches = as.numeric(draw_size) - 1,
		surface = surface,
		prize = prize,
		stringsAsFactors = F
	)		

}
