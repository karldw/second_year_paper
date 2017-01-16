# Downloaded from
# https://github.com/ajdamico/asdfree/blob/070678de772a9dddb200c8712bdeb61fed32e76c/Consumer%20Expenditure%20Survey/download%20all%20microdata.R
# 2016-10-30

# Original code by Anthony Joseph Damico <ajdamico@gmail.com>


BLS_INFO_URL <- 'http://www.bls.gov/cex/pumd_data.htm#stata'
BLS_FILE_URL <- 'http://www.bls.gov/cex/pumd/data/stata'
SAVE_DIR <- 'C:/Users/Karl/Box Sync/second_year_paper_data/CEX/stata_download'
stopifnot(dir.exists(SAVE_DIR))
library(dplyr)
library(ensurer)
library(haven)

# remove the # in order to run this install.packages line only once
# install.packages( c( "MonetDBLite" , "sqldf" , "survey" , "SAScii" , "descr" , "mitools" , "plyr" , "downloader" , "digest" , "readxl" , "stringr" , "reshape2" , "R.utils" , "rvest" ) )



get_years <- function() {
	# figure out which years are available to download
	interview_regex <- ".*/pumd/data/stata/intrvw([0-9][0-9])\\.zip.*"
	# set a max length to prevent R from getting too ahead of itself
	data_years <- readLines(BLS_INFO_URL, n=10000, warn=FALSE) %>%
		grep(interview_regex, ., value=TRUE, perl=TRUE) %>%
		gsub(interview_regex, "\\1", ., perl=TRUE) %>%
		unique() %>%
		# These should all be two-digit years
		ensure(all(nchar(.) == 2)) %>%
		as.numeric() %>%
		ensure(! anyNA(.))
	# add centuries
	data_years <- if_else(data_years > 96, 1900 + data_years, 2000 + data_years)
	return(data_years)
}


download_one_year <- function(yr) {
	year_2digit <- substr(yr, 3, 4)

	diary_filename <- paste0('diary', year_2digit, '.zip')
	diary_url <- file.path(BLS_FILE_URL, diary_filename)
	diary_outfile <- file.path(SAVE_DIR, diary_filename)
	interview_filename <- paste0('intrvw', year_2digit, '.zip')
	interview_url <- file.path(BLS_FILE_URL, interview_filename)
	interview_outfile <- file.path(SAVE_DIR, interview_filename)
	if (! file.exists(interview_outfile)) {
		download.file(interview_url, interview_outfile, mode='wb', quiet=TRUE)
	}
	if (! file.exists(diary_outfile)) {
		download.file(diary_url, diary_outfile, mode='wb', quiet=TRUE)
	}
	downloaded_files <- c(interview_outfile, diary_outfile)
	invisible(downloaded_files)
}



main <- function(){
	years_of_data <- get_years()
	downloaded_files <- lapply(years_of_data, download_one_year)

}

years_of_data <- get_years()
downloaded_files <- lapply(years_of_data, download_one_year)



#
# # loop through each year requested by the user
# for ( year in years.to.download ){
#
#
# 	# loop through the interview, expenditure, diary, and documentation files and..
# 	# download each to a temporary file
# 	# unzip each to a directory within the current working directory
# 	# save in each of the requested formats
# 	for ( fn in ttd ){
#
# 		# filetype-specific output directory
# 		output.directory <- paste0( getwd() , "/" , year , "/" , fn )
#
# 		# if the filetype-specific output directory doesn't exist, create it
# 		try( dir.create( output.directory ) , silent = T )
#
# 		# copy over the filetype-specific ftp path
# 		ftp <- get( paste( fn , "ftp" , sep = "." ) )
#
# 		# download the filetype-specific zipped file
# 		# and save it as the temporary file
# 		download_cached( ftp , tf , mode = 'wb' )
#
#
# 		# unzip all of the files in the downloaded .zip file into the current working directory
# 		# then save all of their unzipped locations into a character vector called 'files'
# 		files <- unzip( tf , exdir = output.directory )
# 		# note that this saves *all* of the files contained in the .zip
#
# 		# loop through each of the dta files and (depending on the conversion options set above) save files in necessary formats
#
# 		# identify dta files
# 		dta.files <- files[ grep( '\\.dta' , files ) ]
#
# 		# loop through a character vector containing the complete filepath
# 		# of each of the dta files downloaded to the local disk..
# 		for ( i in dta.files ){
#
# 			# figure out where the final '/' lies in the string
# 			sl <- max( gregexpr( "\\/" , i )[[1]] )
#
# 			# use that to figure out the filename (without the directory)
# 			dta.fn <- substr( i , sl + 1 , nchar( i ) )
#
# 			# figure out where the last '.' lies in the string
# 			dp <- max( gregexpr( "\\." , i )[[ 1 ]] )
#
# 			# use that to figure out the filename (without the directory or the extension)
# 			df.name <- substr( i , sl + 1 , dp - 1 )
#
# 			# read the current stata-readable (.dta) file into R
# 			x <- read.dta( i )
#
# 			# if the data.frame is a family file, tack on poverty thresholds
# 			if( grepl( "fmli" , df.name ) ){
#
# 				# subset the complete threshold data down to only the current year
# 				thresh_merge <- subset( all_thresholds , this_year == year )
#
# 				# remove the `year` column
# 				thresh_merge$this_year <- NULL
#
# 				# rename fields so they merge cleanly
# 				names( thresh_merge ) <- c( 'family_type' , 'num_kids' , 'poverty_threshold' )
#
# 				x$num_kids <- if_else( x$perslt18 > 8 , 8 , x$perslt18 )
# 				x$num_kids <- if_else( x$num_kids == x$fam_size , x$fam_size - 1 , x$num_kids )
#
# 				# re-categorize family sizes to match census groups
# 				x$family_type <-
# 					if_else( x$fam_size == 1 & x$age_ref < 65 , "Under 65 years" ,
# 					if_else( x$fam_size == 1 & x$age_ref >= 65 , "65 years and over" ,
# 					if_else( x$fam_size == 2 & x$age_ref < 65 , "Householder under 65 years" ,
# 					if_else( x$fam_size == 2 & x$age_ref >= 65 , "Householder 65 years and over" ,
# 					if_else( x$fam_size == 3 , "Three people" ,
# 					if_else( x$fam_size == 4 , "Four people" ,
# 					if_else( x$fam_size == 5 , "Five people" ,
# 					if_else( x$fam_size == 6 , "Six people" ,
# 					if_else( x$fam_size == 7 , "Seven people" ,
# 					if_else( x$fam_size == 8 , "Eight people" ,
# 					if_else( x$fam_size >= 9 , "Nine people or more" , NA ) ) ) ) ) ) ) ) ) ) )
#
# 				# merge on the `poverty_threshold` variable while
# 				# confirming no records were tossed
# 				before_nrow <- nrow( x )
#
# 				x <- merge( x , thresh_merge )
#
# 				stopifnot( nrow( x ) == before_nrow )
#
# 			}
#
# 			# save it to an object named by what's contained in the df.name character string
# 			assign( df.name , x )
#
# 			# save the file as an R data file (.rda) immediately
# 			save( list = df.name , file = paste0( output.directory , "/" , df.name , ".rda" ) )
#
# 			# since the file has been read into RAM, it should be deleted as well
# 			rm( list = df.name ) ; rm( x )
#
# 			# clear up RAM
# 			gc()
#
# 			# then delete the original file from the local disk
# 			file.remove( i )
#
# 		}
# 	}
# }
