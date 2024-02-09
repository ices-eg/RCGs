download_data_from_sharepoint<-function(sharepoint_address, filename_vector, dir_download_browser, dir_download_target, unzip = FALSE, delete_zip = FALSE){
	# Author: Nuno Prista, SLU, 2019 (project PanRegional SubGroup on Fisheries and Sampling Overviews)
		# improvement of function download_data_cs4, developed by Nuno Prista, IPMA, 2015 (project fishPi)
	
	# Contributor(s): 
		# Eros Quesada, SLU, 2024

	# Downloads data from e.g., ICES sharepoint into a dir_download_target in user's computer, unzips file upon request
	
	# Note: developed for Firefox: may (or may not) work on IE
	
	# Requires:
		#sharepoint_address: url
		#filename_vector: vector of filenames to download
		#dir_download_browser: directory where firefox/ie downloads to
		#dir_download_target = final directory where the data should be
	
	# Example:
		#sharepoint_address <- "ADD_HERE_website_address"
		#download_data_from_sharepoint (sharepoint_address, filename_vector = c("CL Landing 2009-2017.zip","CE Effort 2009-2017.zip"), dir_download_browser = "ADD_HERE_download_folder_adress", dir_download_target = getwd(), unzip=TRUE)

	# Development notes
		# 2019-04-03: fixed bug when downloads very fast (small files); simplified code
		# 2019-04-13: fixed another bug when downloads very fast (small files); simplified code
		# 2019-04-13: added option to delete_zip
		# 2019-04-13: fixed bug in saving (was saving large files too fast, with size==0)
		# 2024-02-07: ensure overwriting when unzipping the file at the target directory and print to cat 

	# function for press key (from "https://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r")
	readkey <- function()
	{
		cat ("       Press", magenta("[enter]"), "to continue")
		cat("\n")
		line <- readline()
	}
	
	# Note: for some reason if *png or *txt firefox opens automatically in browser and does not prompt for saving...
	if(sum(grepl(filename_vector, pat=".txt") | grepl(filename_vector, pat=".png"))>0) stop("Sorry, not yet developed for '.txt' and '.png' files")

	# opens connection
	suppressMessages(
		browseURL(sharepoint_address)
	)

	# waits for user and password
	cat("       Press", magenta("[key]"), "when sharepoint is open (you may need to login if you have not already)")
	cat("\n")
	readkey()

	# loop to download files
	for (filename in filename_vector){	
		cat(paste("         Downloading",filename))
		cat("\n")

		# Check if file already exists at target location and, if so, delete it. (Otherwise it won´t unzip). 
		if(any(list.files(dir_download_target) %in% filename)){ 
			cat("           .pre-existing file with the same naming found at target directory, replacing...")
			cat("\n")
			unlink(paste(dir_download_target, filename, sep = "/"),recursive=TRUE)
			if(any(list.files(dir_download_target) %in% filename)){ # Checking that the file is no more there.
				stop("         Unable to overwrite existing file")
			}
		}

		# prompts download file to dir_download_browser
		suppressMessages(
			browseURL(paste(sharepoint_address, filename, sep="/"))
		)

		Sys.sleep(5)
		cat("           .waiting 5 secs for download to start...")
		cat("\n")

		# waits for file to be fully downloaded
		files<-list.files(dir_download_browser);
		#while(sum(grepl(files, pat=".part", fixed=T))>0 |  file.info(paste(dir_download_browser, filename, sep="/"))$size) 

		while(
			(grepl(filename, pat=".zip", fixed=T) & sum(grepl(files, pat=".part", fixed=T))>0) |  
			(is.na(file.size(paste(dir_download_browser, filename, sep="/"))) & file.size(paste(dir_download_browser, paste0(filename, ".zip"), sep="/"))==0)){
			Sys.sleep(2); cat("           .waiting 2 more secs for download to finish..."); cat("\n")
			files<-list.files(dir_download_browser)
			}
		cat("\n")
		cat("         Download finished!")	
		cat(paste(" Final file_size:", file.size(paste(dir_download_browser, filename, sep="/")), "bytes"))
		cat("\n")
		# check
		if(!file.exists(paste(dir_download_browser, filename, sep="/"))) stop ("file not downloaded!")
	
		# copies data from dir_download_browser to dir_download_target, removing from dir_download_browser
		cat("\n")
		cat(paste("           .copying",filename,"from dir_download_browser to target.dir"))
		cat("\n")
		file.copy(from = paste(dir_download_browser,filename,sep="/"), to = dir_download_target)
		cat(paste("           .removing",filename,"from dir_download_browser"))
		cat("\n")
		file.remove(paste(dir_download_browser, filename,sep="/"))
		cat("           .done!"); cat("\n")		

		if(unzip == TRUE){
			if(grepl(filename, pat = ".zip", fixed=T)){
				Sys.sleep(2)
				cat("\n")
				cat("           .unzipping...")
				cat("\n")
				unzip(zipfile = paste(dir_download_target, filename, sep="/"), exdir=dir_download_target)
				cat("           .done!")
				cat("\n")
			} 

		if(delete_zip == TRUE){
			cat("\n")
			cat("         Deleting zip...")
			cat("\n")
			file.remove(paste(dir_download_target, filename,sep="/"))
			cat("           .done!"); cat("\n")
		}
		} else {
			cat("         att: zip options ignored - file extension is not zip"); cat("\n")
		}
	}
}
