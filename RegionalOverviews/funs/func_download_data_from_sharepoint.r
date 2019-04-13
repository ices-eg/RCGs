 download_data_from_sharepoint<-function(sharepoint_address, filename_vector, dir_download_browser, dir_download_target, unzip = FALSE, delete_zip = FALSE)
	{
		# Nuno Prista, SLU, 2019 (project PanRegional SubGroup on Fisheries and Sampling Overviews)
			# improvement of function download_data_cs4, developed by Nuno Prista, IPMA, 2015 (project fishPi)
		
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

			
		# function for press key (from "https://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r")
			readkey <- function()
			{
				cat ("Press [enter] to continue")
				line <- readline()
			}
		
		# Note: for some reason if *png or *txt firefox opens automatically in browser and does not prompt for saving...
			if(sum(grepl(filename_vector, pat=".txt") | grepl(filename_vector, pat=".png"))>0) stop("Sorry, not yet developed for '.txt' and '.png' files")
		
		# opens connection
			browseURL(sharepoint_address)
		
		# waits for user and password
			print("Press key when sharepoint is open (you may need to login if you have not already)")
			readkey()
		
		# loop to download files
			for (filename in filename_vector)
			{	
			print(paste("Downloading",filename))

			# prompts download file to dir_download_browser
				browseURL(paste(sharepoint_address, filename, sep="/"))
	
			Sys.sleep(5); print(".waiting 5 secs for download to start...")
		
			# waits for file to be fully downloaded
				files<-list.files(dir_download_browser);
				#while(sum(grepl(files, pat=".part", fixed=T))>0 |  file.info(paste(dir_download_browser, filename, sep="/"))$size) 
				while(
					(grepl(filename, pat=".zip", fixed=T) & sum(grepl(files, pat=".part", fixed=T))>0) |  
						(!is.na(file.size(paste(dir_download_browser, filename, sep="/"))) & file.size(paste(dir_download_browser, filename, sep="/"))==0)) 
					{
					Sys.sleep(2); print(".waiting 2 more secs for download to finish...")
					files<-list.files(dir_download_browser)
					}
				print("Download finished!!")	
				print(paste("final file_size:", file.size(paste(dir_download_browser, filename, sep="/"))))
		# check
			if(!file.exists(paste(dir_download_browser, filename, sep="/"))) stop ("file not downloaded!")
			
		# copies data from dir_download_browser to dir_download_target, removing from dir_download_browser
			print(paste(".copying",filename,"from dir_download_browser to target.dir"))
			file.copy(from = paste(dir_download_browser,filename,sep="/"), to = dir_download_target)
			print(paste(".removing",filename,"from dir_download_browser"))
			file.remove(paste(dir_download_browser, filename,sep="/"))
		
			print("Done!")		

		if(unzip == TRUE)
			{
			if(grepl(filename, pat = ".zip", fixed=T))
				{
				Sys.sleep(2)
				print(".unzipping...")
	#			browser()
				unzip(zipfile = paste(dir_download_target, filename, sep="/"), exdir=dir_download_target)
				print("Done!")
					if(delete_zip == TRUE)
					{
					print("Deleting zip...")
					file.remove(paste(dir_download_target, filename,sep="/"))
					print("Done!")
					}
				} else print("att: zip options ignored - file extension is not zip")
			}
		}
	}
