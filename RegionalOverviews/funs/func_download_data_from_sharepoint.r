 download_data_from_sharepoint<-function(sharepoint_address, filename_vector, dir_download_browser, dir_download_target, unzip)
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

			
		# function for press key (from "https://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r")
			readkey <- function()
			{
				cat ("Press [enter] to continue")
				line <- readline()
			}
		
		# opens connection
			browseURL(sharepoint_address)
		
		# waits for user and password
			print("Press key when sharepoint is open")
			readkey()
		
		# loop to download files
			for (filename in filename_vector)
			{	
			print(paste("Downloading",filename))

			# prompts download file to dir_download_browser
				browseURL(paste(sharepoint_address, filename, sep="/"))

		
			# waits for file to be fully downloaded
				files<-list.files(dir_download_browser)
				while(sum(grepl(files, pat=".part", fixed=T))>0 | !file.exists(paste(dir_download_browser, filename, sep="/"))) 
					{
					Sys.sleep(2); print(".waiting 2 more secs for download to finish...")
					files<-list.files(dir_download_browser)
					}
					
			print("Download finished!!")	
		
		# copies data from dir_download_browser to dir_download_target, removing from dir_download_browser
			print(paste(".copying",filename,"from dir_download_browser to target.dir"))
			file.copy(from = paste(dir_download_browser,filename,sep="/"), to = dir_download_target)
			print(paste(".removing",filename,"from dir_download_browser"))
			file.remove(paste(dir_download_browser, filename,sep="/"))
		}
		print("Done!")		
	
	if(unzip == TRUE)
		{
		unzip(zipfile = paste(dir_download_target, filename, sep="/"))
		}
	
	}
