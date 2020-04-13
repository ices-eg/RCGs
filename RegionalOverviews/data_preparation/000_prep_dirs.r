# RCG subgroup work on Regional Fisheries and Sampling Overview
	# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël
	# 2019
	
	
# creates directory structure for overviews

	for (target_region in c("RCG_BA","RCG_NA","RCG_NSEA"))
		{
		dir.create(paste("data\\002_prepared\\", target_region, sep=""), recursive=TRUE, showWarnings = FALSE)
		for (overview in c("Annual_Overview","MultiAnnual_Overview","Sampling_Overview"))
			{
			dir.create(paste("graphical_parameters\\", target_region,"\\",overview, sep=""), recursive=TRUE, showWarnings = FALSE)
			dir.create(paste("outputs\\", target_region,"\\",overview,"\\figures", sep=""), recursive=TRUE, showWarnings = FALSE)
			dir.create(paste("outputs\\", target_region,"\\",overview,"\\tables", sep=""), recursive=TRUE, showWarnings = FALSE)
			dir.create(paste("outputs\\", target_region,"\\",overview,"\\report", sep=""), recursive=TRUE, showWarnings = FALSE)
			}
		}	