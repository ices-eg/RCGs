# function that determines which bars in a barplot need inset because they are too small
# Nuno Prista, SLU, Sweden
# Developed @ RCG subgroup work 2020

determine_what_to_inset<-function(freq, target_ratio){

# freq is a sorted frequency distribution
# ratio is the min ratio of inset bars to 1st bar in main plot

ratios<-freq[1]/freq;
output<-names(freq[ratios>target_ratio])
if(length(output)>1) output else NULL
}

# example:
# freq<-sort(rlnorm(n=size, meanlog = 0, sdlog = 2),decreasing=T)
# names(freq)<-1:size; freq
# determine_what_to_inset(freq = freq, target_ratio=10)

# see how it works with different types of frequencies
	# size=50
	# windows(30,20); par(mfcol=c(3,4))
	# for(i in c(0.5, 1, 2, 3))
	# {
	# for(j in 1:3)
	# {
	# freq<-sort(rlnorm(n=size, meanlog = 0, sdlog = i),decreasing=T)
	# names(freq)<-1:size; freq
	# graph<-barplot(freq, las=2, cex.names=0.7, main=paste("sdlog:",i))
	# inset<-determine_what_to_inset(freq = freq, target_ratio=10);inset
	# abline(v=graph[match(inset, names(freq))][1], col="red")
	# }
	# }


