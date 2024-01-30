# load CL data
load(
  paste(params$data_dir, '/', params$CLfileName,'.Rdata', sep = "")
)
cl = cl_rcg # shorter name 

#load CE data

load(
  paste(params$data_dir, '/' ,params$CEfileName,'.Rdata', sep = "")
)

ce = ce_rcg # shorter name 

# put some necessary data prep part below