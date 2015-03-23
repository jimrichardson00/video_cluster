
setwd(master_dir)

system("rm ccipca_video.RData")
system("rm data_video.RData")
system("rm data_areas.RData")

system("rm ccipca_video.RData")
system("rm data_video.RData")
system("rm data_areas.RData")

system("rm n_components.txt")  
system("rm iteration.txt")  
system("rm amnesic.txt") 
system("rm copy.txt")   
system("rm mean_.txt")
system("rm components_.txt")

# ---------------------------------------

system(paste("rm -r ", clust_dir, sep = ""))
system(paste("mkdir ", clust_dir, sep = ""))
system(paste("mkdir ", clust_dir, "/new", sep = ""))
system(paste("mkdir ", clust_dir, "/cur", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/ImageClarity_validated", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/Presence", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/Features", sep = ""))
system(paste("mkdir ", clust_dir, "/cur/Clear/BioticMaterial_validated", sep = ""))

