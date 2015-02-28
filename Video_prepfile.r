# moves and renames video
require(rPython)
for(i in seq(1, length(from_dirs),1 )) {
	setwd(master_dir)
	from_dir = from_dirs[i]
	from_idx = from_idxs[i]
	python.load("1_Rename_video.py")
	python.call("Rename_video", from_idxs, from_dir, to_dir)
}

# --------------------------------------

# bad_video_files <- as.character(as.vector(read.table("Bad_video_files.txt")[, 1]))
# setwd(video_dir)
# for(bad_video_file in bad_video_files) {
# 	print(paste("Removing", bad_video_file))
# 	system(paste("rm ", bad_video_file))
# }
