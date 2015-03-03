rename.r

setwd(frame_dir)

for(video_file in list.files(getwd())) {

	# print(video_file)

	# require(stringr)
	# Set <- as.integer(str_match(video_file, "(.+)_S([0-9][0-9][0-9])([0-9][0-9][0-9])_(.+)")[, 3])
	# Trap <- as.integer(str_match(video_file, "(.+)_S([0-9][0-9][0-9])([0-9][0-9][0-9])_(.+)")[, 4])

	# Set <- formatC(Set, flag = "0", width = 3)
	# Trap <- formatC(Trap, flag = "0", width = 3)

	# F <- str_match(video_file, "(.+_S)([0-9][0-9][0-9])([0-9][0-9][0-9])(_.+)")[, 2]
	# L <- str_match(video_file, "(.+_S)([0-9][0-9][0-9])([0-9][0-9][0-9])(_.+)\\.MP4")[, 5]

	# new_filename = paste(F, Set, "T", Trap, L, ".MP4", sep = "")
	# print(new_filename)

	# system(paste("mv ", video_dir, "/", video_file, " ",
	# 	video_dir, "/", new_filename, sep = ""))

# ------------------------------------

	print(video_file)

	require(stringr)
	Set <- as.integer(str_match(video_file, "(.+)_S([0-9][0-9][0-9])([0-9][0-9][0-9])_(.+)")[, 3])
	Trap <- as.integer(str_match(video_file, "(.+)_S([0-9][0-9][0-9])([0-9][0-9][0-9])_(.+)")[, 4])

	Set <- formatC(Set, flag = "0", width = 3)
	Trap <- formatC(Trap, flag = "0", width = 3)

	F <- str_match(video_file, "(.+_S)([0-9][0-9][0-9])([0-9][0-9][0-9])(_.+)")[, 2]
	L <- str_match(video_file, "(.+_S)([0-9][0-9][0-9])([0-9][0-9][0-9])(_.+)\\.jpg")[, 5]

	new_filename = paste(F, Set, "T", Trap, L, ".jpg", sep = "")
	print(new_filename)

	system(paste("mv ", frame_dir, "/", video_file, " ",
		frame_dir, "/", new_filename, sep = ""))

}