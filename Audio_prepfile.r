# ------------------------------------------------------
# extracts audio from video files and copies it to audio folder

setwd(video_dir)
for(filenameMP4 in list.files(video_dir)) {
	require(stringr)
	filename <- str_match(filenameMP4, "(.+)\\.mp4")[, 2]
	system(command = paste("ffmpeg -i ", filename, ".mp4", " ", filename, ".wav", sep = ""))
	system(command = paste("mv ", video_dir, "/", filename, ".wav", " ", audio_dir, "/", filename, ".wav", sep = ""))
}
