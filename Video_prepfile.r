# --------------------------------------------
# Input: 
# from_idx:
  # A 8 character code indicating the trip. Current codes are as follows:
    # STRS2013 - Stratified random survey 2013
    # STRS2014 - Stratified random survey 2014
    # SKBO2013 - Bowie Seamount 2013
    # SKBO2014 - Bowie Seamount 2014
# from_dir:
  # The location of a folder containing the video files. The video files must be stored in subfolders of the form: SetXXXTrapYYYCameraZZZ, depending on the set, trap and camera number that the video is from.
  # The order of SetXXX, TrapYYY, CameraZZZ in the folder name does not matter
  # Lowercase/uppercase does not matter, the code will convert to lowercase first.
  # Videos can also be stored in subfolders within SetXXXTrapYYYCameraZZZ.
# to_dir:
  # The location to copy the renamed file (currently set as video_dir)
# --------------------------------------------
# Output:
  # The code will copy a video file GOPRdddd.MP4 with trip code STRS2013 (for example) and folder SetXXXTrapYYYCameraZZZ into video_dir with filename: STRS2013_SXXXTYYYCZZZ_GOPRdddd.MP4. 
  # If any one of Set, Trap, Camera is not contained within the folder name, '000' will be used in the place of the digits. As an example, a file GOPRdddd.MP4 stored in SetXXXTrapYYY will be named: STRS2013_SXXXTYYYC000_GOPRdddd.MP4

require(rPython)
i <- 1
for(i in seq(1, length(from_dirs),1 )) {
	setwd(master_dir)
	from_dir = as.character(from_dirs[i])
	from_idx = as.character(from_idxs[i])
	python.load("Video_prepfile.py")
	python.call("Video_prepfile", from_idx, from_dir, video_dir)
}

