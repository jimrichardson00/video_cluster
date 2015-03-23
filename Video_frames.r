# Output:
  # - Code checks video_dir, and frame_dir.
  # - Subset to .MP4 files that are stored in video_dir, but not stored as .jpg files in frame_dir.
  # - For these files, code will calculate an 'average' frame consisting of all frames, past a certain time, blended together. (The number of seconds to skip is dictated by the variable 'skip' which can be set in Video_master.r, currently skip is set at: skip = 6 seconds)

setwd(master_dir)

require(stringr)
if(length(list.files(video_dir, pattern = ".MP4")) > 0) {
  video_files = as.vector(na.omit(str_match(sort(list.files(video_dir)), "(.+)\\..+4")[, 2]))
} else {
  video_files <- vector()
}

require(stringr)
if(length(list.files(frame_dir, pattern = ".jpg")) > 0) {
  video_files_cur = as.vector(na.omit(str_match(sort(list.files(frame_dir)), "(.+)\\.jpg")[, 2]))
} else {
  video_files_cur <- vector()
}

video_files_new <- video_files[!(video_files %in% video_files_cur)]
video_files_new

print(video_files_new)

Mean_frame <- function(video_file, video_dir, frame_dir, skip, Hgp, Wgp) {

  setwd(master_dir)
  
  print(video_file)
  
  require(rPython)
  python.assign("video_file", video_file)
  python.assign("video_dir", video_dir)
  python.assign("frame_dir", frame_dir)
  python.assign("skip", skip)
  python.assign("Hgp", Hgp)
  python.assign("Wgp", Wgp)
  python.assign("data", 0)
  python.load("Remove_trap.py")
  python.load("Mean_frame.py")
  tryCatch({
    python.exec("
      os.chdir(video_dir)
      frame_o = Mean_frame( video_file + '.MP4', video_dir, skip, data, Hgp, Wgp)
      os.chdir(frame_dir)
      cv2.imwrite(video_file + '.jpg', frame_o)
    ")
    },
    error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
    )
}

if(mac == TRUE) {
  lapply(video_files_new, 
  FUN = function(video_file) Mean_frame(video_file = video_file
    , video_dir = video_dir, frame_dir = frame_dir, skip = skip, Hgp = Hgp, Wgp = Wgp)
  )
  } else {
  mclapply(video_files_new, 
    FUN = function(video_file) Mean_frame(video_file = video_file
      , video_dir = video_dir, frame_dir = frame_dir, skip = skip, Hgp = Hgp, Wgp = Wgp)
    , mc.cores = n_cores
    , mc.silent = FALSE
    , mc.preschedule = TRUE
    )
  }

