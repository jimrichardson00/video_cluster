# ---------------------------------------------------------------
# overlays frame from each video to see the max non trap area

setwd(master_dir)
python.load("Add_weighted.py")
python.call("Add_weighted", video_dir, master_dir, W, H)

