# ----------------------------------------
# Output:
# - Loads data_train, creates newdata as video files for which ImageClarity = NA
# - If there are files that have not been validated (ImageClarity = NA), proceed. If not, do nothing.
# - Load each classifier, run classifier on newdata with min cutoff set by P_var, P_val
# - Create lists of Clear, Cloudy videos from output
# - Clears the following folders:
#    (clust_dir)/new/Clear_(classifier_name)_P_val(P_val)
#    (clust_dir)/new/Cloudy_(classifier_name)_P_val(P_val)
#   (deletes and recreates them)
# - Copies each new video frame to following folders: 
#    (clust_dir)/new/Clear_(classifier_name)_P_val(P_val)
#    (clust_dir)/new/Cloudy_(classifier_name)_P_val(P_val)
# according its classification
# ----------------------------------------

# ----------------------------------------
# - Loads data_train, creates newdata as video files for which ImageClarity = NA
load(file = paste("data_train", year, ".RData", sep = ""))


newdata <- data_train[is.na(data_train[, "ImageClarity"]) == TRUE, ]

print(paste("New frames to run ImageClarity classifier on: ", nrow(newdata), sep = ""))
# ----------------------------------------

# ----------------------------------------
# - If there are files that have not been validated (ImageClarity = NA), proceed. If not, do nothing.
if(nrow(newdata) > 0) {

  classifier_name <- "rndf"
  # cycle through classifiers
  for(classifier_name in c("rndf", "arnn", "nbay")) {

		# ----------------------------------------
		# - Load each classifier, run classifier on newdata with min cutoff set by P_var, P_val
    load(paste(classifier_name, "_video", year, "ImageClarity.RData", sep = ""))
    assign("classifier", as.formula(classifier_name))

    Pred_class <- Pred_class(classifier_name = classifier_name,
      classifier = classifier,
      newdata = newdata, 
      Outputs = Outputs,
      P_var = P_var,
      P_val = P_val)
    Pred_class

    # ----------------------------------------
    # - Create lists of Clear, Cloudy videos from output
    Clear_classifier <- newdata$Filenames[Pred_class == "Clear"]
    Cloudy_classifier <- newdata$Filenames[Pred_class == "Cloudy"]

    # ----------------------------------------
    # - Clears the following folders:
    #    (clust_dir)/new/Clear_(classifier_name)_P_val(P_val)
    #    (clust_dir)/new/Cloudy_(classifier_name)_P_val(P_val)
    #   (deletes and recreates them)

    # deletes classification folder, then creates it (aviods overlap)
    system(paste("rm -r ", clust_dir, "/new/Clear_", classifier_name, "_P_val", round(P_val, 2), sep = ""))
    system(paste("mkdir ", clust_dir, "/new/Clear_", classifier_name, "_P_val", round(P_val, 2), sep = ""))

    # deletes classification folder, then creates it (aviods overlap)
    system(paste("rm -r ", clust_dir, "/new/Cloudy_", classifier_name, "_P_val", round(P_val, 2), sep = ""))
    system(paste("mkdir ", clust_dir, "/new/Cloudy_", classifier_name, "_P_val", round(P_val, 2), sep = ""))

    # ----------------------------------------
    # - Copies each new video frame to following folders: 
    #    (clust_dir)/new/Clear_(classifier_name)_P_val(P_val)
    #    (clust_dir)/new/Cloudy_(classifier_name)_P_val(P_val)
    # according its classification

    jpg <- Clear_classifier[1]
    for(jpg in newdata$Filenames) {
      if(jpg %in% Clear_classifier) {
        from_dir = paste(clust_dir, "/new", sep = "")
        dest_dir = paste(clust_dir, "/new/Clear_", classifier_name, "_P_val", round(P_val, 2), sep = "")
        system(paste("cp ", from_dir, "/", jpg, ".jpg", " ", dest_dir, sep = ""))
      } else if(jpg %in% Cloudy_classifier) {
        from_dir = paste(clust_dir, "/new", sep = "")
        dest_dir = paste(clust_dir, "/new/Cloudy_", classifier_name, "_P_val", round(P_val, 2), sep = "")
        system(paste("cp ", from_dir, "/", jpg, ".jpg", " ", dest_dir, sep = ""))
      }
    }
  }
}
