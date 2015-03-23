import os
import os.path
import re
import shutil
import fnmatch

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

def Video_prepfile( from_idx, from_dir, to_dir ):

  from_idx = str(from_idx)
  from_dir = str(from_dir)
  to_dir = str(to_dir)

  print from_idx

  #sets empty lists to be used later for the files and folders in directory
  folders = []
  years = []

  #cycles through folders in directory
  for folder in os.listdir(from_dir):

    #sets the full path of each folder
    folder_path = os.path.abspath(os.path.join(from_dir,folder))
    
    #sets root and extension of each folder
    folderRoot, fileExtension = os.path.splitext(folder_path)

    # if file extension is empty (i.e. a folder), adds the file name to list of file names
    if fileExtension == '':
      folders.append(folder)

  for folder in folders:

    # folder path
    folder_path = os.path.abspath(os.path.join(from_dir,folder))

    #sets root and extension of each folder
    folderRoot, fileExtension = os.path.splitext(folder_path)

    # extract set and trap
    # match_ST = re.search(r'Set(\d+)Trap(\d+)', folder)
    match_set = re.search(r'.*set(\d+).*', folder.lower())
    match_trap = re.search(r'.*trap(\d+).*', folder.lower())
    match_camera = re.search(r'.*camera(\d+).*', folder.lower())

    if match_set or match_trap or match_camera:

      print "From folder: " + folder

      if match_set:
        Set = match_set.group(1)
      else:
        Set = 0

      if match_trap:
        Trap = match_trap.group(1)
      else:
        Trap = 0

      if match_camera:
        Camera = match_camera.group(1)
      else:
        Camera = 0

      Set = str(Set).zfill(3)
      Trap = str(Trap).zfill(3)
      Camera = str(Camera).zfill(3)

      # ----------------------------------------------------------------------
      # first video in each set/trap

      matches = []

      for root, dirnames, filenames in os.walk(folder_path):

        for filename in fnmatch.filter(filenames, '*.MP4'):

          # extract filename path
          filename_path = os.path.abspath(os.path.join(root, filename))

          # extract filename extension
          fileRoot, fileExtension = os.path.splitext(filename_path)

          match_BF = re.search(r'(.+)\.MP4', filename)

          base_filename = match_BF.group(1)

          # new filename
          print "Set: " + str(Set)
          print "Trap: " + str(Trap)
          print "Camera: " + str(Camera)
          print "from_idx: " + str(from_idx)

          NewFilename = str(from_idx) + '_' + 'S' + str(Set) + 'T' + str(Trap) + 'C' + str(Camera) + '_' + str(base_filename) + ".MP4"
          NewFilename_path = os.path.abspath(os.path.join(to_dir, NewFilename))

          print "To folder: " + to_dir
          print "New file name: " + NewFilename

          print os.path.isfile(NewFilename_path)

          if os.path.isfile(NewFilename_path) == False:

            # copies the fileVideo_prepfile# shutil.move(filename_path, to_dir)
            shutil.copy(filename_path, to_dir)

            # full path of copied file
            filename_path = os.path.abspath(os.path.join(to_dir, filename))

            # renames the file
            os.rename(filename_path, NewFilename_path)

          print "Copied and renamed."
          print ""
