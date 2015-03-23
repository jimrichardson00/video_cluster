#import packages you need
import os
import os.path
import time
import datetime
import re
import shutil
import fnmatch

def Rename_video( from_idx, from_dir, to_dir ):

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

            # copies the file
            # shutil.move(filename_path, to_dir)
            shutil.copy(filename_path, to_dir)

            # full path of copied file
            filename_path = os.path.abspath(os.path.join(to_dir, filename))

            # renames the file
            os.rename(filename_path, NewFilename_path)

          print "Copied and renamed."
          print ""
