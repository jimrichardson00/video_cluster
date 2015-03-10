#import packages you need
import os
import os.path
import time
import datetime
import re
import shutil
import fnmatch

def Rename_video( from_idx, from_dir, to_dir ):

  #sets empty lists to be used later for the files and folders in directory
  folders = []
  years = []

  #cycles through folders in directory
  for folder in os.listdir(from_dir):

    #sets the full path of each folder
    folder_path = os.path.abspath(os.path.join(from_dir,folder))
    
    #sets root and extension of each folder
    folderRoot, fileExtension = os.path.splitext(folder_path)
    if fileExtension == '':
      #if file extension is empty (i.e. a folder), adds the file name to list of file names
      folders.append(folder)

  # f = open("/home/jim/Dropbox/Records/Filenames.txt", "r")
  # Filenames_txt = f.read().split('\n')
  # f.close()

  for folder in folders:

    print folder

    # folder path
    folder_path = os.path.abspath(os.path.join(from_dir,folder))

    #sets root and extension of each folder
    folderRoot, fileExtension = os.path.splitext(folder_path)

    # extract set and trap
    # match_ST = re.search(r'Set(\d+)Trap(\d+)', folder)
    match_ST = re.search(r'[a-zA-Z]+(\d+)[a-zA-Z]+(\d+)', folder)

    Set = match_ST.group(1)
    Trap = match_ST.group(2)

    Set = str(Set).zfill(3)
    Trap = str(Trap).zfill(3)

    print Set
    print Trap
    print folder_path

    # ----------------------------------------------------------------------
    # first video in each set/trap

    matches = []

    for root, dirnames, filenames in os.walk(folder_path):

      for filename in fnmatch.filter(filenames, '*.MP4'):

        # extract filename path
        filename_path = os.path.abspath(os.path.join(root, filename))

        # extract filename extension
        fileRoot, fileExtension = os.path.splitext(filename_path)

        print filename
        print fileRoot
        print fileExtension
        print 'from dir' + filename_path

        match_BF = re.search(r'(.+)\.MP4', filename)

        base_filename = match_BF.group(1)

        # copies the file
        shutil.move(filename_path, to_dir)

        # new filename
        NewFilename = from_idx + '_' + 'S' + str(Set) + 'T' + str(Trap) + '_' + str(base_filename) + ".MP4"
        
        print 'new filename ' + NewFilename

        # new filename path
        NewFilename_path = os.path.abspath(os.path.join(to_dir, NewFilename))

        # full path of copied file
        filename_path = os.path.abspath(os.path.join(to_dir, filename))

        # renames the file
        os.rename(filename_path, NewFilename_path)
