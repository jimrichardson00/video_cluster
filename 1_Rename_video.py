#import packages you need
import os
import os.path
import time
import datetime
import re
import shutil

#sets to and from directories
from_dir = '/home/jim/Desktop/DFO Survey - October 2013 - copy/october 2013 video'
to_dir = '/home/jim/Desktop/pca_video/video'

def rename_video( from_dir, to_dir ):

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
      print folder

  # f = open("/home/jim/Dropbox/Records/Filenames.txt", "r")
  # Filenames_txt = f.read().split('\n')
  # f.close()

  for folder in folders:

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

    # ----------------------------------------------------------------------
    # first video in each set/trap

    filenames = os.listdir(folder_path)
    filenames.sort()
    filename = filenames[0]

    # extract filename path
    filename_path = os.path.abspath(os.path.join(folder_path, filename))
    # extract filename extension
    fileRoot, fileExtension = os.path.splitext(filename_path)
    print filename
    if fileExtension == '.MP4':

      # copies the file
      shutil.copy(filename_path, to_dir)

      # new filename
      NewFilename = 'DFO2013' + '_' + 'S' + str(Set) + 'T' + str(Trap) + '_' + str(filename)
      print NewFilename

      # new filename path
      NewFilename_path = os.path.abspath(os.path.join(to_dir, NewFilename))

      # full path of copied file
      filename_path = os.path.abspath(os.path.join(to_dir, filename))

      # renames the file
      os.rename(filename_path, NewFilename_path)

      # ----------------------------------------------------------------
      # all videos in set/trap

    # # may need to use whole path for folder
    # for filename in filenames:

    #   # extract filename path
    #   filename_path = os.path.abspath(os.path.join(folder_path, filename))

    #   # extract filename extension
    #   fileRoot, fileExtension = os.path.splitext(filename_path)

    #   print filename

    #   if fileExtension == '.MP4':

    #   # copies the file
    #   shutil.copy(filename_path, to_dir)

    #   # new filename
    #   NewFilename = 'DFO2013' + '_' + 'S' + str(Set) + 'T' + str(Trap) + '_' + str(filename)
    #   print NewFilename

    #   # new filename path
    #   NewFilename_path = os.path.abspath(os.path.join(to_dir, NewFilename))

    #   # full path of copied file
    #   filename_path = os.path.abspath(os.path.join(to_dir, filename))

    #   # renames the file
    #   os.rename(filename_path, NewFilename_path)

def main():

  if len(sys.argv) != 3:
    print 'usage: python 1_Rename_video.py from_dir to_dir'
    sys.exit(1)

  from_dir = sys.argv[1]
  to_dir = sys.argv[2]

  rename_video( from_dir, to_dir )
  
  sys.exit(1)

if __name__ == '__main__':
  main()