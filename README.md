# bioflowPortable

This is a portable version of bioflow for Windows system.

## Installation and Run

1) Download the repository in one of the following ways:

    1A) Click on the green button "Code" and select the option "Download ZIP"
    
    1B) Clone the repository in your computer using git, for example typing in the terminal:
    
        git clone https://github.com/Breeding-Analytics/bioflowPortable.git

2) Unzip the folder in case you use 1A, otherwise move to step 3.

3) Open the folder and look for the file named "runBio.vbs" and double click on it.

**Note:** Some features used in the generation of HTML report do not work using the current pandoc version that comes with this portable version. Unfortunately, the pandoc version cannot be updated because of the file size. As a workaround, kindly download the latest pandoc version from this link: https://github.com/jgm/pandoc/releases?page=1 then copy the pandoc.exe file and then paste it inside the pandocExe folder before running runBio.vbs

4) When the executable starts it will open an R session. Please don't type or close anything, after some seconds you will see the browser opening bioflow.

5) Start using bioflow

## Final comments

When you use bioflowPortable you will be using your own computational resources. Just for you to know in case your computer slows down for multi-trial analysis routines using Big data.

If you encounter any issue please report it to the github repository under the "Issues" tab.
