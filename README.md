# BRS-Seq-R
Batch process baroreflex sensitivity analysis using the sequence method

Running the script:

use command to get the most updated version:

```R
source("https://github.com/nkcheung95/BRS-Seq-R/blob/main/BRS%20v2.0.r?raw=TRUE")
```


BRS-R v2.0

**Batch process baroreflex sensitivity analysis using the sequence method**
# **Contents**
[File Preparation](#_toc140223748)

[Folder Preparation](#_toc140223749)

[Running the program](#_toc140223750)

[Output Files](#_toc140223751)



The BRS-R program is designed to utilize beat-by-beat physiological data using R-R interval (RRI) and systolic blood pressure (SBP). The program requires the download of R ([link](https://mirror.rcg.sfu.ca/mirror/CRAN/)) and RStudio ([link](https://posit.co/downloads/)) 
# <a name="_toc140223748"></a>**File Preparation:**
Because the program processes files as a batch, many files can be completed at once, if files are prepared correctly.

The program will accept only **.csv** files

These files should be named with participant and condition ID – the filename determines the output folder name after BRS processing

See sample folder for sample data

Files must have only 2 columns, RRI in seconds, and SBP in mmHg. **COLUMNS MUST BE LABELLED RRI AND SBP**.  Naming is important as the program requires the columns to be exact matches – check for spaces at the end of column names.

RRI and SBP data can be pulled using beat by beat identification in labchart and copied from the datapad. Minimum 5 minutes of data is recommended for adequate sequence count.
# <a name="_toc140223749"></a>**Folder Preparation:**
Begin setting up for analysis by creating a project in RStudio (File > New Directory > New Project).

In the new project console, copy and paste the following code:

```R
source("https://github.com/nkcheung95/BRS-Seq-R/blob/main/BRS%20v2.0.r?raw=TRUE")
```



The console in the bottom left will begin running the script. Let the program run until it is completed – The console message will say “All BRS Analyzed” when complete. 

The program will install any required dependencies and create your base file structure inside your original working folder. The working folder will now contain the “data\_csv” folder.

Your folder is now ready for batch processing your BRS files. Place all prepared csv files into the data\_csv folder.
# <a name="_toc140223750"></a>**Running the program:**
After placing all files you would like processed into data\_csv, the program can now be run again as described in the previous section. The console in the bottom left will begin running the script. Let the program run until it is completed. It will take longer this time but be patient – The console message will say “All BRS Analyzed” when complete. A progress bar will appear after each file if you are batch processing files, so do not interrupt the script unless you want to end processing early.

<a name="_toc140223751"></a>Output Files:

Once the processing is complete, your working folder will have a new “**export**” folder. The results from the program are stored here.

Each file from the data\_csv folder will now have its own export folder.

Exported files include:

- BRS results.csv 
  - This file contains the summary info that has been filtered. Mean slope, Slope SD, and sequence count for lag 0,1,2 will be in this file. Outlier sequence slopes that do not meet criteria (R<sup>2</sup><0.85) have been removed from these values.
- lag0\_slope
  - List of the sequences identified using lag 0 with their slopes and r<sup>2</sup> values
- lag0\_data
  - All of the beats identified as part of a sequence using lag 0
- lag1\_slope
- lag1\_data
- lag2\_slope
- lag2\_data

In addition to these files, there will be a “plots” folder in the export for each file. 

These plots are visualizations of every identified sequence, and are not necessary for data extraction, but can be helpful if troubleshooting a particular file. The plots are associated with the slopes in the filtered lagX\_slope.csv files, blue indicates increasing sequences, red indicates decreasing sequences

Future Updates:
Sigmoid baroreflex curve plot
plot label fix (currently not working)



