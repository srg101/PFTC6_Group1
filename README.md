# PFTC6 Group 1

This GitHub repository is for the Group 1 work for the PFTC6 in Aurland, Norway, 2022.
For more information about PFTC see [here](https://plantfunctionaltraitscourses.w.uib.no/).


## Folder sturcture

This repo has a specific folder structure that we recommend as best practice.
The data are kept in separate folders named *raw_data* and *clean_data*, while the code is stored in the folder *code*.
The raw data are the original and untouched files usually .csv or .txt formatted files.
The clean data is often produced from the raw data using code and stored automatically.

In addition, there is a .gitignore text file, which tells git all the files that should be ignored.
This file can be edited and adapted to the users needs.

We also recommend to use a user licence.


## Data documentation

We strongly recommend to document the data, which is best practice.
The *code* folder contains a subfolder *data_dic* with files scripts to produce a data dictionary.

The *data_description.xlsx* file contains basic information about each variable: description, unit and how it was collected.
This file has to be done manually (until we find a solution to automate it).

The R file *make_data_dictionary.R* contains a function that makes the data dictionary from the data file and the description file.

The *data_dic.R* file contains example code to make the data dictionary for each data set and to compile it to one file in the end.
Here each dataset needs to be entered.

The *download_clean_data.R* shows and example for how to download the clean data if it is stored externally on a data repository, for example on OSF.
