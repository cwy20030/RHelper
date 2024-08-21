Revision history for the R/RHelper package
-----------------------------------------

## Version 1.9.2, 2024-08-21

- Minor update on the pakcage:

- Debug internal RHSetting update

## Version 1.9.1, 2024-08-05

- Added Parity() to check the parity of a number (even vs. odd) and a small sub-function within to ensure the number's parity

## Version 1.9.0, 2024-07-12

- Major update on the package:

- Added SandBox() to allow testing code in a "ghost" environment
- Update Lexicographer() to allow file check

- Name Change for  following functions
    - Clear_History() --> Historian()
    - Auto_Importer() --> Importer()
    - Auto_Bulk_Merger() --> Merger()

- Added version control for data and documentaion

- Now containing Butler(), CRA(), Clerk(), Historian(), Importer(), Lexicographer(), Librarian(), Merger(), Name_Checker(), Naming_Variables(), RowBind(), SandBox(), Who_is()


## Version 1.8.0, 2024-05-22

- Major update on all functions and work flow. 

- Added RowBind() to bind data.frame by row without having the same column names.

- Added Lexicographer() to document data dictionary.

- Added Clerk() to list out file and folder structure of a directory.


## Version 1.7.0, 2024-04-05

- Added Butler() to help clean up and maintain Global Environment

- General updates and debugging of all functions.


## Version 1.2.0 - 1.6.0, 2023

- Remove Pattern_Search(), Rank_Prtl()

- Update Auto_Importer() to allow reading of .tst 



## Version 1.1.0, 2022-12-30

- Added CRA() to check and install missing dependencies for packages

- Modify Pattern_Search() algorithm


## Version 1.0.0, 2022-12-01

- Initial Deployment of the Package on GitHub

- The package contaisn Pattern_Search(), Rank_Prtl(), Auto_Bulk_Merger(), Auto_Importer(), Col_Name_Checker(), DF_Name_Checker()



## Version 0.3, 2020

- Added Auto_Bulk_Merger() to merge data.frames 

- Remove Outlier_Remover()


## Version 0.2.0, 2019

- Added several functions
- Outlier_Remover() for removing outliers using Fischer's IQR.
- read_excel_allsheets() to read excel files with multiple tabs
- Auto_Importer() to read .txt, .csv, .xls(x), .sav in bulk
- Who_is() to list out class-specific items stored in Global Environment 


## Version 0.1.0, 2018-09-21

- This is a package to contain helpful R functions for my PhD project

- Currently, Naming_Variables(), Clear_History()
