Please read the presentation FinalPresentation_HardDrive.pptx and report FinalReport_HardDrive.html (please download and open in browser) for details.

xxxxxxxxxxxx

Since importing data is time consuming, two files are created: one for importing data (harddrive_readdata.R), and another for processing data (harddrive_quickloaddata.R).

6 major functions are currently programmed:
1. Create data frame of total number and number of failed hard drive of each day with date information, namely: date, total number of HD, total number of failed HD. Then the time series are plotted.
2. Create data frame of total number and number of failed hard drive of different BRAND of hard drive each day with date information, namely: date, BRAND, total number of HD, total number of failed HD. Then the time series are plotted.
3. Create data frame of total number and number of failed hard drive of different SIZE of hard drive each day with date information, namely: date, SIZE, total number of HD, total number of failed HD. Then the time series are plotted.
4. Create data frame for performing survival analysis of full set of hard drive, namely: serial number, failure (binary).
5. Create data frame for performing survival analysis of different BRANDS of hard drive, namely: serial number, failure (binary), binary details of each of the BRANDS.
6. Create data frame for performing survival analysis of different SIZES of hard drive, namely: serial number, failure (binary), binary details of each of the SIZES.

Different function can be selected within the scripts.

Different year of 2013, 2014 and 2015 can be selected.

Note: creation of each of the RData file can take up to 20 mins for each function of different year. Progress bar is available to know the status of the file creation.