***************************************************
*Replication materials for paper titled "Are Online and Offline Prices Similar? Evidence from Large Multi-Channel Retailers", published in the American Economic Review
*Author: Alberto Cavallo
*Date: 8/2016
**************************************************

***INSTRUCTIONS

-The main script is called "main_validation.do".  This is the only script that needs to be run to replicate the results in the paper. All other scripts are referenced there and will run atuomatically. Graphs will be created inside the "GRAPHS" folder in PNG format. The data used in all scripts is in the folders "RAWDATA" and "DATA"

-Before running, open main_validation.do and change line 11 (shown below) to match the directory where you stored the files in your computer
"global dir_main "X:\Dropbox (Personal)\PAPERS\Validation\REPLICATION"

-These scripts and data files are for STATA 14 and were tested on a Windows 10 machine. Some user commands used in the scripts may need to be installed using "ssc install" syntax. The code tries to do this automatically but you should check for errors.

-I you want to replicate the color scheme of the figures, please copy the file "DATA\scheme-s2monowhite.scheme" to your STATA folder, subfolder "ado\base\s"

-After the code runs, all the files should take about 50mb of disk space.

***TABLES AND FIGURES

-The latex file TABLES AND FIGURES.tex has "live" references to all these tables and figures. After running main_validation.do, you can compile this latex file to see the results. Note that the table/figure numbers will be different because Table 1 and Figure 1 are not there (not compiled from the data)

These are the names of the tables and figures:
-Table 1: List of retailer names. Not compiled from the replication data.
-Table 2: TABLES/table_country_data_0_ns_frag.tex
-Table 3: TABLES/table_country_highlow_0_ns_frag.tex
-Table 4: TABLES/table_cat_id_highlow_0_ns_frag.tex
-Figure 1: App screenshot. Not compiled from the replication data.
-Figure 2: GRAPHS/hist_0_ns_ALL.pdf
-Table 5: TABLES/table_freq_country_0_ns_frag.tex
-Table 6: TABLES/table_pcnew2_country_0_ns_frag.tex
-Table 7: TABLES/table_offlinenew_highlow___frag.tex
-Figure 3: GRAPHS/histogram_amazon_amonoff_0_ns.pdf
-Table 8: TABLES/table_amazon_paper_0_ns_frag.tex
-Table 9: TABLES/table_overlap_country_0_frag.tex

-The same information can also be found in excel files with the same name, just different extension (.xls instead of _frag.tex). For example, Table 2 is found in "TABLES\table_country_data_0_ns.xls"

***DATA DICTIONARY

-The data files are in the RAWDATA and DATA subdirectories, in Stata 14 format. They are also available in the Harvard Dataverse of the Billion Prices Project: https://dataverse.harvard.edu/dataverse/BPP and the BPP website http:\\bpp.mit.edu\datasets

-The file "RAWDATA\online_offline_ALL_clean.dta" contains the main online-offline matched dataset.

  obs:        45,253
 vars:            22                          26 Aug 2016 15:45
 size:    20,590,115
------------------------------------------------------------------------------------------------------------------------------------------
              storage   display
variable name   type    format     Description
------------------------------------------------------------------------------------------------------------------------------------------

COUNTRY         str12   %12s                  Country name
retailer        float   %9.0g                 Retailer id
retailer_s      str14   %14s                  Retailer id in string format
date            float   %td                   Date for offline data collection, in stata format
day             byte    %9.0g                 Day for offline data collection
month           byte    %9.0g                 Month for offline data collection
year            int     %9.0g                 Year for offline data collection
id              str61   %61s                  Offline product BARCODE
price           double  %10.0g                Offline Price
price_online    double  %10.0g                Online Price
imputed         byte    %9.0g                 =0 if the online price was collected on the exact same day (otherwise it was collected within 7 days)
DEVICEID        str16   %16s                  Worker/Phone id
TIME            str5    %9s                   Time of offline data collection
ZIPCODE         str21   %21s                  Zip code of offline data collection
OTHERSKUITEM    str38   %38s                  Offline product BARCODE (when manually entered in the app)
COMMENTS        str168  %168s                 Comments from offline data collection. Initially, workers used an "s" to identify offline sales.
PRICETYPE       str21   %21s                  Type of Price (Sale or Regular). Not used at the beggining.
CODE            str6    %9s                   Project Code. Identifies different collection stages. For internal use BPP.
sale_online     byte    %12.0g                =1 if there is an online sale captured by the scrape job
country_s       str12   %12s                  Country name in "proper" string format (for tables)
------------------------------------------------------------------------------------------------------------------------------------------

-The file "RAWDATA\amazon_compare.dta" contains the data used for the comparison to Amazon's prices.

 obs:         3,992
 vars:            19                          9 Aug 2016 23:23
 size:    20,534,848
---------------------------------------------------------------------------------

              storage   display
variable name   type    format     Description
---------------------------------------------------------------------------------

date_amazon     float   %td                   Date Amazon Price was collected (STATA format)
date            int     %td                   Date Offline Price collected
id              str1224 %24s                  Product id or Barcode
price           double  %10.0g                Price Offline
price_online    double  %10.0g                Price Online
price_amazon    double  %10.0g                Price Amazon
sale_online     byte    %12.0g                =1 if there is an online sale captured by the scrape job
product_online  str1224 %1224s                Product name from the website of the multi-channel retailer
product_amazon  str1821 %1821s                Product name from Amazon
merchant        str288  %288s                 Merchant information displayed on Amazon
URL             str304  %304s                 URL at Amazon
imputed         byte    %9.0g                 =0 if the online price was collected on the exact same day as the offline price (otherwise within 7 days)
COMMENTS        str168  %168s                 Comments from offline data collection. Initially, workers used an "s" to identify offline sales.
PRICETYPE       str21   %21s                  Type of Price (Sale or Regular). Not used at the beggining.
datediff        float   %9.0g                 Days between the Offline and Amazon data collection (date and date_amazon)
cat_id          int     %10.0g                Category Id. Follows COICOP. See http://unstats.un.org/unsd/cr/registry/regcst.asp?Cl=5
category        str38   %38s                  Category description.
retailer_id     float   %9.0g                 Retailer id
retailer_s      str14   %14s                  Retailer id in string format
---------------------------------------------------------------------------------

-The file "RAWDATA\offline.dta" contains the data used for Section IV-B on "Offline Price Dispersion". The format is the same as the main data file, with these additional variables:
---------------------------------------------------------------------------------

zipcode         str9    %9s                   U.S. Zip code
city            str27   %27s                  City
state           str2    %9s                   State
lat             str6    %9s                   Zip code approximate latitude
lgt             str7    %9s                   Zip code approximate longitude
cat_id          int     %10.0g                Category Id. Follows COICOP. See http://unstats.un.org/unsd/cr/registry/regcst.asp?Cl=5
category        str38   %38s                  Category description.
---------------------------------------------------------------------------------

-The file "RAWDATA\ovelap.dta" contains the results from the product selection "ovelap" in Table 9.

  obs:            46
 vars:             8                          29 Aug 2016 10:33
 size:           690
---------------------------------------------------------------------------------
              storage   display
variable name   type    format     Description
---------------------------------------------------------------------------------
samplesize      int     %10.0g     Sample Size for that particular retailer
qtyfoundbyscr~g int     %10.0g     Qty Found by Scraping
qtymissing      int     %10.0g     Qty Missing
missingsthata~e int     %10.0g     Missings that are online
missingswrong~l byte    %10.0g     Missings Wrong Barcode and Available Online
missingsfaile~g byte    %10.0g     Missings Failed Scraping
missingsonlin~e byte    %10.0g     Missings Online Same Price
retailer        float   %9.0g      Retailer id
---------------------------------------------------------------------------------

-The file "RAWDATA\supermarket-1day-45zips.dta" contains the data scraped from a large supermarket that sells online and asks for a zipcode before showing prices. See the Appendix for details.

 obs:        35,132
 vars:             5                          2 Apr 2013 14:51
 size:       702,640
---------------------------------------------------------------------------------
              storage   display
variable name   type    format     Description
---------------------------------------------------------------------------------
id              long    %8.0g      Product Id
price           float   %8.0g      Price at that location
zip             long    %8.0g      Zip code
prices          float   %9.0g      Distinct prices for that id (computed from id and price)
zips            float   %9.0g      Distinct zipcodes for that id (computed from id and zip)
---------------------------------------------------------------------------------

-The file "DATA\retailer_rules_clean_anon.dta" has the category information for each retailer.
This is a list of variables included:

 obs:            61
 vars:             5                          26 Aug 2016 15:45
 size:         4,270
---------------------------------------------------------------------------------
              storage   display    value
variable name   type    format     label      variable label
---------------------------------------------------------------------------------
retailer        float   %9.0g                 Retailer id
country         str14   %14s                  Country name
country_s       str12   %12s                  Country name in "proper" string format (for tables)
cat_id          int     %10.0g                Retailer category Id. Follows COICOP. 9999 identifies retailers that sell a large mix of products.
category        str38   %38s                  Category description.
---------------------------------------------------------------------------------

-The file "RAWDATA\zipcodes.dta" contains the list of all zip codes in the US.


