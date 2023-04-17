# Network Canvas / HBH Partner Services R Shiny “Data Reader” 

These instructions will allow you to utilize the R Shiny “Data Reader” App to facilitate data entry into CHIMS.

## About 
The app was built by the Northwestern University team. It is designed to run locally on Howard Brown computers so that no external data transmission is necessary. The app works by reading in exported Network Canvas files (where every client has a .zip file associated with them) and restructuring it so that it is easier for Partner Services staff members to enter the data into the CHIMS system.  

If you have questions, please reach out to Caden Buckhalt (buckhalt@northwestern.edu) or study MPIs: Michelle Birkett (birkett@northwestern.edu) & Gregory Phillips II (glp2@northwestern.edu)

## Required Downloads 

1. Download and install R from any of these mirror sites:
    - https://cran.r-project.org/mirrors.html
2. Download and install RStudio:  
    - https://www.rstudio.com/products/rstudio/download/#download
3. Download two script files: 
    - Data Cleaning Script: [r34_cleaning.R](/r34_cleaning.R)
    - R Shiny App: [r34-app.R](/r34-app.R)



## Opening the R Shiny Data Reader 
-	Once R and RStudio are installed, open RStudio.
-	Within RStudio, open both R script files:
    -	File -> Open File -> Select “r34_cleaning.R”
    -	File -> Open File -> Select “r34-app.R”
-	You should now have two tabs at the top, one with each file name (see below) 
-	Select “r34-app.R” tab (see below, highlighted yellow area)
-	Click “Run App” (see below, highlighted blue area) and the data reader will begin to load. 
    -	If this is the first time you are opening the data reader on your machine, you may receive a prompt asking “Do you want to install shiny now?” (see below). 
        -	Select “Yes” 
<img width="468" alt="image" src="https://user-images.githubusercontent.com/75645391/176495142-81494e60-4292-4b21-b617-4fdba09b0468.png">
<img width="152" alt="image" src="https://user-images.githubusercontent.com/75645391/176494708-8c9c1fc7-bf29-4160-894a-102df9174a24.png">



## Using the R Shiny "Data Reader" App to Read Client Data 
-	Ensure that you have the .zip file, the interview period, and the interview period start date ready. 
-	Select “Browse” (yellow highlight)
-	Navigate to where the exported Partner Services interview data is located.
-	Find and select the client output .zip file which you wish to read 
    -	For example: “123456-03-10-2022.zip” 
    -	If successful, a blue bar should now read “Upload Complete”
-	Once data is loaded, utilize either the ‘tabs’ at the top or “Next” and “Previous” buttons to navigate through the question packages
-	Each ‘tab’ is meant to map onto a separate question package within CHIMS.
    -	The ‘Venues’ Tab – maps onto the ‘Venues’ question package in CHIMS
        -	Each venue receives a numeric number. 
        -	Scroll through the numbers to see each venue.
    -	The ‘Sexual Behavior’ Tab – maps onto the ‘Sexual Behaviors’ sections of the ‘Risk Behaviors’  question package in CHIMS.
        -	The dropdown options correspond to the ‘Sexual Behaviors within Interview Period’ and ‘Sexual Behaviors within 12 months’ sections.  
        -	To indicate interview period, enter the Interview Period (months) and Interview Period Start Date. The data will dynamically reflect partners within the interview period. 
    -	The ‘Substance Use’ Tab – maps onto the ‘Substance Use’ section of the ‘Risk Behaviors’  question package in CHIMS.
    -	The ‘Referral Contacts’ Tab – maps onto the ‘Partners’ question package in CHIMS.
       - Each referral contact receives a numeric number
        -	Scroll through the numbers to see each contact

<img width="372" alt="image" src="https://user-images.githubusercontent.com/75645391/176495044-4c12c98e-c17d-4066-a1d1-fe3d6c985c19.png">
<img width="259" alt="image" src="https://user-images.githubusercontent.com/75645391/176495056-ea83a663-9b8e-4f71-a33f-e5faea3f56f5.png">


