# Civic Digital Fellowship 2018  

This is the public part of the work I did as a [Civic Digital Fellow](https://www.codingitforward.com/summer-fellowships) at the [National Center for Health Statistics](https://www.cdc.gov/nchs/index.htm), a federal statistical agency part of the [Center for Disease Controls (CDC)](https://www.cdc.gov/) 

Here, I played a role in prototyping how [Health, United States](https://www.cdc.gov/nchs/hus/index.htm), an
    
 annual printed report, could be digitized and visualized. After wrangling the data from Table 63 in [Health, United States 2016](https://www.cdc.gov/nchs/data/hus/hus16.pdf) from a excel sheet that was human readable into a format that was machine readable (not shown), I developed an RShiny app that visualized the data and meets federal accessibility requirements. 

For the presentation I gave on this work at Coding it Forward's Demo Day, click [here](https://github.com/codingitforward/cdfdemoday2018/blob/master/Restructuring%20Health%2C%20United%20States%20%26%20Analyzing%20Opioid%20Deaths.pdf). (The other part of my work that summer involved working with classified data on opioid deaths.) I also gave a version of this presentation internally to the National Center for Health Statistics. 

## Viewing the application

An online version of the app can be found [here](https://afan.shinyapps.io/Table63take3/)

## Files
- table063.csv
  - Data from Table 63 from Health, United States 2016, wrangled into a machine readable format. 
  - [Click here to download the 2017 version of the raw data in the spreadsheet](https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/Health_US/hus17tables/table063.xlsx)
- app.R
  - Code for the RShiny application 
- README.md
