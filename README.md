# Cesarean Section Rates

The purpose of this project is to explore the different factors that correlate with cesarean section rate, 
focusing primarily on geography, age, and ethnicity of the mother. 

### Source Data
All of my data are from the [Wonder data system](https://wonder.cdc.gov/natality.html) available from the CDC, specifically for natality data. 
All data were queried for only single births between 2012 and 2015 that were in hospitals, and not premature 
(37-41 weeks gestational age according to both methods). 

Counties with population below 100,000 are grouped in 'Unknown County' by state. 

Age and ethnic categories with fewer than 20 births were excluded. 

### Results

![map](https://github.com/smitsrr/delivery_method/blob/master/cesarean_rate_map.png "cesarean rate map")
Tidy data available in `county_data_with_rates.csv`

![age_ethnicity](https://github.com/smitsrr/delivery_method/blob/master/cesarean_rate_age_eth.png "cesarean rates by age and ethnicity of mom")

For each age category and ehtnicity of mom, we show the distribution of cesearn rates across counties. The middle horizontal line is the median rate, 
while the upper and lower limits of the box are the first and third quartiles (25th and 75th percentiles). The vertical whiskers extend to 1.5*IQR, 
with individual points beyond the whiskers plotted individually as "outliers".  Tidy data available in `mother_demographics.csv`

### Notes

These analyses are correlational only and are not meant to imply causation. 
