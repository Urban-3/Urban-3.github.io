
## Introduction

### Price Related Differential

IAAO definitation: Price-related differential. The mean (sales ratio)
divided by the weighted mean (sales ratio). The statistic has a slight
bias upward. Price-related differentials above 1.03 tend to indicate
assessment regressivity; price-related differentials below 0.98 tend to
indicate assessment progressivity

Fair & Equitable 2011:

  - “There is a bias in the measure such that PRD in the general range
    of 0.9 and 1.1 are inconclusive indicator of assessment regressivity
    (IAAO, 1980, 14)”
  - “The 1990 Standard on Ratio Studies (IAAO 1990, 24-25) set forth a
    standard for PRD of 0.98 to 1.03”
  - “The price-related differential (PRD) and weighted mean are
    sensitive to sales with high prices even if the ratios on higher
    priced sales do not appear unusual relative to other sales”

At best, PRD is ’an easy to calculate measure that provides “some notion
of any association” between assessment ratios and property value.

### Coefficient of Dispersion

Less than 15 for residential properties

## Calculating Indicators of Vertical Equity

``` r
appraisal_history2019$totalvalue<-appraisal_history2019$LandVal+appraisal_history2019$BldgVal+appraisal_history2019$ImprVal ##Calculate total taxable value
appraisal_history2019_abbr<-appraisal_history2019 %>%
  distinct(ï..PIN,TaxYear, .keep_all = TRUE) %>% #The data set includes duplicated records. Remove duplicates.
  mutate(PIN=ï..PIN, #recode variable names
         year=TaxYear,
         landvalue=LandVal)%>%
  select(PIN, year, landvalue, totalvalue)
appraisal_history2019_abbr$PIN_YEAR=paste(appraisal_history2019_abbr$PIN, appraisal_history2019_abbr$year, sep="_")

#Extract the year from the data field
sales_year<-as.data.frame(sapply(strsplit(sales_data_table$SellDate, "-"), `[`,1))
names(sales_year)<-c("SALES_YEAR")
sales_data_table<-cbind(sales_data_table, sales_year)
sales_data_table$PIN<-str_replace_all(sales_data_table$PINN, "-", "")
sales_data_table$SALES_YEAR<-as.numeric(sales_data_table$SALES_YEAR)

#Extract the most recent sale record
sales_data_table_year<-sales_data_table %>% 
  filter(QualifiedSale=='Y')  %>% #Select qualified sales only
  filter(SALES_YEAR>2000)%>% #Filter sales only after 2000, since we only have appraisal values back to 2001
  filter(VacantLot=="False")%>%
  filter(SellingPrice>10000)%>%
  mutate(SALES_YEAR_1=(SALES_YEAR+1))%>% #Create a new date field that allows the sales to align with the following tax year
  select(PIN, SellingPrice, City, Subdivision, Class, SALES_YEAR, SALES_YEAR_1)
sales_data_table_year$PIN_YEAR=paste(sales_data_table_year$PIN, sales_data_table_year$SALES_YEAR_1, sep="_")

sales_appraised<-merge(appraisal_history2019_abbr, sales_data_table_year, by="PIN_YEAR")
sales_appraised$sales_ratio<-sales_appraised$totalvalue/sales_appraised$SellingPrice

sales_ratio<-sales_appraised%>%
  filter(Class=="RES")%>%
  group_by(SALES_YEAR)%>%
  mutate(selldecile = ntile(SellingPrice, 10),
         mean_ratio = mean(sales_ratio),
         median_ratio = median(sales_ratio),
         weighted_mean = sum(totalvalue)/sum(SellingPrice),
         ratio_std = sd(sales_ratio),
         index = 1,
         count = sum(index),
         median_diff=(sales_ratio-median_ratio),
         coeff_dispersion = (100/count)*(sum(abs(median_diff))/median_ratio),
         coeff_variation = ((100*ratio_std)/mean_ratio),
         price_rltd_diff = (mean_ratio/weighted_mean),
         Year = first(SALES_YEAR))

sales_ratio_summary<-sales_ratio%>%
  group_by(Year)%>%
  summarise(`Mean Ratio` = first(mean_ratio),
            `Meadian Ratio` = first(median_ratio),
            `Coeff. of Dispersion` = first(coeff_dispersion),
            `Coeff. of Variation` = first(coeff_variation),
            `Price Related Differential` = first(price_rltd_diff),
            `Transactions`=sum(index))
```

## Table 1

<!--html_preserve-->

<div id="yryfqgpqfq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yryfqgpqfq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yryfqgpqfq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yryfqgpqfq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yryfqgpqfq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yryfqgpqfq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yryfqgpqfq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yryfqgpqfq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yryfqgpqfq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yryfqgpqfq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yryfqgpqfq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yryfqgpqfq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yryfqgpqfq .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#yryfqgpqfq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yryfqgpqfq .gt_from_md > :first-child {
  margin-top: 0;
}

#yryfqgpqfq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yryfqgpqfq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yryfqgpqfq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#yryfqgpqfq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yryfqgpqfq .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#yryfqgpqfq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yryfqgpqfq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yryfqgpqfq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yryfqgpqfq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yryfqgpqfq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yryfqgpqfq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#yryfqgpqfq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yryfqgpqfq .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#yryfqgpqfq .gt_left {
  text-align: left;
}

#yryfqgpqfq .gt_center {
  text-align: center;
}

#yryfqgpqfq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yryfqgpqfq .gt_font_normal {
  font-weight: normal;
}

#yryfqgpqfq .gt_font_bold {
  font-weight: bold;
}

#yryfqgpqfq .gt_font_italic {
  font-style: italic;
}

#yryfqgpqfq .gt_super {
  font-size: 65%;
}

#yryfqgpqfq .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Indicators of Vertical Equity

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

Buncombe County, North Carolina

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Year

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Mean Ratio

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Meadian Ratio

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Coeff. of Dispersion

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Coeff. of Variation

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Price Related Differential

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Transactions

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_right">

2001

</td>

<td class="gt_row gt_right">

0.98

</td>

<td class="gt_row gt_right">

0.98

</td>

<td class="gt_row gt_right">

8.65

</td>

<td class="gt_row gt_right">

20.70

</td>

<td class="gt_row gt_right">

1.02

</td>

<td class="gt_row gt_right">

2364

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2002

</td>

<td class="gt_row gt_right">

0.95

</td>

<td class="gt_row gt_right">

0.94

</td>

<td class="gt_row gt_right">

11.93

</td>

<td class="gt_row gt_right">

34.22

</td>

<td class="gt_row gt_right">

1.03

</td>

<td class="gt_row gt_right">

2697

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2003

</td>

<td class="gt_row gt_right">

0.90

</td>

<td class="gt_row gt_right">

0.89

</td>

<td class="gt_row gt_right">

12.98

</td>

<td class="gt_row gt_right">

24.37

</td>

<td class="gt_row gt_right">

1.02

</td>

<td class="gt_row gt_right">

2981

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2004

</td>

<td class="gt_row gt_right">

0.84

</td>

<td class="gt_row gt_right">

0.83

</td>

<td class="gt_row gt_right">

14.86

</td>

<td class="gt_row gt_right">

30.95

</td>

<td class="gt_row gt_right">

1.03

</td>

<td class="gt_row gt_right">

3513

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2005

</td>

<td class="gt_row gt_right">

0.99

</td>

<td class="gt_row gt_right">

0.99

</td>

<td class="gt_row gt_right">

10.00

</td>

<td class="gt_row gt_right">

21.86

</td>

<td class="gt_row gt_right">

1.02

</td>

<td class="gt_row gt_right">

3782

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2006

</td>

<td class="gt_row gt_right">

0.90

</td>

<td class="gt_row gt_right">

0.89

</td>

<td class="gt_row gt_right">

15.18

</td>

<td class="gt_row gt_right">

35.16

</td>

<td class="gt_row gt_right">

1.03

</td>

<td class="gt_row gt_right">

3952

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2007

</td>

<td class="gt_row gt_right">

0.85

</td>

<td class="gt_row gt_right">

0.83

</td>

<td class="gt_row gt_right">

14.79

</td>

<td class="gt_row gt_right">

32.25

</td>

<td class="gt_row gt_right">

1.03

</td>

<td class="gt_row gt_right">

3001

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2008

</td>

<td class="gt_row gt_right">

0.87

</td>

<td class="gt_row gt_right">

0.84

</td>

<td class="gt_row gt_right">

15.71

</td>

<td class="gt_row gt_right">

35.27

</td>

<td class="gt_row gt_right">

1.03

</td>

<td class="gt_row gt_right">

2059

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2009

</td>

<td class="gt_row gt_right">

0.90

</td>

<td class="gt_row gt_right">

0.89

</td>

<td class="gt_row gt_right">

14.35

</td>

<td class="gt_row gt_right">

22.78

</td>

<td class="gt_row gt_right">

1.00

</td>

<td class="gt_row gt_right">

1524

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2010

</td>

<td class="gt_row gt_right">

0.94

</td>

<td class="gt_row gt_right">

0.92

</td>

<td class="gt_row gt_right">

14.51

</td>

<td class="gt_row gt_right">

19.18

</td>

<td class="gt_row gt_right">

1.00

</td>

<td class="gt_row gt_right">

1426

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2011

</td>

<td class="gt_row gt_right">

0.98

</td>

<td class="gt_row gt_right">

0.97

</td>

<td class="gt_row gt_right">

14.06

</td>

<td class="gt_row gt_right">

19.84

</td>

<td class="gt_row gt_right">

1.01

</td>

<td class="gt_row gt_right">

1472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2012

</td>

<td class="gt_row gt_right">

0.95

</td>

<td class="gt_row gt_right">

0.96

</td>

<td class="gt_row gt_right">

8.97

</td>

<td class="gt_row gt_right">

15.30

</td>

<td class="gt_row gt_right">

1.01

</td>

<td class="gt_row gt_right">

1820

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2013

</td>

<td class="gt_row gt_right">

0.92

</td>

<td class="gt_row gt_right">

0.92

</td>

<td class="gt_row gt_right">

10.87

</td>

<td class="gt_row gt_right">

16.89

</td>

<td class="gt_row gt_right">

1.02

</td>

<td class="gt_row gt_right">

2401

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2014

</td>

<td class="gt_row gt_right">

0.89

</td>

<td class="gt_row gt_right">

0.88

</td>

<td class="gt_row gt_right">

12.08

</td>

<td class="gt_row gt_right">

16.97

</td>

<td class="gt_row gt_right">

1.02

</td>

<td class="gt_row gt_right">

2595

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2015

</td>

<td class="gt_row gt_right">

0.85

</td>

<td class="gt_row gt_right">

0.84

</td>

<td class="gt_row gt_right">

13.24

</td>

<td class="gt_row gt_right">

18.70

</td>

<td class="gt_row gt_right">

1.01

</td>

<td class="gt_row gt_right">

3239

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2016

</td>

<td class="gt_row gt_right">

0.98

</td>

<td class="gt_row gt_right">

0.98

</td>

<td class="gt_row gt_right">

8.16

</td>

<td class="gt_row gt_right">

12.90

</td>

<td class="gt_row gt_right">

1.01

</td>

<td class="gt_row gt_right">

3578

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2017

</td>

<td class="gt_row gt_right">

0.91

</td>

<td class="gt_row gt_right">

0.92

</td>

<td class="gt_row gt_right">

10.69

</td>

<td class="gt_row gt_right">

14.93

</td>

<td class="gt_row gt_right">

1.01

</td>

<td class="gt_row gt_right">

3391

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2018

</td>

<td class="gt_row gt_right">

0.87

</td>

<td class="gt_row gt_right">

0.87

</td>

<td class="gt_row gt_right">

10.79

</td>

<td class="gt_row gt_right">

14.23

</td>

<td class="gt_row gt_right">

1.00

</td>

<td class="gt_row gt_right">

2962

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

\`\`\`
