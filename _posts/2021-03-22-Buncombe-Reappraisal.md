Buncombe Reappraisal
================

## Step 1: Retrieve Data

This project uses data accessible from the Buncombe County Open Data
Explorer via GeoJSON API. The following data tables were retrieved using
the R Code below:

  - **Buncombe County Property Sales Data**: At the time the data were
    retrieved, this table has 309,144 records with attributes on the
    sales status, amount, and grantor/ grantee for all real property
    sales in County
  - **All Property Bills from 2017**: 137,940 records with attributes on
    the ownership, values, amounts, and billing information for all real
    property tax bills in 2017
  - **Buncombe County Parcel Snapshot from 2017**: 124,117 records with
    attributes on the ownership, value and geography of all parcels in
    2017
  - **Real Estate Appraisal Residential Building 2017**: 110,101 records
    with attributes on the size, value, and status of residential
    buildings
  - **Current Parcel Snapshot**: 129,347 records with attributes on the
    ownership, value and geography of all parcels in 2021

<!-- end list -->

``` r
sales_data<-fromJSON("https://opendata.arcgis.com/datasets/f4927ecad72b46b1b3ff5e27e45889e1_0.geojson", simplifyVector = TRUE)
taxbill2017<-fromJSON("https://opendata.arcgis.com/datasets/e7aecb56d3524f1bb3685b811c04c7ad_0.geojson", simplifyVector = TRUE)
parcel_snapshot_2017<-fromJSON("https://opendata.arcgis.com/datasets/b4a38f6d807247f8bedf16ae2989915f_9.geojson",simplifyVector = TRUE)
residential_2017<-fromJSON("https://opendata.arcgis.com/datasets/08aaa5796f41479a8f805ff7291dafa2_0.geojson", simplifyVector = TRUE)
parcel_snapshot_current<-fromJSON("https://opendata.arcgis.com/datasets/969e8c8088a34464961d227d8b1c3f38_1.geojson", simplifyVector = TRUE)
```

## Step 2: Clean and Compile the Data

### Real Property Sales Data

This data table maintains records on the sale of each real property
transacted the county. If a property has been sold/ changed ownership
more than once, multiple records for the same property appear in the
table. For this project, we are primarily interested in the most recent
sales information for any given property. This data table includes a
sequential ???Sell Number??? field. We use this field to select the record
with the highest absolute Sell Number value (i.e.??the most recent).
Alternatively, it would be possible to select the record with the most
recent transaction date. We also select qualified sales only (other
transactions, including exchanges among family, do not reflect market
conditions and are therefore excluded)

``` r
#Extract the year from the data field
sales_year<-as.data.frame(sapply(strsplit(sales_data_table$SellDate, "-"), `[`,1))
names(sales_year)<-c("SALES_YEAR")
sales_data_table<-cbind(sales_data_table, sales_year)
sales_data_table$PIN<-str_replace_all(sales_data_table$PINN, "-", "")
sales_data_table$SALES_YEAR<-as.numeric(sales_data_table$SALES_YEAR)

#Extract the most recent sale record
sales_data_table_year<-sales_data_table %>% 
  filter(QualifiedSale=='Y')  %>% #Select qualified sales only
  group_by(PINN) %>% #Group by parcel ID
  top_n(1, abs(SellNo)) #Select the parcel ID with the highest sequential sell number
```

### Residential Features 2017 Data Table

This table maintains records on the square footage, building materials,
unique features, age, condition and value of residential buildings in
Buncombe County. Each record in the table is a unique building. If a
parcel has multiple buildings on the lot, then each building receives
its own record and the parcel ID is duplicated. Therefore, we aggregate
the data by Parcel ID to determine a sum total for each of the
residential building characteristics, including finished sq ft, year
built, and number of bedrooms and bathrooms.

``` r
library(dplyr)
names(residential2017_table)
residential2017_table$BldgValue<-as.numeric(residential2017_table$BldgValue)
residential2017_table_dup<-residential2017_table %>%
  select(PIN,BldgNo,Style,SqFeet,FinishedSqFt,YearBuilt,Grade,Condition,Foundation,RoofType,Hvac,FullBath,
         HalfBath,AdditionalPlumbingFixtures,Fireplace,SpecialFeature,Bedroom,BasementGarageDoors,
         IndoorPool,ExtraKitchens,Elevator,BldgValue) %>%
  distinct(PIN, BldgNo, BldgValue, .keep_all = TRUE)

residential2017_table_dissolve1<-residential2017_table_dup %>%
  group_by(PIN) %>% #Group by Parcel ID
  summarize(SqFeet = sum(SqFeet), #Sum building features
            FinishedSqFt = sum(FinishedSqFt),
            FullBath=sum(FullBath),
            HalfBath=sum(HalfBath),
            Bedroom=sum(Bedroom),
            BldgValue=sum(BldgValue))

residential2017_table_dissolve2<-residential2017_table_dup %>%
            group_by(PIN) %>%
            top_n(1, abs(FinishedSqFt)) %>% #Select the features associated with the principal building
            select(PIN, YearBuilt, Grade, Condition)
residential2017_table_dissolve2$dup<-duplicated(residential2017_table_dissolve2$PIN)

residential2017_table_dissolve2<-residential2017_table_dissolve2 %>%
  filter(dup=='FALSE')

residential2017_table_dissolve<-merge(residential2017_table_dissolve1, residential2017_table_dissolve2, by="PIN")   #Merge together building features         
residential2017_table_dissolve$bldval_sqft<-residential2017_table_dissolve$BldgValue/residential2017_table_dissolve$SqFeet #Calculate building value per total building sq ft
residential2017_table_dissolve$bldval_finishsqft<-residential2017_table_dissolve$BldgValue/residential2017_table_dissolve$FinishedSqFt #Calculate building value per finished building sq ft

##Merge Sales and Residential 2017
residential2017_sales<-merge(parcelsnapshot2017_table, sales_data_table_year, by="PIN", all.x=TRUE)
length(unique(residential2017_sales$PINN))
names(residential2017_sales)

##Add residential features fields
residential2017_sales_features<-merge(residential2017_sales, residential2017_table_dissolve, by="PIN", all.x=TRUE)
length(unique(residential2017_sales_features$OBJECTID))
names(residential2017_sales_features)
```

### Tax Bill data for 2017

This table maintains records for the total real property (and personal
property) tax bills for all parcels within the County. For this
analysis, we only select records for real property. The total tax bill
amount reflects all taxes levied by the various taxing authorities
throughout the County, including the County, municipalities, and school
district.

``` r
##Add Tax bill fields
taxbill2017_table$PIN<-str_replace_all(taxbill2017_table$pin, "-", "")
taxbill2017_table$dup<-duplicated(taxbill2017_table$PIN)

taxbill2017_table_abbr<-taxbill2017_table %>%
  filter(real_value>0 | personal_value==0) %>%
  filter(PIN !="") %>%
  filter(dup == 'FALSE') %>%
  select(PIN, tax_due)
residential2017_sales_bill<-merge(residential2017_sales_features, taxbill2017_table_abbr, by="PIN", all.x=TRUE)
```

## Step 3: Calculate Sales Ratio and Effective Tax Rate

### Sales Ratio

One indicator used to evaluated regressivity in property tax assessment
is the Sales Ratio, i.e.??the assessed value of a home divided by its
actual selling price. Homes that are assessed at values higher than
their respective sales price have a Sales Ratio greater than 1 and vice
versa. If all homes are appraised at the same rate (regardless of the
value of the home), then all homes should have the same Sales Ratio. If
homes of lower value have Sales Ratios greater than more valuable homes,
this is an indication regressivity.

### Effective Tax Rate

The effective tax rate is the amount of total property taxes paid
divided by the home???s selling price.

``` r
residential2017_sales_bill$sales_ratio<-residential2017_sales_bill$TaxValue/residential2017_sales_bill$SellingPrice
residential2017_sales_bill$effective_tax_rate<-as.numeric(residential2017_sales_bill$tax_due)/residential2017_sales_bill$SellingPrice

##Select Records with Complete Data
residential2017_sales_bill_select<-residential2017_sales_bill%>%
  filter(SellingPrice>10000 & TaxValue>0)%>%
  filter(VacantLot =='N' | Class.y=='RES') %>%
  filter(SALES_YEAR %in% (2013:2017))%>%
  filter(SALES_YEAR>YearBuilt)%>%
  select(PIN, SellingPrice, SALES_YEAR, TaxValue, effective_tax_rate, sales_ratio, PINN)
```

## Figure 1: Sale Ratio - Total Data 2017

![](https://raw.githubusercontent.com/Urban-3/Urban-3.github.io/main/_posts/assets/2021-03-22-Buncombe-Reappraisal_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Figure 2: Effective Tax Rate - Total Data 2017

![](https://raw.githubusercontent.com/Urban-3/Urban-3.github.io/main/_posts/assets/2021-03-22-Buncombe-Reappraisal_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Step 4: Calculate Sales Price Deciles

``` r
residential2017_sales_bill_select<-residential2017_sales_bill_select %>%
  mutate(selldecile = ntile(SellingPrice, 10))

residential2017_sales_bill_select_decile<-residential2017_sales_bill_select %>%
  group_by(selldecile) %>%
  summarise(saleprice = mean(SellingPrice),
         effective_tax_rate=mean(effective_tax_rate),
         sales_ratio=mean(sales_ratio),
         count=sum(n()))
```

## Figure 3: Sales Ratio (Deciles)

![](https://raw.githubusercontent.com/Urban-3/Urban-3.github.io/main/_posts/assets/2021-03-22-Buncombe-Reappraisal_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Figure 4: Effective Tax Rate by Sale Price (Deciles)

![](https://raw.githubusercontent.com/Urban-3/Urban-3.github.io/main/_posts/assets/2021-03-22-Buncombe-Reappraisal_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
