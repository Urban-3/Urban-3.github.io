home_value_race<-read.csv("D:\\Urban3\\Projects\\NC\\buncombe_co\\storymap\\home_value_by_race.csv", header=TRUE,encoding="UTF-8")
names(home_value_race)
home_value_race$Value<-factor(home_value_race$Value)
home_value_race$Value<-fct_reorder(home_value_race$Value, home_value_race$Value.Order)
home_value_race_gg<-ggplot(data = home_value_race, aes(x=Value, y=Percent, fill=Race, text=paste0("<b>Home Value: </b>", Value, "<br><b>Ownership: </b>", mypercent(Percent)))) +
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())+
  scale_fill_manual("Race", values = c("Black or African American" = "#7DC242", "White" = "#80AEDD"))

home_value_race_label<- list(
  bordercolor = "transparent",
  font = font
)

home_value_race_ggp<-ggplotly(home_value_race_gg, tooltip="text")%>%
  style(hoverlabel = home_value_race_label) %>%
  layout(font = "NimbusSan")
home_value_race_ggp

########################################################################
########################################################################
########################################################################
names(residential_sales_ratio)
residential_sales_ratio_2013<-residential_sales_ratio%>%
  filter(SALES_YEAR_1==2013)
jcurve_history_2013<-ggplot(data = residential_sales_ratio_2013, aes(saleprice, sales_ratio, text=paste0("<b>Mean Selling Price: </b>", mycurrency(saleprice), "<br><b>Mean Appraised Value: </b>", mycurrency(appraised_value), "<br><b> Sales Ratio: ", myratio(sales_ratio)))) +
  geom_line(colour = "#5086C3", group=1, size= 2) +
  geom_point(color="#5086C3", size= 2) + 
  geom_hline(yintercept=1, color="#001E60")+
  labs(y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  ylim(0.8,1.1)+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())
jcurve_history_2013_annotation <- list(
  text = "<b>2013</b>",
  bgcolor = "#001E60",
  font = tax_value_annotation_font,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
jcurve_history_2013_ggp<-ggplotly(jcurve_history_2013, tooltip="text")%>%
  style(hoverlabel = jcurve_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= jcurve_history_2013_annotation)
jcurve_history_2013_ggp

residential_sales_ratio_2014<-residential_sales_ratio%>%
  filter(SALES_YEAR_1==2014)
jcurve_history_2014<-ggplot(data = residential_sales_ratio_2014, aes(saleprice, sales_ratio, text=paste0("<b>Mean Selling Price: </b>", mycurrency(saleprice), "<br><b>Mean Appraised Value: </b>", mycurrency(appraised_value), "<br><b> Sales Ratio: ", myratio(sales_ratio))), size = 0.5) +
  geom_line(colour = "#5086C3", group=1, size= 2) +
  geom_point(color="#5086C3", size= 2) +
  geom_hline(yintercept=1, color="#001E60")+
  labs(y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())
jcurve_label<- list(
  bgcolor = "#5086C3",
  bordercolor = "transparent",
  font = font
)
jcurve_history_2014_annotation <- list(
  text = "<b>2014</b>",
  bgcolor = "#001E60",
  font = tax_value_annotation_font,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
jcurve_history_2014_ggp<-ggplotly(jcurve_history_2014, tooltip="text")%>%
  style(hoverlabel = jcurve_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= jcurve_history_2014_annotation)
jcurve_history_2014_ggp


residential_sales_ratio_2015<-residential_sales_ratio%>%
  filter(SALES_YEAR_1==2015)
jcurve_history_2015<-ggplot(data = residential_sales_ratio_2015, aes(saleprice, sales_ratio, text=paste0("<b>Mean Selling Price: </b>", mycurrency(saleprice), "<br><b>Mean Appraised Value: </b>", mycurrency(appraised_value), "<br><b> Sales Ratio: ", myratio(sales_ratio))), size = 0.5) +
  geom_line(colour = "#5086C3", group=1, size= 2) +
  geom_point(color="#5086C3", size= 2) + 
  geom_hline(yintercept=1, color="#001E60")+ 
  labs(y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())
jcurve_history_2015_annotation <- list(
  text = "<b>2015</b>",
  bgcolor = "#001E60",
  font = tax_value_annotation_font,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
jcurve_history_2015_ggp<-ggplotly(jcurve_history_2015, tooltip="text")%>%
  style(hoverlabel = jcurve_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= jcurve_history_2015_annotation)
jcurve_history_2015_ggp

residential_sales_ratio_2016<-residential_sales_ratio%>%
  filter(SALES_YEAR_1==2016)
jcurve_history_2016<-ggplot(data = residential_sales_ratio_2016, aes(saleprice, sales_ratio, text=paste0("<b>Mean Selling Price: </b>", mycurrency(saleprice), "<br><b>Mean Appraised Value: </b>", mycurrency(appraised_value), "<br><b> Sales Ratio: ", myratio(sales_ratio))), size = 0.5) +
  geom_line(colour = "#5086C3", group=1, size= 2) +
  geom_point(color="#5086C3", size= 2) + 
  geom_hline(yintercept=1, color="#001E60")+
  labs(y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())
jcurve_history_2016_annotation <- list(
  text = "<b>2016</b>",
  bgcolor = "#001E60",
  font = tax_value_annotation_font,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
jcurve_history_2016_ggp<-ggplotly(jcurve_history_2016, tooltip="text")%>%
  style(hoverlabel = jcurve_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= jcurve_history_2016_annotation)
jcurve_history_2016_ggp

jcurve_history_2014_2017<-subplot(jcurve_history_2013_ggp, jcurve_history_2014_ggp,jcurve_history_2015_ggp,jcurve_history_2016_ggp, shareY=TRUE, shareX=TRUE, nrows=1)
jcurve_history_2014_2017

year<-c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
buncombe_rate<-c(0.630,0.590,0.590,0.590,0.590,0.530,0.525,0.525,0.525,0.525,0.525,0.525,0.604,0.604,0.604,0.539,0.529,0.529,0.529)
taxable_value<-c(13777912,17095153,17496502,18034880,19105553,26181099,27258520,28086251,28841167,29086915,29314988,29679981,28057219,28877723,29544516,30417045,36264613,37528113,39338387)
tax_collected<-c(93527086,108802529,113044025,116654786,123668417,152744354,157568418,162584050,165793158,167950517,169368975,171520939,192949306,191565661,196458687,201907004,216086991,221770248,231903837)

buncombe_rate_year<-as.data.frame(cbind(year, buncombe_rate))
buncombe_rate_year$TYPE<-"TAXRATE"
names(buncombe_rate_year)<-c("YEAR","VALUE","TYPE")
taxable_value_year<-as.data.frame(cbind(year, taxable_value))
taxable_value_year$TYPE<-"TAXVALUE"
names(taxable_value_year)<-c("YEAR","VALUE","TYPE")
tax_collected_year<-as.data.frame(cbind(year, tax_collected))
tax_collected_year$TYPE<-"TAXREVENUE"
names(tax_collected_year)<-c("YEAR","VALUE","TYPE")

buncombe_taxes_long<-as.data.frame(rbind(buncombe_rate_year, taxable_value_year, tax_collected_year))

buncombe_taxes<-as.data.frame(cbind(year, buncombe_rate, taxable_value, tax_collected))
tax_collected_m<-buncombe_taxes$tax_collected/1000000
taxable_value_b<-buncombe_taxes$taxable_value/1000000

mycurrency <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=",")))
}

mypercent <- function(x){
  return(paste(formatC(as.numeric(x), format="f", digits=0, big.mark=","),"%"))
}

mypercent_1 <- function(x){
  return(paste(formatC(as.numeric(x), format="f", digits=1, big.mark=","),"%"))
}

mycurrency_M <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","),"M"))
}

mycurrency_B <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","),"B"))
}

myratio <- function(x){
  return(paste(formatC(as.numeric(x), format="f", digits=2, big.mark=",")))
}

font <- list(
  family = "Courier",
  size = 15,
  color = "white"
)

tax_value_annotation_font <- list(
  family = "Courier",
  size = 20,
  color = "white"
)

tax_value_label<- list(
  bgcolor = "#000000",
  bordercolor = "transparent",
  font = font
)

tax_value_annotation <- list(
  text = "<b>Taxable Value</b>",
  bgcolor = "#000000",
  font = tax_value_annotation_font,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

tax_rate_annotation_font <- list(
  family = "Courier",
  size = 20,
  color = "white"
)

tax_rate_label <- list(
  bgcolor = "#808080",
  bordercolor = "transparent",
  font = font
)

tax_rate_annotation <- list(
  text = "<b>Tax Rate</b>",
  font = tax_rate_annotation_font,
  bgcolor = "#808080",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

tax_revenue_annotation_font <- list(
  family = "Courier",
  size = 20,
  color = "white"
)

tax_revenue_label<- list(
  bgcolor = "#5086C3",
  bordercolor = "transparent",
  font = font
)

tax_revenue_annotation <- list(
  text = "<b>Tax Revenue</b>",
  font = tax_revenue_annotation_font,
  bgcolor = "#5086C3",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


tax_value_gg<-ggplot(data=buncombe_taxes, aes(year, taxable_value_b, text=paste0("<b>Total Taxable Value: </b>", mycurrency_B(taxable_value_b), "<br><b>Year: </b>", year)))+
  geom_line(group=1, color = "#000000", size=2)+
  labs(y = "Total Taxable Value", x = "Year")+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())+
  scale_y_continuous(breaks=c(20, 30, 40),
                     labels=c("$20 M", "$30 M", "$40 M"))
tax_value_ggp<-ggplotly(tax_value_gg, tooltip="text")%>%
  style(hoverlabel = tax_value_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= tax_value_annotation)

tax_rate_gg<-ggplot(data=buncombe_taxes, aes(year, buncombe_rate, text=paste0("<b>Tax Rate: </b>", buncombe_rate, "<br><b>Year: </b>", year)))+
  geom_line(group=1, color = "#808080", size=2)+
  ylim(c(0,1))+
  labs(y = "Tax Rate per $100 assessed value", x = "Year")+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())
tax_rate_ggp<-ggplotly(tax_rate_gg, tooltip="text")%>%
  style(hoverlabel = tax_rate_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= tax_rate_annotation)


tax_collected_gg<-ggplot(data=buncombe_taxes, aes(year, tax_collected_m, text=paste0("<b>Property Tax Revenue: </b>", mycurrency_M(tax_collected_m), "<br><b>Year: </b>", year)))+
  geom_line(group=1, color = "#5086C3", size=2)+
  labs(y = "Property Tax Revenue", x = "Year")+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())+
  scale_y_continuous(breaks=c(120, 160, 200),
                     labels=c("$120 M", "$160 M", "$200 M"))
tax_collected_ggp<-ggplotly(tax_collected_gg, tooltip="text")%>%
  style(hoverlabel = tax_revenue_label) %>%
  layout(font = "NimbusSan") %>%
  layout(annotations= tax_revenue_annotation)

subplot(list(tax_value_ggp, tax_rate_ggp, tax_collected_ggp), nrows=1, titleX = TRUE, titleY=TRUE,  margin = 0.07)



###############
minicozzi<-appraisal_history2019_21%>%
  group_by(PIN) %>%
  arrange(year, .by_group=TRUE) %>% ##Arrange the data according to appraisal year, this is an important step for in order to calculate percent change
  mutate(pct_change= ((totalvalue-lag(totalvalue))/lag(totalvalue)*100), ##Calculate percent change from the previous year. Here, the lag() function uses the preceding value. Important to arrange first!!
         first_totval=head(totalvalue, 1), ##Adds a new column with the 2001 total taxable value, used as the baseline for the next percent change calculation
         baselinechange=case_when(totalvalue!=first_totval~(totalvalue-first_totval)*100/first_totval, TRUE~1*NA),##Calculates percent change relative to the base year's total value
         first_landval=head(landvalue, 1),
         landbaselinechange=case_when(landvalue!=first_landval~(landvalue-first_landval)*100/first_landval, TRUE~1*NA))%>%
  filter(PIN=='964902712000000')

minicozzi_gg<-ggplot(data=minicozzi, aes(year, baselinechange, text=paste0("<b>Total Value: </b>", mycurrency(totalvalue), "<br><b>Percent Change: </b>", mypercent(baselinechange)))) +
  geom_line(color="#5086C3", size=2, group=1)+
  labs(y="Percent Change in Taxable Value", x="Year")+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank())

minicozzi_ggp<-ggplotly(minicozzi_gg, tooltip="text")%>%
  style(hoverlabel = tax_revenue_label) %>%
  layout(font = "NimbusSan")
minicozzi_ggp


######################################################
######################################################
##Organize 2019 appraisal history data frame
appraisal_history2019_full<-appraisal_history2019 %>%
  distinct(�..PIN,TaxYear, .keep_all = TRUE) %>% ##Remove duplicates within the data frame
  mutate(PIN=�..PIN,
         year=TaxYear,
         landvalue=LandVal)%>%
  add_count(PIN)%>% ##Add a field for the number of times the PIN appears in the data frame
  filter(n==19)%>% ##Select only those records with 19 observations per PIN (complete data from 2001-2019)
  filter(totalvalue > 0 & landvalue<(totalvalue*0.5))%>% ##Select only those records with values in 2001
  select(PIN, year, landvalue, totalvalue)

unique2019<-unique(appraisal_history2019_full$PIN) ##Create a list of unique parcel IDs, to be used in the nex step

###Organize 2021 appraisal data frame
snapshot2021_full<-parcelsnapshot2021_table %>%
  mutate(year=2021,
         landvalue=LandValue,
         totalvalue=TaxValue)%>%
  filter(PIN %in% unique2019)%>% ##Select parcels from the 2021 dataframe that are also in the 2019 dataframe
  filter(Class %in% c('100', '180'))%>% ##Select only single family residential parcels
  filter(totalvalue > 0)%>%
  select(PIN, year, landvalue, totalvalue)

appraisal_history2019_21_full<-rbind(appraisal_history2019_full,snapshot2021_full) ##Combine the 2019 and 2021 data frames

##Select complete records
appraisal_history2019_21_full_s<-appraisal_history2019_21_full%>%
  add_count(PIN)%>% ##Add count field 
  filter(n==20) ##Select only those PINs with complete records from 2001-2021

##Calculate percent change in value
appraisal_history2019_21_full_s<-appraisal_history2019_21_full_s%>%
  filter(totalvalue!=0)%>%
  group_by(PIN) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(pct_change= ((totalvalue-lag(totalvalue))/lag(totalvalue)*100), ##Calculate percent change from the previous year. Here, the lag() function uses the preceding value. Important to arrange first!!
         first_totval=head(totalvalue, 1), ##Adds a new column with the 2001 total taxable value, used as the baseline for the next percent change calculation
         baselinechange=case_when(totalvalue!=first_totval~(totalvalue-first_totval)*100/first_totval, TRUE~1*NA),##Calculates percent change relative to the base year's total value
         first_landval=head(landvalue, 1), ##Do the same for land value
         landbaselinechange=case_when(landvalue!=first_landval~(landvalue-first_landval)*100/first_landval, TRUE~1*NA))%>% 
  ungroup()%>%
  filter(year!=2001)%>% ##Percent change from 2001 will not be calculated on itself, so remove it from the data frame
  mutate(Quintile = ntile(first_totval, 5))%>% ##Calculate quintiles according to the homes original value in 2001
  group_by(Quintile, year)%>%
  summarise(mean=mean(baselinechange, na.rm=TRUE), 
            meanval=mean(totalvalue),
            meanland=mean(landbaselinechange, na.rm=TRUE))

appraisal_history2019_21_full_s$Quintile<-as.factor(appraisal_history2019_21_full_s$Quintile)
appraisal_history2019_21_full_s<-appraisal_history2019_21_full_s%>%
  mutate(value_label=ifelse(Quintile==5, "Highest", ifelse(Quintile==4, "Moderately High", ifelse(Quintile==3, "Medium", ifelse(Quintile==2, "Moderately Low", "Lowest")))))
appraisal_history2019_21_full_s$value_label<-factor(appraisal_history2019_21_full_s$value_label, levels=c("Lowest", "Moderately Low", "Medium", "Moderately High", "Highest"))

#### Figure 5: Percent change in TOTAL taxable value from 2001-- comparisons across home value
quintile_total_annotation_font <- list(
  family = "Courier",
  size = 20,
  color = "white"
)

quintile_total_label<- list(
  bgcolor = "#5086C3",
  bordercolor = "transparent",
  font = font
)

quintile_total_annotation <- list(
  text = "<b>Total Value</b>",
  font = tax_revenue_annotation_font,
  bgcolor = "#5086C3",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
quintile_total_gg<-ggplot(data=appraisal_history2019_21_full_s, aes(x=year, y=mean, group=Quintile, text=paste0("<b>Value: </b>", value_label, "<br><b>Percent Change: </b>", mypercent(mean), "<br><b>Year: </b>", year))) +
  geom_line(aes(size=value_label), color="#5086C3")+
  xlab("Year") + ylab("Avg. Percent Change in Taxable Value")+
  ylim(c(0,720))+
  scale_size_manual(values = c(2,1.5,1,0.75,0.5), name = "Starting Value")+
  theme(panel.background = element_blank(),
        legend.position = "none")

quintile_total_ggp<-ggplotly(quintile_total_gg, tooltip="text")%>%
  style(hoverlabel = tax_revenue_label) %>%
  layout(font = "NimbusSan")%>%
  layout(annotations= quintile_total_annotation)

quintile_total_ggp

#### Figure 6: Percent change in LAND value from 2001-- comparisons across home value

quintile_land_annotation_font <- list(
  family = "Courier",
  size = 20,
  color = "white"
)

quintile_land_label<- list(
  bgcolor = "#001E60",
  bordercolor = "transparent",
  font = font
)

quintile_land_annotation <- list(
  text = "<b>Land Value</b>",
  font = tax_revenue_annotation_font,
  bgcolor = "#001E60",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


quintile_land_gg<-ggplot(data=appraisal_history2019_21_full_s, aes(x=year, y=meanland, group=Quintile, text=paste0("<b>Value: </b>", value_label, "<br><b>Percent Change: </b>", mypercent(meanland), "<br><b>Year: </b>", year))) +
  geom_line(aes(size=value_label), color="#001E60")+
  xlab("Year") + ylab("Avg. Percent Change in Taxable Value")+
  ylim(c(0,720))+
  scale_size_manual(values = c(2,1.5,1,0.75,0.5), name = "Starting Value")+
  theme(panel.background = element_blank())
quintile_land_gg

quintile_land_ggp<-ggplotly(quintile_land_gg, tooltip="text")%>%
  style(hoverlabel = quintile_land_label) %>%
  layout(font = "NimbusSan") %>%
  layout(annotations= quintile_land_annotation)

quintile_land_ggp

subplot(list(quintile_total_ggp, quintile_land_ggp), nrows=1, titleX = TRUE,  shareY=TRUE, margin = 0.07)





##################################
##################################
names(sales_ratio)
sales_ratio_complete<-sales_appraised%>%
  filter(Class %in% c("RES","TOWNHOME","MULTIPLE R"," CONDO"))%>%
  mutate(selldecile = ntile(SellingPrice, 10))%>%
  group_by(selldecile)%>%
  summarise(mean_ratio = mean(sales_ratio),
            mean_selling_price = mean(SellingPrice),
            mean_assessed_value = mean(totalvalue))

ggplot(data = sales_ratio_complete, aes(mean_selling_price, mean_ratio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Average Sales-to-Appraised-Value Ratios (SFR, Condo, Townhome)",
       subtitle = "2001 - 2021",
       y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90))

sales_data_table<-read.csv("D:\\Urban3\\Projects\\NC\\buncombe_co\\analyzed_data\\r_output\\sales_data_properties.csv", header=TRUE)
appraisal_history2019$totalvalue<-appraisal_history2019$LandVal+appraisal_history2019$BldgVal+appraisal_history2019$ImprVal ##Calculate total taxable value
appraisal_history2019_abbr<-appraisal_history2019 %>%
  distinct(�..PIN,TaxYear, .keep_all = TRUE) %>% #The data set includes duplicated records. Remove duplicates.
  mutate(PIN=�..PIN, #recode variable names
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

##To Compare Sales Values to the NEXT Assessment Year Value
sales_data_table_year$PIN_YEAR=paste(sales_data_table_year$PIN, sales_data_table_year$SALES_YEAR_1, sep="_")

sales_appraised<-merge(appraisal_history2019_abbr, sales_data_table_year, by="PIN_YEAR")
sales_appraised$sales_ratio<-sales_appraised$totalvalue/sales_appraised$SellingPrice
snapshot2019<-read.csv("D:\\Urban3\\Projects\\NC\\buncombe_co\\Buncombe_County_Parcel_Snapshot_from_2019-shp\\Buncombe_County_Parcel_Snapshot_from_2019.csv", header=TRUE)
names(snapshot2019)
snapshot2019_s<-snapshot2019%>%
  select(PIN,NeighborhoodCode)

sales_appraised_wngh<-merge(sales_appraised, snapshot2019_s, by.x="PIN.x", by.y="PIN")
tax_district_race<-read.csv("D:\\Urban3\\Projects\\NC\\buncombe_co\\analyzed_data\\r_output\\tax_district_race.csv", header=TRUE)

sales_appraised_wngh$index<-1
residential_sales_ratio_wngh<-sales_appraised_wngh%>%
  filter(Class %in% c("RES","TOWNHOME","MULTIPLE R"," CONDO"))%>%
  group_by(NeighborhoodCode)%>%
  summarise(mean_ratio=mean(sales_ratio),
         median_ratio=median(sales_ratio),
         mean_selling_price=mean(SellingPrice),
         median_selling_price=median(SellingPrice),
         mean_total_value=mean(totalvalue),
         median_total_value=median(totalvalue),
         count=sum(index))%>%
  filter(count>20)

ggplot(data = residential_sales_ratio_wngh, aes(mean_selling_price, mean_ratio)) +
  geom_point(color="steelblue", size= 0.5) + 
  geom_smooth(method=lm)+
  labs(title = "Average Sales-to-Appraised-Value Ratios (SFR)",
       subtitle = "2001 - 2021",
       y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90))  

sales_appraised_wngh_race<-merge(residential_sales_ratio_wngh, tax_district_race, by.x="NeighborhoodCode", by.y="Neighborho")

ggplot(data = sales_appraised_wngh_race, aes(PERC_BLACK, mean_ratio)) +
  geom_point(color="steelblue", size= 0.5) + 
  geom_smooth(method=lm)+
  labs(title = "Average Sales-to-Appraised-Value Ratios (SFR)",
       subtitle = "2001 - 2021",
       y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  theme(axis.text.x = element_text(angle = 90))  

filter(NeighborhoodCode %in% neighborhoods)





residential_sales_ratio<-sales_appraised%>%
  filter(Class %in% c("RES","TOWNHOME","MULTIPLE R"," CONDO"))%>%
  group_by(SALES_YEAR_1)%>%
  mutate(selldecile = ntile(SellingPrice, 10),
         mean_year_ratio=mean(sales_ratio),
         median_year_ratio=median(sales_ratio),
         mean_selling_price=mean(SellingPrice),
         median_selling_price=median(SellingPrice),
         mean_total_value=mean(totalvalue),
         median_total_value=median(totalvalue))%>%
  ungroup()%>%
  group_by(selldecile, SALES_YEAR_1) %>%
  summarise(saleprice = mean(SellingPrice),
            sales_ratio=mean(sales_ratio),
            appraised_value=mean(totalvalue),
            count=sum(n()),
            mean_year_ratio = first(mean_year_ratio),
            median_year_ratio = first(median_year_ratio),
            mean_selling_price=first(mean_selling_price),
            median_selling_price=first(median_selling_price),
            mean_total_value=first(mean_total_value),
            median_total_value=first(median_total_value))

ggplot(data = residential_sales_ratio, aes(saleprice, sales_ratio)) +
  geom_line(color = "steelblue", size = 0.5) +
  geom_point(color="steelblue", size= 0.5) + 
  labs(title = "Average Sales-to-Appraised-Value Ratios (SFR)",
       subtitle = "2001 - 2021",
       y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  facet_grid(~ SALES_YEAR_1)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = residential_sales_ratio, aes(saleprice, sales_ratio)) +
  geom_line(color = "steelblue", size = 0.5) +
  geom_point(color="steelblue", size= 0.5) + 
  labs(title = "Average Sales-to-Appraised-Value Ratios (SFR)",
       subtitle = "2001 - 2021",
       y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  facet_grid(~ SALES_YEAR_1)+
  theme(axis.text.x = element_text(angle = 90))+ 
  geom_line(aes(saleprice, median_year_ratio), color = "steelblue", size = 0.5)

ggplot(data = residential_sales_ratio, aes(saleprice, sales_ratio)) +
  geom_line(aes(saleprice, median_year_ratio), color = "steelblue", size = 0.5)+
  scale_x_continuous(labels=scales::dollar_format())+
  facet_grid(~ SALES_YEAR_1)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = residential_sales_ratio, aes(saleprice, sales_ratio)) +
  geom_line(aes(saleprice, mean_selling_price), color = "red", size = 0.5)+
  geom_line(aes(saleprice, mean_total_value), color = "green", size = 0.5)+
  scale_x_continuous(labels=scales::dollar_format())+
  facet_grid(~ SALES_YEAR_1)+
  theme(axis.text.x = element_text(angle = 90))

names(sales_appraised)

residential_sales_ratio_2019<-sales_appraised%>%
  filter(Class %in% c("RES","TOWNHOME","MULTIPLE R"," CONDO"))%>%
  filter(SALES_YEAR_1==2019)%>%
  group_by(SALES_YEAR_1)%>%
  mutate(selldecile = ntile(SellingPrice, 10),
         mean_year_ratio=mean(sales_ratio),
         median_year_ratio=median(sales_ratio))
write.csv(residential_sales_ratio_2019, "D:\\Urban3\\Projects\\NC\\buncombe_co\\analyzed_data\\r_output\\residential_sales_ratio_2019.csv")

p <-sales_appraised%>%
  filter(Class %in% c("RES","TOWNHOME","MULTIPLE R"," CONDO"))%>%
  select(totalvalue, SellingPrice, SALES_YEAR_1)%>%
  gather(Category, Value, totalvalue:SellingPrice)%>%
  ggplot( aes(x=Value, fill=Category)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position='identity', bins=30) +
  scale_fill_manual(values=c("#69b3a2", "#404080"))+
  facet_grid(~ SALES_YEAR_1)+
  xlim(c(0,2000000))
p


################################################
################################################

buncombe_finances<-read.csv("D:\\Urban3\\Projects\\NC\\buncombe_co\\storymap\\buncombe_finances.csv", header=TRUE)
names(buncombe_finances)
unique(buncombe_finances$RevenueType)
buncombe_finances$RevenueType<-factor(buncombe_finances$RevenueType, levels=c("Local option sales taxes","Other taxes","Intergovernmental","Permits and fees","Charges for services","Investment earnings","Miscellaneous", "Ad valorem taxes"))



county_finances_annotation_font <- list(
  family = "Courier",
  size = 20,
  color = "white"
)

county_finances_label<- list(
  font = "Courier"
)

buncombe_finances$Location_f<-factor(buncombe_finances$Location, levels=c("Weaverville","Black Mountain","Asheville","Montreat","Woodfin","Biltmore Forest","Buncombe"))
buncombe_finances_gg<-ggplot(buncombe_finances, aes(x = 1, y = Percent, fill = RevenueType, text=paste0(RevenueType, "<br>", mycurrency(Amount), " (", mypercent(Percent),")"))) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#5086C3","#F4F4F5", "#A6AAA8", "#C2C2C3", "#9A9A9B", "#E0E0E1", "#545455", "#363637"))+
  facet_grid(. ~ Location_f)+
  theme(strip.text = element_text(colour = 'white', face='bold'))+
  theme(panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        strip.background =element_rect(fill="#001E60"))+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

buncombe_finances_gg
buncombe_finances_ggp<-ggplotly(buncombe_finances_gg, tooltip="text")%>%
  style(hoverlabel = county_finances_label) %>%
  layout(font = "NimbusSan")
buncombe_finances_ggp

################################################################################################
################################################################################################

neighborhoods<-c("R4GB", "R3GB", "BMFE", "BMFI", "R4FC","BHDA", "ASHC")
names(appraisal_history2019_21_full_wneigh)
appraisal_history2019_21_full_wneigh_s<-appraisal_history2019_21_full_wneigh%>%
  group_by(PIN_join) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(pct_change= ((totalvalue-lag(totalvalue))/lag(totalvalue)*100), ##Calculate percent change from the previous year. Here, the lag() function uses the preceding value. Important to arrange first!!
         first_totval=head(totalvalue, 1), ##Adds a new column with the 2001 total taxable value, used as the baseline for the next percent change calculation
         baselinechange=case_when(totalvalue!=first_totval~(totalvalue-first_totval)*100/first_totval, TRUE~1*NA),##Calculates percent change relative to the base year's total value
         first_landval=head(landvalue, 1), ##Do the same for land value
         landbaselinechange=case_when(landvalue!=first_landval~(landvalue-first_landval)*100/first_landval, TRUE~1*NA))%>% 
  ungroup()%>%
  filter(year!=2001)%>%
  group_by(Neighborho, year)%>%
  filter(Neighborho %in% neighborhoods)%>%
  summarise(mean=mean(baselinechange, na.rm=TRUE),
            meanval=mean(totalvalue),
            meanland=mean(landbaselinechange, na.rm=TRUE))


#### Figure 10: Change in Total taxable value-- comparisons between neighborhoods 
ggplot(data=appraisal_history2019_21_full_wneigh_s, aes(x=year, y=meanval, group=Neighborho)) +
  geom_line(aes(color=Neighborho))+
  xlab("Year") + ylab("Avg. Change in Total Taxable Value")+
  scale_color_discrete(name = "Neighborhood")+
  scale_y_continuous(labels=scales::dollar_format())


#### Figure 11: Percent change in TOTAL taxable value from 2001-- comparisons between neighborhoods 
ggplot(data=appraisal_history2019_21_full_wneigh_s, aes(x=year, y=mean, group=Neighborho)) +
  geom_line(aes(color=Neighborho))+
  xlab("Year") + ylab("Avg. Percent Change in Total Taxable Value")+
  scale_color_discrete(name = "Neighborhood")


#### Figure 12: Percent change in LAND value from 2001-- comparisons between neighborhoods
ggplot(data=appraisal_history2019_21_full_wneigh_s, aes(x=year, y=meanland, group=Neighborho)) +
  geom_line(aes(color=Neighborho))+
  xlab("Year") + ylab("Avg. Percent Change in Land Value")+
  scale_color_discrete(name = "City")
