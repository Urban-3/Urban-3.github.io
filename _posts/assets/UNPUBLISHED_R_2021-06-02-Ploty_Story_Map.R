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


residential_sales_ratio_2013_2016<-residential_sales_ratio%>%
  filter(SALES_YEAR_1>2012 & SALES_YEAR_1<2017)

ggplot(data = residential_sales_ratio_2013_2016, aes(saleprice, sales_ratio)) +
  geom_line(color = "steelblue", size = 0.5) +
  geom_point(color="steelblue", size= 0.5) + 
  labs(title = "Average Sales-to-Appraised-Value Ratios (SFR)",
       subtitle = "2001 - 2021",
       y = "Averages Sales : Value Ratio", x = "Selling Price") + 
  scale_x_continuous(labels=scales::dollar_format())+
  facet_grid(~ SALES_YEAR_1)+
  theme(axis.text.x = element_text(angle = 90))+ 
  geom_line(aes(saleprice, median_year_ratio), color = "steelblue", size = 0.5)

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

mycurrency_M <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","),"M"))
}

mycurrency_B <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","),"B"))
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
