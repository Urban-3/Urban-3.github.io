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

buncombe_taxes<-as.data.frame(cbind(year, buncombe_rate, taxable_value, tax_collected))

font <- list(
  family = "Courier",
  size = 15,
  color = "white"
)
label <- list(
  bgcolor = "#001E60",
  bordercolor = "transparent",
  font = font
)


tax_rate_gg<-ggplot(data=buncombe_taxes, aes(year, buncombe_rate), text=paste("<b>Tax Rate:</b>", buncombe_rate))+
  geom_line(color = "#5086C3")+
  ylim(c(0,1))+
  labs(y = "Tax Rate per $100 assessed value", x = "Year")+
  theme(axis.text.x = element_text(angle = 90))
tax_rate_ggp<-ggplotly(tax_rate_gg, tooltip="text")%>%
  style(hoverlabel = label) %>%
  layout(font = "NimbusSan")
tax_rate_ggp

ggplot(data=buncombe_taxes, aes(year, taxable_value))+
  geom_line(color = "steelblue")

ggplot(data=buncombe_taxes, aes(year, tax_collected))+
  geom_line(color = "steelblue")
