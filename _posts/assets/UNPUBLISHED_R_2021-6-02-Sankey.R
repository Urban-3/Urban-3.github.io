p <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Ad Valorem Taxes",	"Intergovernmental",	"Sales Taxes",	"Sales and Services",	"Appropriated Fund Balance",	"Other Taxes & Licenses",	"Transfers from Other Funds",	"Permits & Fees",	"Other", 	"Bond Proceeds",	"General Fund",	"Education",	"Human Services",	"Public Safety",	"General Government",	"Debt Service",	"Culture & Recreation",	"Economic & Physical Development",	"Transfers to Other Funds"),
    pad = 15,
    thickness = 20,
    color = c("#5086C3","#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8",	"#A6AAA8"),
    line = list(
      color = "black",
      width = 0.5
    ),
    hovertemplate = c("$%{value}<extra></extra>")
  ),
  link = list(
    source = c(0,	1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	10,	10,	10,	10,	10,	10,	10),
    target = c(10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	11,	12,	13,	14,	15,	16,	17,	18),
    value =  c(212211847,	44059853,	30068224,	16949701,	12216382,	7333500,	6547802,	4124000, 1649376,	1369302,	92561618,	86408546,	68103608,	49299222,	19723865,	8416714,	7686792,	4329622),
    hovertemplate = c("$%{value}<extra></extra>"),
    color=c("#719ac7","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4","#c4c4c4")
 )) %>% 
  layout(
    title = "Buncombe County FY22 Budget",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F, title="Holiday Number", tickvals=-1:4, ticktext=1:6),
    yaxis = list(showgrid = F, zeroline = F, showticklabels=FALSE)
  )
p
Sys.setenv("plotly_username"="obaber")
Sys.setenv("plotly_api_key"="dsO9s1YZcMpIlVdogcLY")
chart_link = api_create(p, filename="cra-budget-2019")
chart_link

