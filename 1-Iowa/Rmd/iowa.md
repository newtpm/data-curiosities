---
title: "Iowa Liquor Sales Analysis"
output:
  html_document:
    #df_print: paged
    theme: sandstone
    highlight: kate
    css: public_style.css
    code_folding: hide
    keep_md: true
fontsize: 16pt
editor_options:
  chunk_output_type: console
---

<style type="text/css">
.main-container {
  max-width: 1250px;
  margin-left: auto;
  margin-right: auto;
}
</style>

# Objective 

The State of Iowa has published a really interesting dataset around their liquor sales. I thought it would be interesting to see how sales/consumption patterns changed as a result of coronavirus. The data is for wholesale sales from the distributor to the retail outlet so is not directly indicative of consumer patterns, but our period of interest is 11 weeks so we feel it is as good a proxy as we can expect. 

Source data is 19M purchases by Iowa Class “E” liquor licensees,  dating back to 2012, and can be downloaded from [data.iowa.gov](https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy). 




```r
library(tidyverse)
library(lubridate)
library(wesanderson)
library(janitor)
library(gt)
library(ggiraph)

liq <- read_rds("../input/iowa_liq_short.Rds")  %>%
  mutate(Inv = str_remove_all(Inv, "INV-"),
         InvLine = str_sub(Inv, -5, -1),
         Inv = str_sub(Inv, 1, 6))

#wes_cols <- c(wes_palette("GrandBudapest2"), wes_palette("GrandBudapest1"))
wes_cols <- c(wes_palette("Darjeeling2"), rev(wes_palette("Darjeeling1")))

gt(head(liq, 1))
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#oiagxfkbog .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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

#oiagxfkbog .gt_heading {
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

#oiagxfkbog .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oiagxfkbog .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oiagxfkbog .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oiagxfkbog .gt_col_headings {
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

#oiagxfkbog .gt_col_heading {
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

#oiagxfkbog .gt_column_spanner_outer {
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

#oiagxfkbog .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oiagxfkbog .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oiagxfkbog .gt_column_spanner {
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

#oiagxfkbog .gt_group_heading {
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

#oiagxfkbog .gt_empty_group_heading {
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

#oiagxfkbog .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oiagxfkbog .gt_from_md > :first-child {
  margin-top: 0;
}

#oiagxfkbog .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oiagxfkbog .gt_row {
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

#oiagxfkbog .gt_stub {
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

#oiagxfkbog .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oiagxfkbog .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#oiagxfkbog .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oiagxfkbog .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oiagxfkbog .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oiagxfkbog .gt_footnotes {
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

#oiagxfkbog .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#oiagxfkbog .gt_sourcenotes {
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

#oiagxfkbog .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#oiagxfkbog .gt_left {
  text-align: left;
}

#oiagxfkbog .gt_center {
  text-align: center;
}

#oiagxfkbog .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oiagxfkbog .gt_font_normal {
  font-weight: normal;
}

#oiagxfkbog .gt_font_bold {
  font-weight: bold;
}

#oiagxfkbog .gt_font_italic {
  font-style: italic;
}

#oiagxfkbog .gt_super {
  font-size: 65%;
}

#oiagxfkbog .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="oiagxfkbog" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Inv</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Date</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StoreNo</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Store</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">City</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Zip</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">County</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Long</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Lat</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Cat</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Vendor</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">ItemNo</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Item</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">BottleVol</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">UnitCost</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">UnitPrice</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Sales</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Units</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SalesVol</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Month</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Week</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Day</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Source</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Brand</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">InvLine</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">217962</td>
      <td class="gt_row gt_left">2019-09-10</td>
      <td class="gt_row gt_right">2604</td>
      <td class="gt_row gt_left">Hy-Vee Wine and Spirits</td>
      <td class="gt_row gt_left">Le Mars</td>
      <td class="gt_row gt_right">51031</td>
      <td class="gt_row gt_left">Plymouth</td>
      <td class="gt_row gt_left">-96.18335000000002</td>
      <td class="gt_row gt_left">42.778257</td>
      <td class="gt_row gt_left">American Vodkas</td>
      <td class="gt_row gt_left">Laird &amp; Company</td>
      <td class="gt_row gt_right">35926</td>
      <td class="gt_row gt_left">Five O'Clock Vodka PET</td>
      <td class="gt_row gt_right">750</td>
      <td class="gt_row gt_right">3.37</td>
      <td class="gt_row gt_right">5.06</td>
      <td class="gt_row gt_right">60.72</td>
      <td class="gt_row gt_right">12</td>
      <td class="gt_row gt_right">9</td>
      <td class="gt_row gt_right">2019</td>
      <td class="gt_row gt_center">Sep</td>
      <td class="gt_row gt_right">37</td>
      <td class="gt_row gt_center">Tue</td>
      <td class="gt_row gt_left">Vodka</td>
      <td class="gt_row gt_left">American</td>
      <td class="gt_row gt_left">Five O'Clock Vodka PET</td>
      <td class="gt_row gt_left">00021</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

# Exploratory Data Analysis

Our reduced data set is from 2019-01-02 through 2020-05-29 and contains 3.41M rows and totals \$495.01M in wholesale sales or about 31.56M litres of booze. 

### Disclaimer

I know nothing about the liquor industry (apart from what I learned as a consumer). Consequently, I may have things wrong here - so this analysis is for curiosity purposes only. 

Also, this data is just liquor sales for the state of Iowa... so I recognize that sales patterns may differ elsewhere and beer/wine substitutions are ignored. 

## Conronavirus Lockdown 

Iowa never actually issued a "Stay-at-Home" order, but business were restricted, some employees were laid off, and employers told their staff to work from home where possible. For our analysis, we assume any coronovirus impact started with Week 11 (Monday March 11, 2020) and continuing to Week 22 (which is the extent of our dataset). For convenience we will call this the "Lockdown Period". 

### Sales by Week

Judging from the Year over Year sales, there is a noticeable increase in sales during the lockdown period. 


```r
liq %>%
  group_by(Year, Week) %>%
  summarise(Sales = sum(Sales)) %>%
  filter(Week != 53) %>% # partial
  ggplot(aes(Week, Sales/1e6, group = Year, color = factor(Year))) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = c(11, 22), linetype = 2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.position = "bottom", legend.title=element_blank()) +
  labs(title = NULL, x = "Week", 
       y= "Sales (in $M's)") +
  scale_x_continuous(breaks = seq(1, 53, by = 3), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 10, by = .5), expand = c(0, 0)) +
  scale_color_manual(values = wes_cols[c(4, 2)])
```

<img src="iowa_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## Top 2020 Brands


```r
top <- liq %>% 
  group_by(Brand) %>%
  summarise(Sales = sum(Sales)/1e6) %>%
  arrange(desc(Sales))
```

### Category Tastes

We can determine that overall sales increased almost 10% over the same period in 2019, but what is the composition of that increase. As this was not a typical Y-Y increase, it is interesting to see how tastes changed given that people were going to be more home-bound than normal. 


```r
lock <- liq %>%
  filter(Week >= 11 & Week <= 22)

lock %>% 
  group_by(Year, Type) %>%
  summarise(Sales = round(sum(Sales)/1e6, 2)) %>%
  pivot_wider(names_from = Year, values_from = Sales ) %>%
  mutate(Change = `2020` - `2019`) %>%
  arrange(desc(`2020`)) %>%
  adorn_totals() %>%
  mutate(`Change%` = round(Change / `2019` * 100, 2)) %>%
  gt() %>%
    tab_header(
    title = "Store Purchases 2019-2020 (in $M)",
    subtitle = "For Coronavirus Related Weeks 11-22"
  ) %>%
    fmt_number(
    columns = vars(`2019`, `2020`),
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = wes_cols[2]),
      cell_text(color = "white")
      ),
    locations = cells_body(
      rows = Type == "Total")
  )
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#adxzlguwzj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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

#adxzlguwzj .gt_heading {
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

#adxzlguwzj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#adxzlguwzj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#adxzlguwzj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#adxzlguwzj .gt_col_headings {
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

#adxzlguwzj .gt_col_heading {
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

#adxzlguwzj .gt_column_spanner_outer {
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

#adxzlguwzj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#adxzlguwzj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#adxzlguwzj .gt_column_spanner {
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

#adxzlguwzj .gt_group_heading {
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

#adxzlguwzj .gt_empty_group_heading {
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

#adxzlguwzj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#adxzlguwzj .gt_from_md > :first-child {
  margin-top: 0;
}

#adxzlguwzj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#adxzlguwzj .gt_row {
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

#adxzlguwzj .gt_stub {
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

#adxzlguwzj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#adxzlguwzj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#adxzlguwzj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#adxzlguwzj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#adxzlguwzj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#adxzlguwzj .gt_footnotes {
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

#adxzlguwzj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#adxzlguwzj .gt_sourcenotes {
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

#adxzlguwzj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#adxzlguwzj .gt_left {
  text-align: left;
}

#adxzlguwzj .gt_center {
  text-align: center;
}

#adxzlguwzj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#adxzlguwzj .gt_font_normal {
  font-weight: normal;
}

#adxzlguwzj .gt_font_bold {
  font-weight: bold;
}

#adxzlguwzj .gt_font_italic {
  font-style: italic;
}

#adxzlguwzj .gt_super {
  font-size: 65%;
}

#adxzlguwzj .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="adxzlguwzj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style>Store Purchases 2019-2020 (in $M)</th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>For Coronavirus Related Weeks 11-22</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2019</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2020</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Change</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Change%</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">Whisky</td>
      <td class="gt_row gt_right">23.3</td>
      <td class="gt_row gt_right">26.7</td>
      <td class="gt_row gt_right">3.34</td>
      <td class="gt_row gt_right">14.32</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Vodka</td>
      <td class="gt_row gt_right">18.6</td>
      <td class="gt_row gt_right">19.7</td>
      <td class="gt_row gt_right">1.09</td>
      <td class="gt_row gt_right">5.86</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Liqueurs</td>
      <td class="gt_row gt_right">8.2</td>
      <td class="gt_row gt_right">9.3</td>
      <td class="gt_row gt_right">1.04</td>
      <td class="gt_row gt_right">12.65</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Rum</td>
      <td class="gt_row gt_right">8.6</td>
      <td class="gt_row gt_right">9.1</td>
      <td class="gt_row gt_right">0.44</td>
      <td class="gt_row gt_right">5.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Tequila</td>
      <td class="gt_row gt_right">5.7</td>
      <td class="gt_row gt_right">5.8</td>
      <td class="gt_row gt_right">0.14</td>
      <td class="gt_row gt_right">2.45</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Brandy</td>
      <td class="gt_row gt_right">4.0</td>
      <td class="gt_row gt_right">5.3</td>
      <td class="gt_row gt_right">1.28</td>
      <td class="gt_row gt_right">32.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Pre-Mixed</td>
      <td class="gt_row gt_right">1.8</td>
      <td class="gt_row gt_right">3.0</td>
      <td class="gt_row gt_right">1.25</td>
      <td class="gt_row gt_right">71.43</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Specialty</td>
      <td class="gt_row gt_right">2.8</td>
      <td class="gt_row gt_right">2.0</td>
      <td class="gt_row gt_right">-0.79</td>
      <td class="gt_row gt_right">-28.01</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Gin</td>
      <td class="gt_row gt_right">1.9</td>
      <td class="gt_row gt_right">1.9</td>
      <td class="gt_row gt_right">0.01</td>
      <td class="gt_row gt_right">0.52</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Schnapps</td>
      <td class="gt_row gt_right">2.4</td>
      <td class="gt_row gt_right">1.9</td>
      <td class="gt_row gt_right">-0.50</td>
      <td class="gt_row gt_right">-20.49</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Neutral Grain</td>
      <td class="gt_row gt_right">0.2</td>
      <td class="gt_row gt_right">0.4</td>
      <td class="gt_row gt_right">0.25</td>
      <td class="gt_row gt_right">147.06</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Other</td>
      <td class="gt_row gt_right">0.2</td>
      <td class="gt_row gt_right">0.3</td>
      <td class="gt_row gt_right">0.03</td>
      <td class="gt_row gt_right">12.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="background-color: #046C9A; color: white;">Total</td>
      <td class="gt_row gt_right" style="background-color: #046C9A; color: white;">77.8</td>
      <td class="gt_row gt_right" style="background-color: #046C9A; color: white;">85.4</td>
      <td class="gt_row gt_right" style="background-color: #046C9A; color: white;">7.58</td>
      <td class="gt_row gt_right" style="background-color: #046C9A; color: white;">9.74</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->



### Taste Notes 

Whisky (including Bourbon) is the most common liquor type and it remained the go-to tipple of choice during lockdown - with the 14% increase representing the largest Y-Y dollar increase of $3.3M, and represents 44% of the total increase. We will look later at a substitution effect, to see if people changed to whisky from their normal drink. 

Relatively large decreases were seen in Specialty (-28%), which includes packages with glasses or some other promotion, and Schnapps, which I imagine is more of a party (shots) drink. 

Interesting increases are seen in Liqueurs (13%) and a dollar increase almost that of Vodka - and is due to the large increases in Fireball (see below). The largest relative increase is in the Neutral Grain Spirits category which is Everclear and Moonshine type products. I was also surprised to see Brandy sales increased by 32%, but that might be because I think it is awful stuff. Gin was both a smaller percent of sales in absolute terms, and was constant Y-Y. 

## Brand Tastes

The item/product names are varied and inconsistent across years, while the Item# has no apparent logic built in. So in the absence of a mapping table, I made a few changes to the more pominent item names to better align with the brand. (If interested, expand the code below.)


```r
lock_item <- lock %>%
  group_by(Year, Brand) %>%
  summarise(Sales = sum(Sales)/ 1e6) %>%
  ungroup() %>%
  pivot_wider(names_from = Year, values_from = Sales ) %>%
  mutate(Change = round(`2020` - `2019`, 2),
         `Change%` = round(Change / `2019` * 100, 2)) %>%
  arrange(desc(Change))

head(lock_item, 12) %>%
  gt() %>%
    tab_header(
    title = "Top 12 Brand Changes 2019-2020 (in $M)",
    subtitle = "For Coronavirus Related Weeks 11-22"
  ) %>%
    fmt_number(
    columns = vars(`2019`, `2020`),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = wes_cols[3]),
      cell_text(color = "white")
      ),
    locations = cells_body(
      rows = Brand == "Total")
  )
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rxowzbmijh .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
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

#rxowzbmijh .gt_heading {
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

#rxowzbmijh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rxowzbmijh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rxowzbmijh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxowzbmijh .gt_col_headings {
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

#rxowzbmijh .gt_col_heading {
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

#rxowzbmijh .gt_column_spanner_outer {
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

#rxowzbmijh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rxowzbmijh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rxowzbmijh .gt_column_spanner {
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

#rxowzbmijh .gt_group_heading {
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

#rxowzbmijh .gt_empty_group_heading {
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

#rxowzbmijh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rxowzbmijh .gt_from_md > :first-child {
  margin-top: 0;
}

#rxowzbmijh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rxowzbmijh .gt_row {
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

#rxowzbmijh .gt_stub {
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

#rxowzbmijh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxowzbmijh .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rxowzbmijh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxowzbmijh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rxowzbmijh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxowzbmijh .gt_footnotes {
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

#rxowzbmijh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rxowzbmijh .gt_sourcenotes {
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

#rxowzbmijh .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rxowzbmijh .gt_left {
  text-align: left;
}

#rxowzbmijh .gt_center {
  text-align: center;
}

#rxowzbmijh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rxowzbmijh .gt_font_normal {
  font-weight: normal;
}

#rxowzbmijh .gt_font_bold {
  font-weight: bold;
}

#rxowzbmijh .gt_font_italic {
  font-style: italic;
}

#rxowzbmijh .gt_super {
  font-size: 65%;
}

#rxowzbmijh .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rxowzbmijh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal" style>Top 12 Brand Changes 2019-2020 (in $M)</th>
    </tr>
    <tr>
      <th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>For Coronavirus Related Weeks 11-22</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Brand</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2019</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2020</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Change</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Change%</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">Jose Cuervo</td>
      <td class="gt_row gt_right">2.73</td>
      <td class="gt_row gt_right">3.84</td>
      <td class="gt_row gt_right">1.11</td>
      <td class="gt_row gt_right">40.65</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Hennessy</td>
      <td class="gt_row gt_right">1.90</td>
      <td class="gt_row gt_right">2.83</td>
      <td class="gt_row gt_right">0.93</td>
      <td class="gt_row gt_right">49.06</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Tito's</td>
      <td class="gt_row gt_right">3.93</td>
      <td class="gt_row gt_right">4.83</td>
      <td class="gt_row gt_right">0.90</td>
      <td class="gt_row gt_right">22.91</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Black Velvet</td>
      <td class="gt_row gt_right">3.49</td>
      <td class="gt_row gt_right">4.33</td>
      <td class="gt_row gt_right">0.83</td>
      <td class="gt_row gt_right">23.77</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Fireball</td>
      <td class="gt_row gt_right">3.42</td>
      <td class="gt_row gt_right">4.23</td>
      <td class="gt_row gt_right">0.80</td>
      <td class="gt_row gt_right">23.37</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Crown Royal</td>
      <td class="gt_row gt_right">4.65</td>
      <td class="gt_row gt_right">5.15</td>
      <td class="gt_row gt_right">0.51</td>
      <td class="gt_row gt_right">10.98</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">New Amsterdam</td>
      <td class="gt_row gt_right">0.89</td>
      <td class="gt_row gt_right">1.27</td>
      <td class="gt_row gt_right">0.38</td>
      <td class="gt_row gt_right">42.89</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Remy Martin</td>
      <td class="gt_row gt_right">0.82</td>
      <td class="gt_row gt_right">1.10</td>
      <td class="gt_row gt_right">0.27</td>
      <td class="gt_row gt_right">32.80</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Jim Beam</td>
      <td class="gt_row gt_right">1.80</td>
      <td class="gt_row gt_right">2.04</td>
      <td class="gt_row gt_right">0.23</td>
      <td class="gt_row gt_right">12.76</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Blue Ox Vodka</td>
      <td class="gt_row gt_right">0.19</td>
      <td class="gt_row gt_right">0.38</td>
      <td class="gt_row gt_right">0.19</td>
      <td class="gt_row gt_right">101.33</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Hawkeye</td>
      <td class="gt_row gt_right">1.31</td>
      <td class="gt_row gt_right">1.46</td>
      <td class="gt_row gt_right">0.15</td>
      <td class="gt_row gt_right">11.42</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Smirnoff</td>
      <td class="gt_row gt_right">2.18</td>
      <td class="gt_row gt_right">2.33</td>
      <td class="gt_row gt_right">0.15</td>
      <td class="gt_row gt_right">6.89</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->


```r
gg1 <- lock_item %>%
  filter(`2020` > 0.25) %>%
  ggplot(aes(Change, `Change%`, size = `2020`)) +
  geom_point_interactive(aes(tooltip = Brand), color = wes_cols[5], alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.position = "bottom") +
  labs(size = "2020 Sales") +
  labs(title = "Week 11-22 Y-Y Change for Brands Over $250K in 2020 Sales", x = "Change (in $M)", 
       y= "Change %") +
  scale_x_continuous(breaks = seq(-5, 5, by = .1)) +
  scale_y_continuous(breaks = seq(-50, 200, by = 20)) 
  

girafe(ggobj = gg1, height_svg = 3, width_svg = 9, 
       options = list(opts_tooltip(use_fill = TRUE))) 
```

<!--html_preserve--><div id="htmlwidget-3c7fb3437c87667f15a6" style="width:1152px;height:672px;" class="girafe html-widget"></div>
<script type="application/json" data-for="htmlwidget-3c7fb3437c87667f15a6">{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238\" viewBox=\"0 0 648.00 216.00\">\n  <g>\n    <defs>\n      <clipPath id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_1\">\n        <rect x=\"0.00\" y=\"0.00\" width=\"648.00\" height=\"216.00\"/>\n      <\/clipPath>\n    <\/defs>\n    <rect x=\"0.00\" y=\"0.00\" width=\"648.00\" height=\"216.00\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_1\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_1)\" fill=\"#FFFFFF\" fill-opacity=\"1\" stroke-width=\"0.75\" stroke=\"#FFFFFF\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"round\"/>\n    <defs>\n      <clipPath id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_2\">\n        <rect x=\"0.00\" y=\"0.00\" width=\"648.00\" height=\"216.00\"/>\n      <\/clipPath>\n    <\/defs>\n    <defs>\n      <clipPath id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3\">\n        <rect x=\"36.06\" y=\"23.19\" width=\"606.46\" height=\"113.07\"/>\n      <\/clipPath>\n    <\/defs>\n    <polyline points=\"36.06,136.08 642.52,136.08\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_2\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,120.83 642.52,120.83\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_3\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,105.58 642.52,105.58\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_4\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,90.33 642.52,90.33\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_5\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,75.08 642.52,75.08\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_6\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,59.84 642.52,59.84\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_7\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,44.59 642.52,44.59\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_8\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,29.34 642.52,29.34\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_9\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"48.09,136.25 48.09,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_10\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"86.92,136.25 86.92,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_11\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"125.75,136.25 125.75,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_12\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"164.57,136.25 164.57,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_13\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"203.40,136.25 203.40,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_14\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"242.22,136.25 242.22,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_15\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"281.05,136.25 281.05,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_16\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"319.88,136.25 319.88,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_17\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"358.70,136.25 358.70,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_18\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"397.53,136.25 397.53,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_19\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"436.35,136.25 436.35,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_20\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"475.18,136.25 475.18,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_21\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"514.01,136.25 514.01,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_22\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"552.83,136.25 552.83,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_23\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"591.66,136.25 591.66,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_24\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"630.48,136.25 630.48,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_25\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"0.533489\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,128.45 642.52,128.45\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_26\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,113.21 642.52,113.21\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_27\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,97.96 642.52,97.96\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_28\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,82.71 642.52,82.71\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_29\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,67.46 642.52,67.46\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_30\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,52.21 642.52,52.21\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_31\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"36.06,36.96 642.52,36.96\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_32\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"67.51,136.25 67.51,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_33\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"106.33,136.25 106.33,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_34\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"145.16,136.25 145.16,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_35\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"183.99,136.25 183.99,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_36\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"222.81,136.25 222.81,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_37\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"261.64,136.25 261.64,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_38\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"300.46,136.25 300.46,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_39\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"339.29,136.25 339.29,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_40\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"378.12,136.25 378.12,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_41\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"416.94,136.25 416.94,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_42\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"455.77,136.25 455.77,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_43\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"494.59,136.25 494.59,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_44\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"533.42,136.25 533.42,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_45\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"572.25,136.25 572.25,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_46\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <polyline points=\"611.07,136.25 611.07,23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_47\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"none\" stroke-width=\"1.06698\" stroke=\"#EBEBEB\" stroke-opacity=\"1\" stroke-linejoin=\"round\" stroke-linecap=\"butt\"/>\n    <circle cx=\"614.95\" cy=\"74.59\" r=\"4.49pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_48\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Jose Cuervo\"/>\n    <circle cx=\"545.07\" cy=\"68.18\" r=\"3.97pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_49\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Hennessy\"/>\n    <circle cx=\"533.42\" cy=\"88.11\" r=\"4.93pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_50\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Tito&amp;#39;s\"/>\n    <circle cx=\"506.24\" cy=\"87.46\" r=\"4.71pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_51\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Black Velvet\"/>\n    <circle cx=\"494.59\" cy=\"87.76\" r=\"4.67pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_52\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Fireball\"/>\n    <circle cx=\"382.00\" cy=\"97.21\" r=\"5.07pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_53\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Crown Royal\"/>\n    <circle cx=\"331.52\" cy=\"72.88\" r=\"2.89pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_54\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"New Amsterdam\"/>\n    <circle cx=\"288.82\" cy=\"80.57\" r=\"2.73pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_55\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Remy Martin\"/>\n    <circle cx=\"273.29\" cy=\"95.85\" r=\"3.48pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_56\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Jim Beam\"/>\n    <circle cx=\"257.75\" cy=\"28.33\" r=\"1.71pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_57\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Blue Ox Vodka\"/>\n    <circle cx=\"242.22\" cy=\"96.87\" r=\"3.05pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_58\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Hawkeye\"/>\n    <circle cx=\"242.22\" cy=\"100.33\" r=\"3.67pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_59\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Smirnoff\"/>\n    <circle cx=\"238.34\" cy=\"90.10\" r=\"2.45pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_60\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Svedka\"/>\n    <circle cx=\"230.58\" cy=\"61.45\" r=\"1.56pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_61\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Platinum 7x Vodka\"/>\n    <circle cx=\"222.81\" cy=\"62.81\" r=\"1.38pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_62\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Bulleit Bourbon\"/>\n    <circle cx=\"218.93\" cy=\"94.23\" r=\"2.28pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_63\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Admiral Nelson Spiced Rum\"/>\n    <circle cx=\"211.16\" cy=\"79.77\" r=\"1.37pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_64\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Fris Danish Vodka\"/>\n    <circle cx=\"203.40\" cy=\"95.04\" r=\"1.78pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_65\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Baileys Original Irish Cream\"/>\n    <circle cx=\"203.40\" cy=\"102.44\" r=\"2.88pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_66\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Seagram&amp;#39;s\"/>\n    <circle cx=\"199.52\" cy=\"91.80\" r=\"1.23pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_67\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Rumchata\"/>\n    <circle cx=\"195.63\" cy=\"96.83\" r=\"1.45pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_68\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Kahlua Coffee\"/>\n    <circle cx=\"191.75\" cy=\"98.87\" r=\"1.07pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_69\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Canadian Ltd Whisky\"/>\n    <circle cx=\"191.75\" cy=\"99.87\" r=\"1.40pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_70\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Paul Masson Grande Amber Brandy VS\"/>\n    <circle cx=\"187.87\" cy=\"105.06\" r=\"3.07pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_71\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Bacardi\"/>\n    <circle cx=\"183.99\" cy=\"105.58\" r=\"4.72pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_72\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Captain Morgan\"/>\n    <circle cx=\"183.99\" cy=\"105.58\" r=\"2.06pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_73\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Makers Mark\"/>\n    <circle cx=\"183.99\" cy=\"105.58\" r=\"1.31pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_74\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Templeton 4YR Rye\"/>\n    <circle cx=\"180.10\" cy=\"108.11\" r=\"1.44pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_75\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"UV Blue Raspberry\"/>\n    <circle cx=\"176.22\" cy=\"108.86\" r=\"1.87pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_76\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Tanqueray Gin\"/>\n    <circle cx=\"172.34\" cy=\"113.73\" r=\"1.17pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_77\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Five Star\"/>\n    <circle cx=\"160.69\" cy=\"112.93\" r=\"2.07pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_78\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Five O&amp;#39;Clock Vodka\"/>\n    <circle cx=\"156.81\" cy=\"111.51\" r=\"2.45pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_79\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Malibu Coconut Rum\"/>\n    <circle cx=\"156.81\" cy=\"119.43\" r=\"1.52pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_80\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Southern Comfort\"/>\n    <circle cx=\"152.92\" cy=\"119.20\" r=\"1.69pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_81\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Dr McGillicuddys Cherry\"/>\n    <circle cx=\"152.92\" cy=\"122.00\" r=\"1.43pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_82\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Ketel One\"/>\n    <circle cx=\"152.92\" cy=\"110.34\" r=\"2.83pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_83\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Patron\"/>\n    <circle cx=\"149.04\" cy=\"124.87\" r=\"1.25pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_84\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Phillips Vodka\"/>\n    <circle cx=\"133.51\" cy=\"114.43\" r=\"2.61pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_85\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Jameson\"/>\n    <circle cx=\"129.63\" cy=\"109.37\" r=\"3.88pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_86\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Jack Daniels\"/>\n    <circle cx=\"121.86\" cy=\"120.42\" r=\"2.22pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_87\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Jagermeister Liqueur\"/>\n    <circle cx=\"114.10\" cy=\"131.12\" r=\"1.64pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_88\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Barton Vodka\"/>\n    <circle cx=\"114.10\" cy=\"121.94\" r=\"2.22pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_89\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Grey Goose\"/>\n    <circle cx=\"63.62\" cy=\"120.85\" r=\"2.86pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_90\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\" title=\"Absolut\"/>\n    <line x1=\"183.99\" y1=\"136.25\" x2=\"183.99\" y2=\"23.19\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_91\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" stroke-width=\"1.06698\" stroke=\"#7F7F7F\" stroke-opacity=\"1\" stroke-dasharray=\"4,4\" stroke-linejoin=\"round\" stroke-linecap=\"butt\" fill=\"#7F7F7F\" fill-opacity=\"1\"/>\n    <line x1=\"36.06\" y1=\"105.58\" x2=\"642.52\" y2=\"105.58\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_92\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_3)\" stroke-width=\"1.06698\" stroke=\"#7F7F7F\" stroke-opacity=\"1\" stroke-dasharray=\"4,4\" stroke-linejoin=\"round\" stroke-linecap=\"butt\" fill=\"#7F7F7F\" fill-opacity=\"1\"/>\n    <defs>\n      <clipPath id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4\">\n        <rect x=\"0.00\" y=\"0.00\" width=\"648.00\" height=\"216.00\"/>\n      <\/clipPath>\n    <\/defs>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"18.41\" y=\"131.60\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_93\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">-30<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"18.41\" y=\"116.36\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_94\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">-10<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"21.34\" y=\"101.11\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_95\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">10<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"21.34\" y=\"85.86\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_96\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">30<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"21.34\" y=\"70.61\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_97\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">50<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"21.34\" y=\"55.36\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_98\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">70<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"21.34\" y=\"40.11\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_99\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">90<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(65.38,157.47) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_100\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">-0.3<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(104.21,157.47) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_101\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">-0.2<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(143.03,157.47) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_102\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">-0.1<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(183.32,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_103\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.0<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(222.15,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_104\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.1<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(260.98,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_105\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.2<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(299.80,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_106\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.3<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(338.63,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_107\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.4<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(377.45,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_108\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.5<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(416.28,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_109\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.6<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(455.11,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_110\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.7<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(493.93,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_111\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.8<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(532.76,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_112\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">0.9<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(571.58,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_113\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">1.0<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(610.41,154.93) rotate(-60)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_114\" font-size=\"6.60pt\" fill=\"#4D4D4D\" fill-opacity=\"1\" font-family=\"Arial\">1.1<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"301.38\" y=\"169.01\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_115\" font-size=\"8.25pt\" font-family=\"Arial\">Change (in $M)<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text transform=\"translate(13.35,105.40) rotate(-90)\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_116\" font-size=\"8.25pt\" font-family=\"Arial\">Change %<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"228.94\" y=\"200.34\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_117\" font-size=\"8.25pt\" font-family=\"Arial\">2020 Sales<\/text>\n    <\/g>\n    <circle cx=\"298.10\" cy=\"196.40\" r=\"2.63pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_118\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\"/>\n    <circle cx=\"331.23\" cy=\"196.40\" r=\"3.46pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_119\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\"/>\n    <circle cx=\"364.36\" cy=\"196.40\" r=\"4.06pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_120\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\"/>\n    <circle cx=\"397.50\" cy=\"196.40\" r=\"4.56pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_121\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\"/>\n    <circle cx=\"430.63\" cy=\"196.40\" r=\"5.00pt\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_122\" clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\" fill=\"#000000\" fill-opacity=\"0.9\" stroke-width=\"0.708661\" stroke=\"#000000\" stroke-opacity=\"0.9\" stroke-linejoin=\"round\" stroke-linecap=\"round\"/>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"312.22\" y=\"199.55\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_123\" font-size=\"6.60pt\" font-family=\"Arial\">1<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"345.35\" y=\"199.55\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_124\" font-size=\"6.60pt\" font-family=\"Arial\">2<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"378.48\" y=\"199.55\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_125\" font-size=\"6.60pt\" font-family=\"Arial\">3<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"411.62\" y=\"199.55\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_126\" font-size=\"6.60pt\" font-family=\"Arial\">4<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"444.75\" y=\"199.55\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_127\" font-size=\"6.60pt\" font-family=\"Arial\">5<\/text>\n    <\/g>\n    <g clip-path=\"url(#svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_cl_4)\">\n      <text x=\"36.06\" y=\"14.93\" id=\"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238_el_128\" font-size=\"9.90pt\" font-family=\"Arial\">Week 11-22 Y-Y Change for Brands Over $250K in 2020 Sales<\/text>\n    <\/g>\n  <\/g>\n<\/svg>\n","js":null,"uid":"svg_eaa4b902-9d3d-4e3f-a785-a53ed5855238","ratio":3,"settings":{"tooltip":{"css":" .tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px 2px 2px 2px ; position:absolute;pointer-events:none;z-index:999;}\n","offx":10,"offy":0,"use_cursor_pos":true,"opacity":0.9,"usefill":true,"usestroke":false,"delay":{"over":200,"out":500}},"hover":{"css":" .hover_SVGID_ { fill:orange;stroke:gray; }\n"},"hoverkey":{"css":" .hover_key_SVGID_ { stroke:red; }\n"},"hovertheme":{"css":" .hover_theme_SVGID_ { fill:green; }\n"},"zoom":{"min":1,"max":1},"capture":{"css":" .selected_SVGID_ { fill:red;stroke:gray; }\n","type":"multiple","only_shiny":true,"selected":[]},"capturekey":{"css":" .selected_key_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"capturetheme":{"css":" .selected_theme_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"toolbar":{"position":"topright","saveaspng":true},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

By hovering over the above chart, we can easily see that Absolut and Jack Daniels were the most negatively impacted as they both say declining Y-Y sales despite the overal market increase.  

## Size Tastes

Here we look to see if stores ordered larger than normal sizes.


```r
gal <- lock %>%
  group_by(Year, BottleVol) %>%
  summarise(Sales = sum(Sales)/1e6) %>%
  ungroup() %>%
  mutate(BottleVol = factor(BottleVol)) %>%
  pivot_wider(names_from = Year, values_from = Sales, values_fill = 0)

ggplot(gal) +
  geom_segment(aes(x = BottleVol, xend = BottleVol, y = `2019`, yend = `2020`), color = "grey80",
               arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point( aes(x=BottleVol, y=`2019`), color= wes_cols[4], size=4 ) +
  geom_point( aes(x=BottleVol, y=`2020`), color= wes_cols[3], size=4, alpha = 0.5 ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.position = "bottom", legend.title=element_blank()) +
  labs(title = "2019 to 2020 Change in Sales by Bottle Size", x = "Bottle Volume (in ml)", 
       y= "Sales (in $M)") +
  scale_y_continuous(breaks = seq(0, 50, by = 2), expand = c(0, 0.1)) +
  coord_flip()
```

<img src="iowa_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

## Major Sizes


```r
gal2 <- lock %>%
  filter(BottleVol %in% c(375, 750, 1000, 1750),
         Brand %in% head(top$Brand, 8)) %>%
  group_by(Year, Brand, BottleVol) %>%
  summarise(Sales = sum(Sales)/1e6) %>%
  ungroup() %>%
  mutate(BottleVol = factor(BottleVol)) %>%
  pivot_wider(names_from = Year, values_from = Sales, values_fill = 0)


ggplot(gal2) +
  geom_segment(aes(x = BottleVol, xend = BottleVol, y = `2019`, yend = `2020`), color = "grey80",
               arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point( aes(x=BottleVol, y=`2019`), color= wes_cols[4], size=4 ) +
  geom_point( aes(x=BottleVol, y=`2020`), color= wes_cols[3], size=4, alpha = 0.5 ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.position = "bottom", legend.title=element_blank()) +
  labs(title = "2019 to 2020 Change in Sales by Bottle Size",
       subtitle = "Top 8 Brands by 2020 Sales", x = "Bottle Volume (in ml)", 
       y= "Sales (in $M)") +
  scale_y_continuous(breaks = seq(0, 50, by = .5), expand = c(0, 0.1)) +
  coord_flip() +
  facet_wrap(~Brand, nrow = 2)
```

<img src="iowa_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />


# Pricing Lawsuits

According to the [Des Moines Register](https://www.desmoinesregister.com/story/news/2019/12/17/iowa-illegally-marked-up-liquor-prices-years-lawsuit-alleges/2680983001/), *"The [Iowa Alcoholic Beverages Division (ABD)] is the state's exclusive liquor wholesaler. It buys liquor from suppliers and can mark up the price by 50% under Iowa law before selling it to retailers, such as grocery stores, liquor stores and other liquor license holders"*

The Register also adds that there has been a whistleblower suit filed about complaints that the state's markup was larger than the legislated 50% maximum. There is a related lawsuit [here](https://www.courthousenews.com/wp-content/uploads/2019/12/Polk-LACL-683.pdf). So, with that context, and a nicely relevant dataset, I thought it would be worth taking a look at pricing. 

## Pricing Data

We are provided with two pricing fields, `UnitCost` which is the price to the ABD from the manufacturer, and the `UnitPrice` which is the marked-up price charged to retailers. However, it does appears that the given `UnitPrice` does not always equal the total Sales divided by the Units purchased which is surely a more plausible figure than the published price. 


```r
prices <- liq %>%
  mutate(CalcPrice = Sales / Units,
         PriceDiff = CalcPrice - UnitPrice,
         NomMkup = (UnitPrice / UnitCost -1) * 100,
         CalcMkup = round((CalcPrice / UnitCost -1) * 100, 1),
         Over = PriceDiff * Units,
         Store = str_sub(Store, 1, 25),
         Item = str_sub(Item, 1, 25)) %>%
  select(Inv, InvLine, Date, Store, Item, UnitCost, UnitPrice, NomMkup, Units,
         Sales, CalcPrice, CalcMkup, PriceDiff, Over)
```

If we define the Nominal Markup as (`UnitPrice` / `UnitCost` -1), then we can see there are  3.405M transactions (99.8%) that were nominally markedup at the maximum 50%. If we define Actual Markup as (`CalcPrice` / `UnitCost` -1), it was 50% 3.302M times or 96.7% of the time. Below we take a look at the distribution of calculated markups that are NOT exactly 50%.


```r
prices2 <- prices %>%
  filter(round(CalcMkup, 0) != 50)

prices2 %>%
  ggplot(aes(CalcMkup)) +
  geom_histogram(binwidth = 5, fill = wes_cols[2], color = wes_cols[1]) +
  geom_vline(xintercept = 50) +
  xlim(-75, 200) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.position = "bottom") +
  labs(size = "2020 Sales") +
  labs(title = "Markup Distribution (Excl 50% Markup Exactly)", x = "Markup Bin", 
       y= "Count") +
  scale_x_continuous(breaks = seq(-75, 200, by = 10), limits = c(-75, 200)) +
  scale_y_continuous(breaks = seq(0, 2e5, by = 5e3)) 
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
```

<img src="iowa_files/figure-html/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Of the 109.6K transactions that are not marked-up exactly 50%, we can see that 62.1% are less than the statutory limit. This is permissable, so we will remove these and just examine the transactions over the 50% limit. 


```r
prices3 <- prices2 %>%
  filter(CalcMkup > 50)
```


### Invoices


```r
prices4 <- prices %>%
  group_by(Inv) %>%
  summarise(Over = sum(Over)) %>%
  arrange(desc(Over))
```

