Iowa Liquor Sales Analysis
================

<style type="text/css">
.main-container {
  max-width: 1250px;
  margin-left: auto;
  margin-right: auto;
}
</style>

# Objective

The State of Iowa has published a really interesting dataset around
their liquor sales. I thought it would be interesting to see how
sales/consumption patterns changed as a result of coronavirus. The data
is for wholesale sales from the distributor to the retail outlet so is
not directly indicative of consumer patterns, but our period of interest
is 11 weeks so we feel it is as good a proxy as we can expect.

Source data is 19M purchases by Iowa Class “E” liquor licensees, dating
back to 2012, and can be downloaded from
[data.iowa.gov](https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy).

``` r
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

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ygujeghbpr .gt_table {
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

#ygujeghbpr .gt_heading {
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

#ygujeghbpr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ygujeghbpr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ygujeghbpr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ygujeghbpr .gt_col_headings {
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

#ygujeghbpr .gt_col_heading {
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

#ygujeghbpr .gt_column_spanner_outer {
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

#ygujeghbpr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ygujeghbpr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ygujeghbpr .gt_column_spanner {
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

#ygujeghbpr .gt_group_heading {
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

#ygujeghbpr .gt_empty_group_heading {
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

#ygujeghbpr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ygujeghbpr .gt_from_md > :first-child {
  margin-top: 0;
}

#ygujeghbpr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ygujeghbpr .gt_row {
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

#ygujeghbpr .gt_stub {
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

#ygujeghbpr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ygujeghbpr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ygujeghbpr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ygujeghbpr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ygujeghbpr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ygujeghbpr .gt_footnotes {
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

#ygujeghbpr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ygujeghbpr .gt_sourcenotes {
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

#ygujeghbpr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ygujeghbpr .gt_left {
  text-align: left;
}

#ygujeghbpr .gt_center {
  text-align: center;
}

#ygujeghbpr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ygujeghbpr .gt_font_normal {
  font-weight: normal;
}

#ygujeghbpr .gt_font_bold {
  font-weight: bold;
}

#ygujeghbpr .gt_font_italic {
  font-style: italic;
}

#ygujeghbpr .gt_super {
  font-size: 65%;
}

#ygujeghbpr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ygujeghbpr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Inv

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Date

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StoreNo

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Store

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

City

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Zip

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

County

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Long

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Lat

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Cat

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Vendor

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

ItemNo

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Item

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

BottleVol

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

UnitCost

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

UnitPrice

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Sales

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Units

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SalesVol

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Year

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Month

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Week

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Day

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Type

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Source

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Brand

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

InvLine

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

217962

</td>

<td class="gt_row gt_left">

2019-09-10

</td>

<td class="gt_row gt_right">

2604

</td>

<td class="gt_row gt_left">

Hy-Vee Wine and Spirits

</td>

<td class="gt_row gt_left">

Le Mars

</td>

<td class="gt_row gt_right">

51031

</td>

<td class="gt_row gt_left">

Plymouth

</td>

<td class="gt_row gt_left">

\-96.18335000000002

</td>

<td class="gt_row gt_left">

42.778257

</td>

<td class="gt_row gt_left">

American Vodkas

</td>

<td class="gt_row gt_left">

Laird & Company

</td>

<td class="gt_row gt_right">

35926

</td>

<td class="gt_row gt_left">

Five O’Clock Vodka PET

</td>

<td class="gt_row gt_right">

750

</td>

<td class="gt_row gt_right">

3.37

</td>

<td class="gt_row gt_right">

5.06

</td>

<td class="gt_row gt_right">

60.72

</td>

<td class="gt_row gt_right">

12

</td>

<td class="gt_row gt_right">

9

</td>

<td class="gt_row gt_right">

2019

</td>

<td class="gt_row gt_center">

Sep

</td>

<td class="gt_row gt_right">

37

</td>

<td class="gt_row gt_center">

Tue

</td>

<td class="gt_row gt_left">

Vodka

</td>

<td class="gt_row gt_left">

American

</td>

<td class="gt_row gt_left">

Five O’Clock Vodka PET

</td>

<td class="gt_row gt_left">

00021

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

# Exploratory Data Analysis

Our reduced data set is from 2019-01-02 through 2020-05-29 and contains
3.41M rows and totals $495.01M in wholesale sales or about 31.56M litres
of booze.

### Disclaimer

I know nothing about the liquor industry (apart from what I learned as a
consumer). Consequently, I may have things wrong here - so this analysis
is for curiosity purposes only.

Also, this data is just liquor sales for the state of Iowa… so I
recognize that sales patterns may differ elsewhere and beer/wine
substitutions are ignored.

## Conronavirus Lockdown

Iowa never actually issued a “Stay-at-Home” order, but business were
restricted, some employees were laid off, and employers told their staff
to work from home where possible. For our analysis, we assume any
coronovirus impact started with Week 11 (Monday March 11, 2020) and
continuing to Week 22 (which is the extent of our dataset). For
convenience we will call this the “Lockdown Period”.

### Sales by Week

Judging from the Year over Year sales, there is a noticeable increase in
sales during the lockdown period.

``` r
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

<img src="README_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## Top 2020 Brands

``` r
top <- liq %>% 
  group_by(Brand) %>%
  summarise(Sales = sum(Sales)/1e6) %>%
  arrange(desc(Sales))
```

### Category Tastes

We can determine that overall sales increased almost 10% over the same
period in 2019, but what is the composition of that increase. As this
was not a typical Y-Y increase, it is interesting to see how tastes
changed given that people were going to be more home-bound than normal.

``` r
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

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wzdqsdylsa .gt_table {
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

#wzdqsdylsa .gt_heading {
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

#wzdqsdylsa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wzdqsdylsa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wzdqsdylsa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wzdqsdylsa .gt_col_headings {
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

#wzdqsdylsa .gt_col_heading {
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

#wzdqsdylsa .gt_column_spanner_outer {
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

#wzdqsdylsa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wzdqsdylsa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wzdqsdylsa .gt_column_spanner {
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

#wzdqsdylsa .gt_group_heading {
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

#wzdqsdylsa .gt_empty_group_heading {
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

#wzdqsdylsa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wzdqsdylsa .gt_from_md > :first-child {
  margin-top: 0;
}

#wzdqsdylsa .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wzdqsdylsa .gt_row {
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

#wzdqsdylsa .gt_stub {
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

#wzdqsdylsa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wzdqsdylsa .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wzdqsdylsa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wzdqsdylsa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wzdqsdylsa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wzdqsdylsa .gt_footnotes {
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

#wzdqsdylsa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wzdqsdylsa .gt_sourcenotes {
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

#wzdqsdylsa .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wzdqsdylsa .gt_left {
  text-align: left;
}

#wzdqsdylsa .gt_center {
  text-align: center;
}

#wzdqsdylsa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wzdqsdylsa .gt_font_normal {
  font-weight: normal;
}

#wzdqsdylsa .gt_font_bold {
  font-weight: bold;
}

#wzdqsdylsa .gt_font_italic {
  font-style: italic;
}

#wzdqsdylsa .gt_super {
  font-size: 65%;
}

#wzdqsdylsa .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="wzdqsdylsa" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="5" class="gt_heading gt_title gt_font_normal" style>

Store Purchases 2019-2020 (in $M)

</th>

</tr>

<tr>

<th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

For Coronavirus Related Weeks 11-22

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Type

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

2019

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

2020

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Change

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Change%

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Whisky

</td>

<td class="gt_row gt_right">

23.3

</td>

<td class="gt_row gt_right">

26.7

</td>

<td class="gt_row gt_right">

3.34

</td>

<td class="gt_row gt_right">

14.32

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Vodka

</td>

<td class="gt_row gt_right">

18.6

</td>

<td class="gt_row gt_right">

19.7

</td>

<td class="gt_row gt_right">

1.09

</td>

<td class="gt_row gt_right">

5.86

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Liqueurs

</td>

<td class="gt_row gt_right">

8.2

</td>

<td class="gt_row gt_right">

9.3

</td>

<td class="gt_row gt_right">

1.04

</td>

<td class="gt_row gt_right">

12.65

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Rum

</td>

<td class="gt_row gt_right">

8.6

</td>

<td class="gt_row gt_right">

9.1

</td>

<td class="gt_row gt_right">

0.44

</td>

<td class="gt_row gt_right">

5.11

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Tequila

</td>

<td class="gt_row gt_right">

5.7

</td>

<td class="gt_row gt_right">

5.8

</td>

<td class="gt_row gt_right">

0.14

</td>

<td class="gt_row gt_right">

2.45

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Brandy

</td>

<td class="gt_row gt_right">

4.0

</td>

<td class="gt_row gt_right">

5.3

</td>

<td class="gt_row gt_right">

1.28

</td>

<td class="gt_row gt_right">

32.00

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Pre-Mixed

</td>

<td class="gt_row gt_right">

1.8

</td>

<td class="gt_row gt_right">

3.0

</td>

<td class="gt_row gt_right">

1.25

</td>

<td class="gt_row gt_right">

71.43

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Specialty

</td>

<td class="gt_row gt_right">

2.8

</td>

<td class="gt_row gt_right">

2.0

</td>

<td class="gt_row gt_right">

\-0.79

</td>

<td class="gt_row gt_right">

\-28.01

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Gin

</td>

<td class="gt_row gt_right">

1.9

</td>

<td class="gt_row gt_right">

1.9

</td>

<td class="gt_row gt_right">

0.01

</td>

<td class="gt_row gt_right">

0.52

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Schnapps

</td>

<td class="gt_row gt_right">

2.4

</td>

<td class="gt_row gt_right">

1.9

</td>

<td class="gt_row gt_right">

\-0.50

</td>

<td class="gt_row gt_right">

\-20.49

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Neutral Grain

</td>

<td class="gt_row gt_right">

0.2

</td>

<td class="gt_row gt_right">

0.4

</td>

<td class="gt_row gt_right">

0.25

</td>

<td class="gt_row gt_right">

147.06

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Other

</td>

<td class="gt_row gt_right">

0.2

</td>

<td class="gt_row gt_right">

0.3

</td>

<td class="gt_row gt_right">

0.03

</td>

<td class="gt_row gt_right">

12.00

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="background-color: #046C9A; color: white;">

Total

</td>

<td class="gt_row gt_right" style="background-color: #046C9A; color: white;">

77.8

</td>

<td class="gt_row gt_right" style="background-color: #046C9A; color: white;">

85.4

</td>

<td class="gt_row gt_right" style="background-color: #046C9A; color: white;">

7.58

</td>

<td class="gt_row gt_right" style="background-color: #046C9A; color: white;">

9.74

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

### Taste Notes

Whisky (including Bourbon) is the most common liquor type and it
remained the go-to tipple of choice during lockdown - with the 14%
increase representing the largest Y-Y dollar increase of $3.3M, and
represents 44% of the total increase. We will look later at a
substitution effect, to see if people changed to whisky from their
normal drink.

Relatively large decreases were seen in Specialty (-28%), which includes
packages with glasses or some other promotion, and Schnapps, which I
imagine is more of a party (shots) drink.

Interesting increases are seen in Liqueurs (13%) and a dollar increase
almost that of Vodka - and is due to the large increases in Fireball
(see below). The largest relative increase is in the Neutral Grain
Spirits category which is Everclear and Moonshine type products. I was
also surprised to see Brandy sales increased by 32%, but that might be
because I think it is awful stuff. Gin was both a smaller percent of
sales in absolute terms, and was constant Y-Y.

## Brand Tastes

The item/product names are varied and inconsistent across years, while
the Item\# has no apparent logic built in. So in the absence of a
mapping table, I made a few changes to the more pominent item names to
better align with the brand. (If interested, expand the code below.)

``` r
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

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ynrynutjse .gt_table {
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

#ynrynutjse .gt_heading {
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

#ynrynutjse .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ynrynutjse .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ynrynutjse .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ynrynutjse .gt_col_headings {
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

#ynrynutjse .gt_col_heading {
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

#ynrynutjse .gt_column_spanner_outer {
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

#ynrynutjse .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ynrynutjse .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ynrynutjse .gt_column_spanner {
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

#ynrynutjse .gt_group_heading {
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

#ynrynutjse .gt_empty_group_heading {
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

#ynrynutjse .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ynrynutjse .gt_from_md > :first-child {
  margin-top: 0;
}

#ynrynutjse .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ynrynutjse .gt_row {
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

#ynrynutjse .gt_stub {
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

#ynrynutjse .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ynrynutjse .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ynrynutjse .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ynrynutjse .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ynrynutjse .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ynrynutjse .gt_footnotes {
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

#ynrynutjse .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ynrynutjse .gt_sourcenotes {
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

#ynrynutjse .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ynrynutjse .gt_left {
  text-align: left;
}

#ynrynutjse .gt_center {
  text-align: center;
}

#ynrynutjse .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ynrynutjse .gt_font_normal {
  font-weight: normal;
}

#ynrynutjse .gt_font_bold {
  font-weight: bold;
}

#ynrynutjse .gt_font_italic {
  font-style: italic;
}

#ynrynutjse .gt_super {
  font-size: 65%;
}

#ynrynutjse .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ynrynutjse" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="5" class="gt_heading gt_title gt_font_normal" style>

Top 12 Brand Changes 2019-2020 (in $M)

</th>

</tr>

<tr>

<th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

For Coronavirus Related Weeks 11-22

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Brand

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

2019

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

2020

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Change

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Change%

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Jose Cuervo

</td>

<td class="gt_row gt_right">

2.73

</td>

<td class="gt_row gt_right">

3.84

</td>

<td class="gt_row gt_right">

1.11

</td>

<td class="gt_row gt_right">

40.65

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Hennessy

</td>

<td class="gt_row gt_right">

1.90

</td>

<td class="gt_row gt_right">

2.83

</td>

<td class="gt_row gt_right">

0.93

</td>

<td class="gt_row gt_right">

49.06

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Tito’s

</td>

<td class="gt_row gt_right">

3.93

</td>

<td class="gt_row gt_right">

4.83

</td>

<td class="gt_row gt_right">

0.90

</td>

<td class="gt_row gt_right">

22.91

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Black Velvet

</td>

<td class="gt_row gt_right">

3.49

</td>

<td class="gt_row gt_right">

4.33

</td>

<td class="gt_row gt_right">

0.83

</td>

<td class="gt_row gt_right">

23.77

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Fireball

</td>

<td class="gt_row gt_right">

3.42

</td>

<td class="gt_row gt_right">

4.23

</td>

<td class="gt_row gt_right">

0.80

</td>

<td class="gt_row gt_right">

23.37

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Crown Royal

</td>

<td class="gt_row gt_right">

4.65

</td>

<td class="gt_row gt_right">

5.15

</td>

<td class="gt_row gt_right">

0.51

</td>

<td class="gt_row gt_right">

10.98

</td>

</tr>

<tr>

<td class="gt_row gt_left">

New Amsterdam

</td>

<td class="gt_row gt_right">

0.89

</td>

<td class="gt_row gt_right">

1.27

</td>

<td class="gt_row gt_right">

0.38

</td>

<td class="gt_row gt_right">

42.89

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Remy Martin

</td>

<td class="gt_row gt_right">

0.82

</td>

<td class="gt_row gt_right">

1.10

</td>

<td class="gt_row gt_right">

0.27

</td>

<td class="gt_row gt_right">

32.80

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Jim Beam

</td>

<td class="gt_row gt_right">

1.80

</td>

<td class="gt_row gt_right">

2.04

</td>

<td class="gt_row gt_right">

0.23

</td>

<td class="gt_row gt_right">

12.76

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Blue Ox Vodka

</td>

<td class="gt_row gt_right">

0.19

</td>

<td class="gt_row gt_right">

0.38

</td>

<td class="gt_row gt_right">

0.19

</td>

<td class="gt_row gt_right">

101.33

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Hawkeye

</td>

<td class="gt_row gt_right">

1.31

</td>

<td class="gt_row gt_right">

1.46

</td>

<td class="gt_row gt_right">

0.15

</td>

<td class="gt_row gt_right">

11.42

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Smirnoff

</td>

<td class="gt_row gt_right">

2.18

</td>

<td class="gt_row gt_right">

2.33

</td>

<td class="gt_row gt_right">

0.15

</td>

<td class="gt_row gt_right">

6.89

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

``` r
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

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

By hovering over the above chart, we can easily see that Absolut and
Jack Daniels were the most negatively impacted as they both say
declining Y-Y sales despite the overal market increase.

## Size Tastes

Here we look to see if stores ordered larger than normal sizes.

``` r
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

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

## Major Sizes

``` r
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

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

# Pricing Lawsuits

According to the [Des Moines
Register](https://www.desmoinesregister.com/story/news/2019/12/17/iowa-illegally-marked-up-liquor-prices-years-lawsuit-alleges/2680983001/),
*“The \[Iowa Alcoholic Beverages Division (ABD)\] is the state’s
exclusive liquor wholesaler. It buys liquor from suppliers and can mark
up the price by 50% under Iowa law before selling it to retailers, such
as grocery stores, liquor stores and other liquor license holders”*

The Register also adds that there has been a whistleblower suit filed
about complaints that the state’s markup was larger than the legislated
50% maximum. There is a related lawsuit
[here](https://www.courthousenews.com/wp-content/uploads/2019/12/Polk-LACL-683.pdf).
So, with that context, and a nicely relevant dataset, I thought it would
be worth taking a look at pricing.

## Pricing Data

We are provided with two pricing fields, `UnitCost` which is the price
to the ABD from the manufacturer, and the `UnitPrice` which is the
marked-up price charged to retailers. However, it does appears that the
given `UnitPrice` does not always equal the total Sales divided by the
Units purchased which is surely a more plausible figure than the
published price.

``` r
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

If we define the Nominal Markup as (`UnitPrice` / `UnitCost` -1), then
we can see there are 3.405M transactions (99.8%) that were nominally
markedup at the maximum 50%. If we define Actual Markup as (`CalcPrice`
/ `UnitCost` -1), it was 50% 3.302M times or 96.7% of the time. Below we
take a look at the distribution of calculated markups that are NOT
exactly 50%.

``` r
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

    ## Scale for 'x' is already present. Adding another scale for 'x', which will
    ## replace the existing scale.

<img src="README_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Of the 109.6K transactions that are not marked-up exactly 50%, we can
see that 62.1% are less than the statutory limit. This is permissable,
so we will remove these and just examine the transactions over the 50%
limit.

``` r
prices3 <- prices2 %>%
  filter(CalcMkup > 50)
```

### Invoices

``` r
prices4 <- prices %>%
  group_by(Inv) %>%
  summarise(Over = sum(Over)) %>%
  arrange(desc(Over))
```
