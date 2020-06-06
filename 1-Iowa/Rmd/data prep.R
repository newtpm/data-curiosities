library(tidyverse)
library(lubridate)
library(vroom)

liq <- vroom("../input/Iowa_Liquor_Sales.csv") %>%
  select(Inv = `Invoice/Item Number`, Date, StoreNo = `Store Number`, 
         Store = `Store Name`, City, Zip = `Zip Code`, County, Location = `Store Location`,
         Cat = `Category Name`, Vendor = `Vendor Name`, ItemNo = `Item Number`,
         Item = `Item Description`, BottleVol = `Bottle Volume (ml)`, UnitCost = `State Bottle Cost`,
         UnitPrice = `State Bottle Retail`, Sales = `Sale (Dollars)`,
         Units = `Bottles Sold`, SalesVol = `Volume Sold (Liters)`) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  filter(Date >= "2019-01-01") %>%
  separate(Store, c("Store"), sep = " /", extra = "drop" ) %>% # remove everything after "/"
  separate(Location, c("Point", "Long", "Lat"), sep = " ", extra = "merge" ) %>% # derive Lat?Long
  mutate(Lat = str_remove_all(Lat, "[()]"),
         Long = str_remove_all(Long, "[()]"),
         Year = year(Date),
         Month = month(Date, label = T),
         Week = week(Date),
         Day = wday(Date, label = T),
         County = str_to_title(County),
         Vendor = str_to_title(Vendor),
         ItemNo = factor(ItemNo)) %>%
  mutate(Type = case_when( # 44 Cats in source so need to streamline
    str_detect(str_to_lower(Cat), "liqueur") ~ "Liqueurs",
    str_detect(str_to_lower(Cat), "brand") ~ "Brandy",
    str_detect(str_to_lower(Cat), "schnapp") ~ "Schnapps",
    str_detect(str_to_lower(Cat), "special") ~ "Specialty",
    str_detect(str_to_lower(Cat), "cocktail") ~ "Pre-Mixed",
    str_detect(str_to_lower(Cat), "neutral grain") ~ "Neutral Grain",
    str_detect(str_to_lower(Cat), "vodka") ~ "Vodka",
    str_detect(str_to_lower(Cat), "gin") ~ "Gin",
    str_detect(str_to_lower(Cat), "rum") ~ "Rum",
    str_detect(str_to_lower(Cat), "whisk|scotch|bourbon") ~ "Whisky",
    str_detect(str_to_lower(Cat), "mezcal|tequila") ~ "Tequila",
    TRUE ~ "Other" )) %>%
  mutate(Source = case_when(
    str_detect(str_to_lower(Cat), "imported|scotch|canadian|irish|tequila") ~ "Imported",
    str_detect(str_to_lower(Cat), "american|tennessee") ~ "American",
    TRUE ~ "Unknown" )) %>%
  mutate(Brand = case_when(
    str_detect(str_to_lower(Item), "captain morgan") ~ "Captain Morgan",
    str_detect(str_to_lower(Item), "fireball") ~ "Fireball",
    str_detect(str_to_lower(Item), "hennessy") ~ "Hennessy",
    str_detect(str_to_lower(Item), "tito") ~ "Tito's",
    str_detect(str_to_lower(Item), "smirnoff") ~ "Smirnoff",
    str_detect(str_to_lower(Item), "crown royal") ~ "Crown Royal",
    str_detect(str_to_lower(Item), "black velvet") ~ "Black Velvet",
    str_detect(str_to_lower(Item), "jim beam") ~ "Jim Beam",
    str_detect(str_to_lower(Item), "everclear") ~ "Everclear",
    str_detect(str_to_lower(Item), "remy martin") ~ "Remy Martin",
    str_detect(str_to_lower(Item), "bacardi") ~ "Bacardi",
    str_detect(str_to_lower(Item), "patron") ~ "Patron",
    str_detect(str_to_lower(Item), "svedka") ~ "Svedka",
    str_detect(str_to_lower(Item), "absolut") ~ "Absolut",
    str_detect(str_to_lower(Item), "seagram") ~ "Seagram's",
    str_detect(str_to_lower(Item), "jack daniel") ~ "Jack Daniels",
    str_detect(str_to_lower(Item), "new amsterdam") ~ "New Amsterdam",
    str_detect(str_to_lower(Item), "hawkeye") ~ "Hawkeye",
    str_detect(str_to_lower(Item), "jose cuervo|1800") ~ "Jose Cuervo",
    TRUE ~ Item )) %>%
  select(-Point)

write_rds(liq, "../input/iowa_liq_short.Rds") %>%
  gc()
