## crew prepack colors
library(tidyverse)
library(data.table)
library(sqldf)

## read orders data 
crew_orders <- readxl::read_xlsx("C:/Users/joshua.mark/OneDrive - Accenture/Desktop/CREW/Tableau Data/CREW_orders.xlsx", 
                                 # sheet = 'Updated Order Data') %>% data.frame()
                                 sheet = 1) %>% data.frame()

## read sku_key data 
sku_key <- readxl::read_xlsx("C:/Users/joshua.mark/OneDrive - Accenture/Desktop/CREW/Tableau Data/PPK_key.xlsx", 
                             sheet = 1) %>% data.frame()

## create orders: item level orders df 
orders <- crew_orders %>%  
  filter(Key.Item %in% c('Chubby Bottle', 'Crew Bottle', 'Food52 Combo', 'Pour Spout')) %>% 
  mutate(ordered_sku = ifelse(!is.na(PPK.SKU), PPK.SKU, SKU.1)) %>% 
  mutate(ordered_qty = ifelse(!is.na(PPK.QTY), PPK.QTY, SKU.1.QTY)) %>% 
  select(ERP, Order.Date, Category, Key.Item, Pack.Configuration, ordered_sku, ordered_qty)

#### SINGLE ORDERS ####
## filter to singles orders (ppk_config == single)
single_orders <- orders %>% filter(Pack.Configuration == 'Single')

## sku information for all SINGLE SKUs
## PPK.SKU n/a
single_skus <- sku_key %>% 
  filter(PPK.SKU == 'n/a') %>% 
  select(sku = SKU.1, Full.Description, Style, Color, PPK.SKU) %>% 
  distinct()

## merge singles orders with SKU information 
single_orders <- single_orders %>% 
  merge(single_skus
        ,by.x = 'ordered_sku'
        ,by.y = 'sku'
        ,all.x = TRUE) %>% 
  select(ERP
         ,Order.Date
         ,Category
         ,PPK.SKU
         ,ordered_sku
         ,ordered_qty
         ,Key.Item
         ,unit_color = Color
         ,Pack.Configuration
         ,Style
         ,Full.Description)

#### PREPACK ORDERS #####
## prepack orders df
prepack_orders <- orders %>% filter(Pack.Configuration != 'Single')

## sku information for pre-packed SKUs 
## qty and colors in each ppk 
prepack_skus <- sku_key %>% 
  filter(PPK.SKU != 'n/a') %>% 
  select(-Key.Item, -Pack.Configuration)

## merge prepack orders with prepack SKU information
## get the individual SKUs in each order 
prepack_orders <- prepack_orders %>% 
  merge(prepack_skus, by.x = 'ordered_sku', by.y = 'PPK.SKU', all.x = TRUE) %>% 
  mutate(sku1 = paste(SKU.1, SKU.1.QTY, sep = '_'), 
         sku2 = paste(SKU.2, SKU.2.QTY, sep = '_'), 
         sku3 = paste(SKU.3, SKU.3.QTY, sep = '_')
         # sku4 = paste(SKU.4, SKU.4.QTY, sep = '_'), 
         # sku5 = paste(SKU.5, SKU.5.QTY, sep = '_'), 
         # sku6 = paste(SKU.6, SKU.6.QTY, sep = '_'), 
         # sku7 = paste(SKU.7, SKU.7.QTY, sep = '_')
         ) %>% 
  select(ERP, PPK.SKU = ordered_sku, Order.Date, Category, Key.Item, Pack.Configuration, Style,
         ppk_ordered_qty = ordered_qty, Full.Description, prepack_color = Color, 
         sku1, sku2, sku3#, sku4, sku5, sku6, sku7
         ) %>% distinct() %>% 
  gather(sku_nbr, ordered_sku, sku1:sku3) %>% 
  filter(ordered_sku != 'NA_NA') %>% 
  separate(ordered_sku, into = c('ordered_sku', 'qty_in_prepack'), sep = '_') %>% 
  arrange(ERP) %>% 
  select(-sku_nbr) %>% 
  mutate(ppk_ordered_qty = as.numeric(ppk_ordered_qty), 
         qty_in_prepack = as.numeric(qty_in_prepack), 
         ordered_qty = qty_in_prepack * ppk_ordered_qty)

## get SKU information for the single SKU (derived from ppk_sku)
prepack_orders <- sqldf("select po.*, s.Color as unit_color
              from prepack_orders po 
              join single_skus s on po.ordered_sku = s.sku")

## combine prepack and single orders dataframes 
all_orders <- suppressWarnings(bind_rows(prepack_orders, single_orders) %>% 
                                 arrange(ERP))

## get shipping info for each ERP 
order_info <- crew_orders %>% 
  select(ERP
         ,Ship.City
         ,Ship.Region
         ,Ship.Country
         ,Ship.Zip) %>% 
  distinct()

## add shipping info to all_orders 
all_orders <- merge(all_orders, order_info, by = 'ERP', all.x = TRUE)

## output final file for Tableau 
fwrite(all_orders, "C:/Users/joshua.mark/OneDrive - Accenture/Desktop/CREW/Tableau Data/CREW_quantities.csv")
