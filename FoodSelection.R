# Food Selection Processing

####### Enter Delivery Phone Numbers Here ########

dels <- c('857-999-6768', '774-381-4632')

##################################################

library(tidyverse)
library(Hmisc)
library(googlesheets4)

qnames <- read_csv('data_clean/pickup_match.csv')
guest.data.raw <- read_csv('data_clean/visit_history_current.csv')
guest.data.clean <- guest.data.raw %>%
  select(id = "Guest ID", first = "First Name", middle = "Middle Name", last = "Last Name", hhsize = "Total Individuals", visit = "Visit on") %>%
  separate(visit, into=c("visit.date", "visit.time"), sep = " ") %>%
  mutate(visit.date = as.Date(visit.date, "%Y-%m-%d"),
         yearmon = substring(as.character(visit.date), first = 1, last = 7),
         name.sb = ifelse(!is.na(middle), paste(first, middle, last, sep=" "), paste(first, last, sep = " "))) %>%
  group_by(id) %>%
  mutate(last.visit = max(visit.date),
         last.visit = ifelse(visit.date==last.visit, 1, 0)) %>%
  ungroup() 
# sometimes peoples names and household sizes change - extract most recent name and household size

guest.size <- guest.data.clean %>%
  filter(last.visit==1) %>%
  distinct(id, hhsize, name.sb)

#merge into final guest data

guest.final <- guest.data.clean %>%
  select(id, yearmon) %>%
  merge(guest.size, by = "id") %>%
  group_by(id, yearmon, hhsize, name.sb) %>%
  summarise(visits = n()) %>%
  ungroup() %>%
  mutate(visit_no = ifelse(yearmon=='2021-04', visits, 0)) %>%
  group_by(id, hhsize, name.sb) %>%
  summarise(visits = sum(visit_no))

rm(guest.size)


day.trans <- read_csv('data_clean/day_trans.csv', locale = readr::locale(encoding = "latin1"))

langs <- c('Resp_Eng', 'Resp_KA', 'Resp_Port', 'Resp_Esp')

for (l in langs){
  
  resp <- read_sheet('https://docs.google.com/spreadsheets/d/1m9bbVTLoEgN2Ne8Ns1jgW-f-R7NOw_RANsRDxqX9HQ8/edit#gid=553182362',
                     sheet = l)
    
  names(resp) <- qnames$q_short
  
  resp.fin <- resp %>%
    mutate(time = as.Date(substring(as.character(time), first = 1, last = 10)),
           allergies = as.character(allergies)
           ) %>%
    filter(time>Sys.Date()-7|del=="Yes"|del=="Si"|del=="Wi"|del=="Sí") %>%
    merge(day.trans, by.x = "pickup", by.y = "pickup", all.x = T) %>%
    filter(str_detect(pickup_eng, pattern = paste(weekdays(Sys.Date()+1)))|phone %in% dels)
  
  if (nrow(resp.fin)==0) next
  
  if(exists('orders')) {
    orders <- bind_rows(orders, resp.fin)
  }
  
  else {
    orders <- resp.fin
  }
  
}

rm(l, langs, resp, resp.fin)

#merge in soxbox names

orders.fin <- orders %>%
  unite('name', 3:4, remove = T, sep = " ") 

name.sb <- data.frame(guest.final$name.sb) %>%
  unique()

name.of <- data.frame(orders.fin$name)

name.of$name.sb <- "" # Creating an empty column
for(i in 1:dim(name.of)[1]) {
  x <- agrep(name.of$orders.fin.name[i], name.sb$guest.final.name.sb,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.08, useBytes = TRUE)
  x <- paste0(x,"")
  name.of$name.sb[i] <- x
} 

rm(i, x, name.sb)

orders.fin <- orders.fin %>%
  merge(name.of, by.x = "name", by.y = "orders.fin.name", all.x = T) %>%
  merge(guest.final, by = "name.sb", all.x = T) %>%  
  mutate(pickup_eng = ifelse(is.na(pickup_eng), "Delivery", pickup_eng),
         visit_no = ifelse(visits==0, "First",
                           ifelse(visits==1, "Second", "ThirdPlus")))

rm(name.of, day.trans, orders)


# Create google sheet for Save as Doc processing

out_frs <- orders.fin %>%
  select(Name = name, Size = hhsize, Visit = visit_no, Pickup = pickup_eng, Restrictions = allergies,
         FreshVeg = veg_frs, FreshFruit = fruit_frs, 
         Milk = milk, Eggs = eggs, Cheese = cheese, Yogurt = yogurt,
         FrozenProduce = produce_frz, FrozenProtein = protein_frz) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)


out_dry <- orders.fin %>%
  select(Name = name, Size = hhsize, Visit = visit_no, Pickup = pickup_eng, Restrictions = allergies,
         Cereal = cereal, CanMeat = protein_cnd, CanFruit = fruit_cnd, CanVeg = veg_cnd,
         Juice = juice, Soup = soup, Tomato = tom, DryFruit = dry_fruit,
         Pasta = pasta, Rice = rice, EasyPrep = mac,
         PeanutButter = pb, Beans = beans, Milk = milk, Condiment = con,
         Oil = oil, Coffee = coffee) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

gs4_create(paste("Orders_", Sys.Date(), sep = ''), sheets = list(dry = out_dry, fresh = out_frs))









###########################################################################################################




