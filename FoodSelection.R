# Food Selection Processing

####### Enter Delivery Phone Numbers Here ########

dels <- c('617-230-0464', '617-599-2915')

##################################################

library(tidyverse)
library(Hmisc)
library(googlesheets4)

qnames <- read_csv('data_clean/pickup_match_eng.csv')
guest.data.raw <- read_csv('data_clean/visit_history_current.csv')
guest.data.clean <- guest.data.raw %>%
  select(id = "Guest ID", first = "First Name", middle = "Middle Name", last = "Last Name", hhsize = "Total Individuals", visit = "Visit on", phone = "Home Phone") %>%
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
  mutate(visit_no = ifelse(yearmon=='2021-05', visits, 0)) %>%
  group_by(id, hhsize, name.sb) %>%
  summarise(visits = sum(visit_no))

rm(guest.size)


day.trans <- read_csv('data_clean/day_trans.csv', locale = readr::locale(encoding = "latin1"))

langs <- c('Resp_Eng2', 'Resp_KA2', 'Resp_Esp2', 'Resp_Port2')


for (l in langs){
  
  resp <- read_sheet('https://docs.google.com/spreadsheets/d/1m9bbVTLoEgN2Ne8Ns1jgW-f-R7NOw_RANsRDxqX9HQ8/edit#gid=553182362',
                     sheet = l)
  names(resp) <- qnames$q_short_eng
  
  
  resp.fin <- resp %>%
    mutate(time = as.Date(substring(as.character(time), first = 1, last = 10)),
           allergies2 = as.character(allergies2),
           allergies1 = as.character(allergies1),
           milk1 = ifelse(is.list(milk1), unlist(milk1), milk1),
           milk2 = ifelse(is.list(milk2), unlist(milk2), milk2)
           ) %>%
    filter(time>Sys.Date()-6|del=="Yes"|del=="Si"|del=="Wi"|del=="Sí") %>%
    merge(day.trans, by.x = "pickup", by.y = "pickup", all.x = T) %>%
    filter(str_detect(pickup_eng, pattern = paste(weekdays(Sys.Date())))|phone %in% dels|first_nm %in% dels)
    
  
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
         visit_no = ifelse(visits==0|name=="Victoria Mahin", "First",
                           ifelse(visits==1, "Second", "ThirdPlus")))

rm(name.of, day.trans, orders)


# Create google sheet for Save as Doc processing


out_frs1 <- orders.fin %>%
  filter(visit_no=="First"|is.na(visit_no)) %>%
  select(Name = name, Size = hhsize, Visit = visit_no, Pickup = pickup_eng, Restrictions = allergies1,
         FreshVeg = veg_frs1, FreshFruit = fruit_frs1, 
         Milk = milk1, Eggs = eggs1, Cheese = cheese1, Yogurt = yogurt1,
         FrozenProduce = produce_frz1, FrozenProtein = protein_frz1) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_frs2 <- orders.fin %>%
  filter(visit_no=="Second") %>%
  select(Name = name, Size = hhsize, Visit = visit_no, Pickup = pickup_eng, Restrictions = allergies2,
         FreshVeg = veg_frs2, FreshFruit = fruit_frs2, 
         FrozenProduce = produce_frz2, FrozenProtein = protein_frz2) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)



out_dry1 <- orders.fin %>%
  filter(visit_no=="First"|is.na(visit_no)) %>%
  select(Name = name, Size = hhsize, Visit = visit_no, Phone = phone, Pickup = pickup_eng, Restrictions = allergies1,
         Cereal = cereal1, CanMeat = protein_cnd1, CanFruit = fruit_cnd1, CanVeg = veg_cnd1,
         Juice = juice1, Soup = soup1, Tomato = tom1, DryFruit = dry_fruit1,
         Pasta = pasta1, Rice = rice1, EasyPrep = mac1,
         PeanutButter = pb1, Beans = breans1, Milk = milk1, Condiment = con1,
         Oil = oil1, Coffee = coffee1, Toiletry = toil1) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_dry2 <- orders.fin %>%
  filter(visit_no=="Second") %>%
  select(Name = name, Size = hhsize, Visit = visit_no, Phone = phone, Pickup = pickup_eng, Restrictions = allergies2,
         Cereal = cereal2, CanMeat = protein_cnd2, CanFruit = fruit_cnd2, CanVeg = veg_cnd2,
         Juice = juice2, Soup = soup2, Tomato = tom2, DryFruit = dry_fruit2,
         Pasta = pasta2, Rice = rice2, EasyPrep = mac2,
         PeanutButter = pb2, Beans = beans2, Milk = milk2) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

gs4_create(paste("Orders_", Sys.Date()+1, "_Del", sep = ''), sheets = list(dry1 = out_dry1, fresh1 = out_frs1, dry2 = out_dry2, fresh2 = out_frs2))









###########################################################################################################




