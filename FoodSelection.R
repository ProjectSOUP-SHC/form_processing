# Food Selection Processing

####### Enter Delivery Phone Numbers Here ##################

dels <- c()

####### Enter Current Month Here (format "YYYY-MM") ########

current_yearmon <- "2021-07"

############################################################

library(tidyverse)
library(Hmisc)
library(googlesheets4)

qnames <- read_csv('data_clean/pickup_match_eng.csv')
visit.data.raw <- read_csv('data_clean/visit_history_current.csv', 
                           col_types = cols(
                             `Visit ID` = col_double(),
                             `Visit on` = col_datetime(format = ""),
                             `Visit Location` = col_character(),
                             `Guest Number` = col_double(),
                             `Tracking Method` = col_character(),
                             `Tracking Result` = col_double(),
                             `Visit Status` = col_character(),
                             Volunteer = col_character(),
                             `Volunteer Overide Allow` = col_character(),
                             `Volunteer Overide Ident` = col_logical(),
                             `Volunteer Overide Cancel` = col_logical(),
                             `Volunteer Overide Conditions` = col_logical(),
                             `Outreach ID` = col_double(),
                             `Outreach Name` = col_character(),
                             `Outreach on` = col_datetime(format = ""),
                             `Guest ID` = col_double(),
                             `First Name` = col_character(),
                             `Middle Name` = col_character(),
                             `Last Name` = col_character(),
                             `Date of Birth` = col_logical(),
                             Address = col_character(),
                             `Apartment/Suite` = col_character(),
                             `City/Town` = col_character(),
                             `State/Province` = col_character(),
                             `Zip/Postal Code` = col_character(),
                             Homeless = col_character(),
                             `Total Individuals` = col_double(),
                             `0 to 17` = col_double(),
                             `18 to 64` = col_double(),
                             `65+` = col_double(),
                             `Blocked from Receiving Help` = col_character(),
                             Email = col_logical(),
                             `Home Phone` = col_character(),
                             `Cell Phone` = col_double(),
                             `Marital Status` = col_logical(),
                             Spouse = col_logical(),
                             `Nationality/Race` = col_character(),
                             `Created At` = col_datetime(format = ""),
                             `Updated At` = col_datetime(format = ""),
                             Ethnicity = col_character(),
                             `Start Date` = col_date(format = ""),
                             Gender = col_character(),
                             `Head of Household` = col_character(),
                             `Household Status` = col_character(),
                             `Housing Status` = col_character(),
                             `Household Income` = col_character(),
                             `# of Disabled` = col_double(),
                             `Employment Status` = col_character(),
                             `Cash Income` = col_character(),
                             `Benefit Income` = col_character(),
                             `Number of Bags` = col_double()
                           ))
visit.data.clean <- visit.data.raw %>%
  select(id = "Guest ID", first = "First Name", last = "Last Name", 
         hhsize = "Total Individuals", visit = "Visit on") %>%
  separate(visit, into=c("visit.date", "visit.time"), sep = " ") %>%
  mutate(visit.date = as.Date(visit.date, "%Y-%m-%d"),
         yearmon = substring(as.character(visit.date), first = 1, last = 7),
         name.sb = paste(first, last, sep=" ")
         ) %>%
  group_by(id) %>%
  mutate(last.visit = max(visit.date),
         last.visit = ifelse(visit.date==last.visit, 1, 0)) %>%
  ungroup() 
# sometimes peoples names and household sizes change - extract most recent name and household size

guest.size <- visit.data.clean %>%
  filter(last.visit==1) %>%
  distinct(id, hhsize, name.sb)

guest.phone <- read_csv("data_clean/guest_data.csv",
                        col_types = cols(
                          .default = col_character(),
                          `Guest ID` = col_double(),
                          `Date of Birth` = col_logical(),
                          `Total Individuals` = col_double(),
                          `0 to 17` = col_double(),
                          `18 to 64` = col_double(),
                          `65+` = col_double(),
                          Email = col_logical(),
                          `Home Phone` = col_double(),
                          `Cell Phone` = col_double(),
                          `Marital Status` = col_logical(),
                          Spouse = col_logical(),
                          `# of Disabled` = col_double()
                        )) %>%
  select(id = "Guest ID", phone = "Home Phone") %>%
  mutate(
    phone = paste(substring(phone, first = 1, last = 3),
                  substring(phone, first = 4, last = 6),
                  substring(phone, first = 7, last = 10),
                  sep = '-')
  )

#merge into final guest data

guest.final <- visit.data.clean %>%
  select(id, yearmon) %>%
  merge(guest.size, by = "id") %>% 
  merge(guest.phone, by = "id") %>%
  group_by(id, yearmon, hhsize, name.sb, phone) %>%
  summarise(visits = n()) %>%
  ungroup() %>%
  mutate(visit_no = ifelse(yearmon==current_yearmon, visits, 0)) %>%
  group_by(id, hhsize, name.sb, phone) %>%
  summarise(visits = sum(visit_no))

rm(guest.size, guest.phone, visit.data.clean)


day.trans <- read_csv('data_clean/day_trans.csv', locale = readr::locale(encoding = "latin1"))

langs <- c('Resp_Eng2', 'Resp_KA2', 'Resp_Esp2', 'Resp_Port2')

for (l in langs){
  
  resp <- read_sheet('https://docs.google.com/spreadsheets/d/1m9bbVTLoEgN2Ne8Ns1jgW-f-R7NOw_RANsRDxqX9HQ8/edit#gid=553182362',
                     sheet = l,
                     col_types = "Tccccccccccccccccccccccccccccccccccccccccccccccccccccc")
  names(resp) <- qnames$q_short_eng
  
  
  resp.fin <- resp %>%
    mutate(time = as.Date(substring(as.character(time), first = 1, last = 10))
           ) %>%
    filter(time>Sys.Date()-6|del=="Yes"|del=="Sim"|del=="Wi"|del=="Sí") %>%
    merge(day.trans, by.x = "pickup", by.y = "pickup", all.x = T) %>%
    filter(str_detect(pickup_eng, pattern = paste(weekdays(Sys.Date())))|phone %in% dels)
    
  
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
  unite('name', 3:4, remove = T, sep = " ") %>%
  merge(guest.final, by = "phone", all.x =T) %>%
  mutate(pickup_eng = ifelse(is.na(pickup_eng), "Delivery", pickup_eng),
         visit_no_sb = ifelse(visits==0, "First",
                              ifelse(visits==1, "Second", "ThirdPlus")))

rm(day.trans, orders)


# Create google sheet for Save as Doc processing


out_frs1_1 <- orders.fin %>% #First visit who list first or don't know
  filter((visit_no_sb=="First"|is.na(visit_no_sb))&(visit_no=="Yes"|visit_no=="Don't know"|is.na(visit_no))) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Pickup = pickup_eng, Restrictions = allergies1,
         FreshVeg = veg_frs1, FreshFruit = fruit_frs1, 
         Milk = milk1, Eggs = eggs1, Cheese = cheese1, Yogurt = yogurt1,
         FrozenProduce = produce_frz1, FrozenProtein = protein_frz1) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_frs1_2 <- orders.fin %>% #First visit who list second
  filter((visit_no_sb=="First")&(visit_no=="No")) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Pickup = pickup_eng, Restrictions = allergies1,
         FreshVeg = veg_frs2, FreshFruit = fruit_frs2, 
         Milk = milk2, 
         FrozenProduce = produce_frz2, FrozenProtein = protein_frz2) %>%
  mutate(Eggs = "Ask client", 
         Cheese = "Ask client", 
         Yogurt = "Ask client") %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_frs1 <- bind_cols(out_frs1_1, select(out_frs1_2, -item))

out_frs2_2 <- orders.fin %>% #Second visit who list second
  filter((visit_no_sb=="Second"|is.na(visit_no_sb)|visit_no_sb=="ThirdPlus")&(visit_no=="No")) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Pickup = pickup_eng, Restrictions = allergies2,
         FreshVeg = veg_frs2, FreshFruit = fruit_frs2, Milk = milk2,
         FrozenProduce = produce_frz2, FrozenProtein = protein_frz2) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_frs2_1 <- orders.fin %>% #Second visit who list first or don't know
  filter((visit_no_sb=="Second"|visit_no_sb=="ThirdPlus")&(visit_no=="Don't know"|visit_no=="Yes")) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Pickup = pickup_eng, Restrictions = allergies1,
         FreshVeg = veg_frs1, FreshFruit = fruit_frs1, Milk = milk1,
         FrozenProduce = produce_frz1, FrozenProtein = protein_frz1) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_frs2 <- bind_cols(out_frs2_2, select(out_frs2_1, -item))

out_dry1_1 <- orders.fin %>%
  filter((visit_no_sb=="First"|is.na(visit_no_sb))&(visit_no=="Yes"|visit_no=="Don't know"|is.na(visit_no))) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Phone = phone, Pickup = pickup_eng, Restrictions = allergies1,
         Cereal = cereal1, CanMeat = protein_cnd1, CanFruit = fruit_cnd1, CanVeg = veg_cnd1,
         Juice = juice1, Soup = soup1, Tomato = tom1, DryFruit = dry_fruit1,
         Pasta = pasta1, Rice = rice1, EasyPrep = mac1,
         PeanutButter = pb1, Beans = breans1, Milk = milk1, Condiment = con1,
         Oil = oil1, Coffee = coffee1, Toiletry = toil1) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_dry1_2 <- orders.fin %>%
  filter((visit_no_sb=="First")&(visit_no=="No")) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Phone = phone, Pickup = pickup_eng, Restrictions = allergies1,
         Cereal = cereal2, CanMeat = protein_cnd2, CanFruit = fruit_cnd2, CanVeg = veg_cnd2,
         Juice = juice2, Soup = soup2, Tomato = tom2, DryFruit = dry_fruit2,
         Pasta = pasta2, Rice = rice2, EasyPrep = mac2,
         PeanutButter = pb2, Beans = beans2, Milk = milk2) %>%
  mutate(Condiment = "Ask client",
         Oil = "Ask client", 
         Coffee = "Ask client", 
         Toiletry = "Ask client") %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_dry1 <- bind_cols(out_dry1_1, select(out_dry1_2, -item))

out_dry2_2 <- orders.fin %>%
  filter((visit_no_sb=="Second"|is.na(visit_no_sb)|visit_no_sb=="ThirdPlus")&(visit_no=="No")) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Phone = phone, Pickup = pickup_eng, Restrictions = allergies2,
         Cereal = cereal2, CanMeat = protein_cnd2, CanFruit = fruit_cnd2, CanVeg = veg_cnd2,
         Juice = juice2, Soup = soup2, Tomato = tom2, DryFruit = dry_fruit2,
         Pasta = pasta2, Rice = rice2, EasyPrep = mac2,
         PeanutButter = pb2, Beans = beans2, Milk = milk2) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_dry2_1 <- orders.fin %>%
  filter((visit_no_sb=="Second"|visit_no_sb=="ThirdPlus")&(visit_no=="Don't know"|visit_no=="Yes")) %>%
  select(Name = name, Size = hhsize, Visit = visit_no_sb, Phone = phone, Pickup = pickup_eng, Restrictions = allergies1,
         Cereal = cereal1, CanMeat = protein_cnd1, CanFruit = fruit_cnd1, CanVeg = veg_cnd1,
         Juice = juice1, Soup = soup1, Tomato = tom1, DryFruit = dry_fruit1,
         Pasta = pasta1, Rice = rice1, EasyPrep = mac1,
         PeanutButter = pb1, Beans = breans1, Milk = milk1) %>%
  t() %>%
  as.data.frame() %>%
  bind_cols(item = row.names(.)) %>%
  relocate(item)

out_dry2 <- bind_cols(out_dry2_2, select(out_dry2_1, -item))

gs4_create(paste("Orders_", Sys.Date(), sep = ''), sheets = list(dry1 = out_dry1, fresh1 = out_frs1, dry2 = out_dry2, fresh2 = out_frs2))









###########################################################################################################




