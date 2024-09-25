
# Set working directory
setwd("/Users/vtitl/Documents/GitHub/ced/donation_data/")

# Load necessary libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)


final_data = readRDS("data/finalDataset.rds")


####
final_data=final_data %>% group_by(donor_name, donor_lastname, donor_birthyear) %>% mutate(id_xx = cur_group_id())
final_data=final_data %>% group_by(donation_party, donor_name, donor_lastname, donor_birthyear) %>% mutate(id_xx_donorParty = cur_group_id())

### unique donors
print(max(final_data$id_xx))
### unique party donors
print(max(final_data$id_xx_donorParty))



# Collapse donation_all by polparty, surname, firstname, and birthdate
final_data_collapsed <- final_data %>%
  group_by(donation_party, donor_lastname, donor_name, donor_birthdate) %>%
  summarise(donation_all = sum(donation_all))
