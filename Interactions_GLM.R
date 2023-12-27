############ Analysis Monkey Interaction Data #####
### M. Entezami
rm(list=objects())  #Erase workspace
library(rstudioapi)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory

#### Data Prep ####
dt <- read.csv("Megadatafile.csv", stringsAsFactors = F)
dt <- dt[, -14]
# Find initiator
initiator <- substr(dt$iti,3,3)
dt$ini <- initiator

dt <- dt %>%
  mutate(zone = case_when(
    grepl("H - Outside Museum|H- Outside Museum|H-Outside Museum|Zone H|H- Abandoned House|H- Outside Mudeum|H- Abandoned house", LocationText) ~ "High traffic area",
    grepl("Zone I|I-Telekom|I- Behind Telekom|I-Rock area|I - Rock area|I- Near Makam|I- Rock Area|I- Telekom|^I$|I-Rock Area|I- Rock area", LocationText) ~ "High traffic area",
    grepl("Zone D|D- Playground|D - Playground|D - Trem waiting area|D - Pathwalk|D - Cark park|D - School front gate|^D$|D -  Playground", LocationText) ~ "High traffic area",
    grepl("^C$|C- Forest near BM", LocationText) ~ "Medium traffic area",
    grepl("E- Chalet|Zone E|E- Near Entrance|E- Playground|E - 3 Intersection|^E$|E- 3 Intersection", LocationText) ~ "Medium traffic area",
    grepl("B-Near Dining Hall|^B$", LocationText) ~ "Low traffic area",
    grepl("F- Near House|F- School|F- Telaga Tujuh|^F$|Zone F \\(near school\\)", LocationText) ~ "Medium traffic area",
    grepl("^G$", LocationText) ~ "High traffic area",
    grepl("^J$", LocationText) ~ "High traffic area",
    grepl("A-III|^A$|A-1", LocationText) ~ "Low traffic area",
    TRUE ~ "Other"  # Fallback category for unexpected or uncategorized entries
  ))

# Get provisioning 
dt$Prov <- sub(",.*", "", dt$Provtm)
dt$Prov <- ifelse(dt$Prov == "-", "No", "Yes")

## remove NAs
initiator[which(initiator=="")] <- NA
initiator[which(initiator==" ")] <- NA

initiator_type <- substr(dt$iti,1,1)
dt$init <- initiator_type

mapping <- data.frame(
  HAprim = c('PC', 'PP', 'F', 'NW', 'C', 'R', 'Co', 'DA', 'UK', 'HW', 
             'WNC', 'T', 'S', 'HA', 'NR', 'CP', 'PBC', 'PBF', 'AF', 'SO', 
             'AC', 'PT', 'NIR', 'AE', 'M', 'Mod', 'E', 'RA'),
  HAprimcat = c(rep('Provisioned', 3), 
                rep('Aggressive', 9), 
                rep('Non aggressive', 7),
                rep('Tourist', 4), 
                rep('Fearful', 5))
)

dt <- merge(dt, mapping, by = "HAprim", all.x = TRUE)

mapping <- data.frame(
  HAsec = c('PC', 'PP', 'F', 'NW', 'C', 'R', 'Co', 'DA', 'UK', 'HW', 
            'WNC', 'T', 'S', 'HA', 'NR', 'CP', 'PBC', 'PBF', 'AF', 'SO', 
            'AC', 'PT', 'NIR', 'AE', 'M', 'Mod', 'E', 'RA'),
  HAseccat = c(rep('Provisioned', 3), 
               rep('Aggressive', 9), 
               rep('Non aggressive', 7),
               rep('Tourist', 4), 
               rep('Fearful', 5))
)

dt <- merge(dt, mapping, by = "HAsec", all.x = TRUE)

mapping <- data.frame(
  MAprim = c('CT', 'TR', 'MAG', 'P', 'F', 'DP', 'G', 'MI', 'FN', 'T', 
             'NR', 'S', 'MAL', 'NIR', 'MA', 'MPBF', 'MPBC', 'MV', 'RU', 
             'FUN', 'FUS', 'TC', 'UF', 'B', 'ST'),
  MAprimcat = c(rep('Agonistic', 5), 
                rep('Destructive', 1), 
                rep('Natural', 4),
                rep('Nonaggressive', 7), 
                rep('Fearful', 2), 
                rep('Provisioned', 6))
)


dt <- merge(dt, mapping, by = "MAprim", all.x = TRUE)


mapping <- data.frame(
  MAsec = c('CT', 'TR', 'MAG', 'P', 'F', 'DP', 'G', 'MI', 'FN', 'T', 
            'NR', 'S', 'MAL', 'NIR', 'MA', 'MPBF', 'MPBC', 'MV', 'RU', 
            'FUN', 'FUS', 'TC', 'UF', 'B', 'ST'),
  MAseccat = c(rep('Agonistic', 5), 
               rep('Destructive', 1), 
               rep('Natural', 4),
               rep('Nonaggressive', 7), 
               rep('Fearful', 2), 
               rep('Provisioned', 6))
)


dt <- merge(dt, mapping, by = "MAsec", all.x = TRUE)


hdt <- dt[which(dt$ini=="H"),]
mdt <- dt[which(dt$ini=="M"),]
#### Generalised Linear Model ####

MP_data <- data.frame(MP = rep(0,201), prov = 0, zone = 0)
MP_data$MP <- ifelse(mdt$MAprimcat == "Agonistic" & !is.na(mdt$MAprimcat), 1, 0)
MP_data$prov <- factor(mdt$Prov)
desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
MP_data$zone <- factor(mdt$zone, levels = desired_order)

MP_model <- glm(MP ~ prov + zone, data = MP_data, family = "binomial")
summary(MP_model)
#plot(MP_model)


HP_data <- data.frame(MP = rep(0,1795), HP = 0, prov = 0, zone = 0)
HP_data$MP <- ifelse(hdt$MAprimcat == "Agonistic" & !is.na(hdt$MAprimcat), 1, 0)
desired_order <- c("Non aggressive", "Aggressive", "Provisioned", "Fearful", "Tourist")
HP_data$HP <- factor(hdt$HAprimcat, levels = desired_order)
HP_data$prov <- factor(hdt$Prov)
desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
HP_data$zone <- factor(hdt$zone, levels = desired_order)

MP_model <- glm(MP ~ HP + prov + zone, data = HP_data, family = "binomial")
summary(MP_model)
#plot(MP_model)


MP_data <- data.frame(MP = rep(0,201), prov = 0, zone = 0)
MP_data$MP <- ifelse(mdt$MAprimcat == "Fearful" & !is.na(mdt$MAprimcat), 1, 0)
MP_data$prov <- factor(mdt$Prov)
desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
MP_data$zone <- factor(mdt$zone, levels = desired_order)

MP_model <- glm(MP ~ prov + zone, data = MP_data, family = "binomial")
summary(MP_model)
#plot(MP_model)


HP_data <- data.frame(MP = rep(0,1795), HP = 0, prov = 0, zone = 0)
HP_data$MP <- ifelse(hdt$MAprimcat == "Fearful" & !is.na(hdt$MAprimcat), 1, 0)
desired_order <- c("Non aggressive", "Aggressive", "Provisioned", "Fearful", "Tourist")
HP_data$HP <- factor(hdt$HAprimcat, levels = desired_order)
HP_data$prov <- factor(hdt$Prov)
desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
HP_data$zone <- factor(hdt$zone, levels = desired_order)

MP_model <- glm(MP ~ HP + prov + zone, data = HP_data, family = "binomial")
summary(MP_model)
#plot(MP_model)
