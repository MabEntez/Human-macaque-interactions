### M. Entezami
rm(list=objects())  #Erase workspace
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(rstudioapi)
library(MASS)
setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory
# Read the data
data <- read.csv("Scan sample Megadata file 6.csv")

#### Behaviour Data prep ####
# Extracting zone information
data$Zone <- NA
colnames(data)[1] <- "Dateexcel"
data$Dateexcel[data$Dateexcel == ""] <- NA
data$Zone <- ifelse(str_detect(data$Behaviour, "Zone"), data$Behaviour, NA)
data$Zone <- zoo::na.locf(data$Zone, na.rm=FALSE)
data$Dateexcel <- ifelse(!is.na(data$Zone) & !is.na(data$Dateexcel), data$Dateexcel, NA)

# Extracting ZoneType
data$ZoneType <- str_extract(data$Zone, "\\(([^)]+)\\)")

# Removing the text inside the brackets from Zone
data$Zone <- str_replace(data$Zone, "\\s*\\([^)]+\\)", "")
data$Zone <- str_extract(data$Zone, "Zone [A-Z]")
# Use tidyr's fill() to propagate the Dateexcel values downwards
data <- data %>%
  fill(Dateexcel, .direction = "down")

# Standardize Timeexcel values
data$Timeexcel <- str_replace_all(data$Timeexcel, fixed(" "), "")
data$Timeexcel <- str_replace_all(data$Timeexcel, fixed("am"), " AM")
data$Timeexcel <- str_replace_all(data$Timeexcel, fixed("pm"), " PM")

data$Timeexcel <- ifelse(data$Timeexcel >= "12:00" & data$Timeexcel < "01:00" & 
                           str_detect(data$Timeexcel, " AM"), 
                         str_replace(data$Timeexcel, " AM", " PM"), 
                         data$Timeexcel)

data$Timeexcel <- format(strptime(data$Timeexcel, format="%I.%M %p"), format="%H:%M")

# Recategorize Timeexcel values
data <- data %>%
  mutate(TimeCategory = case_when(
    Timeexcel >= "09:00" & Timeexcel <= "10:30" ~ "Early morning",
    Timeexcel > "10:30" & Timeexcel <= "12:00" ~ "Late morning",
    Timeexcel > "12:00" & Timeexcel <= "13:30" ~ "Afternoon",
    Timeexcel > "13:30" & Timeexcel <= "15:30" ~ "Early evening",
    Timeexcel > "15:30" & Timeexcel <= "20:00" ~ "Late evening",
    TRUE ~ "Other"
  ))

# Separate each behavior into a new row
beh_data_long <- data %>% 
  filter(!str_detect(Behaviour, "BM-Zone")) %>% 
  separate_rows(Behaviour, sep=",") 

# Remove unnecessary rows and columns
beh_data_long <- beh_data_long[!is.na(beh_data_long$Timeexcel),]
beh_data_long$Behaviour <- beh_data_long$Behaviour[!is.na(beh_data_long$Behaviour)]

# Print the organized data
beh_data_long <- beh_data_long[,-c(7,8)]
beh_data_long <- beh_data_long %>%
  filter(Timeexcel != "")
beh_data_long <- beh_data_long %>%
  filter(Behaviour != "")
beh_data_long <- beh_data_long %>%
  filter(grepl("\\(LTM\\)", ZoneType))

replacement_values <- c("I", "IR", "IA", "IB", "Tg", "T", "FO", "FON", "F", "N", 
                        "Q", "G2", "P", "OP", "GP", "LS", "J", "BG", "D", "X", 
                        "XR", "E", "S", "RP", "SM", "M", "SF", "G", "P1")

descriptions <- c("Inactive", "Inactive", "Inactive", "Inactive", "Travel", "Travel", 
                  "Feeding Natural", "Feeding Unnatural", "Feeding Unnatural", "Feeding Natural", 
                  "Feeding Natural", "Interaction", "Interaction", "Interaction", "Interaction", 
                  "Interaction", "Interaction", "Interaction", "Aggression", "Aggression", 
                  "Aggressive reqruitment", "Eyebrow lift", "Sexual", "Sexual", "Sexual", 
                  "Sexual", "Sexual", "Autogrooming", "Solitary play")

beh_data_long <- beh_data_long %>%
  mutate(Behaviour = descriptions[match(Behaviour, replacement_values)])

beh_data_long <- beh_data_long %>%
  mutate(Zone = case_when(
    Zone %in% c("Zone A", "Zone B") ~ "Low traffic area",
    Zone %in% c("Zone C", "Zone E", "Zone F", "Zone K") ~ "Medium traffic area",
    Zone %in% c("Zone D", "Zone G", "Zone H", "Zone I", "Zone J") ~ "High traffic area",
    TRUE ~ Zone
  ))

beh_data_long <- beh_data_long %>%
  mutate(HumanPresenceText = case_when(
    HumanPresenceText %in% c("-", "") ~ "No",
    TRUE ~ HumanPresenceText
  ))

time <- data %>%
  count(TimeCategory)

zone <- data %>%
  count(Zone)
#### Substrate Data prep ####

# Separate each behavior into a new row
sub_data_long <- data %>% 
  filter(!str_detect(Substrate, "BM-Zone")) %>% 
  separate_rows(Substrate, sep=",") 

# Remove unnecessary rows and columns
sub_data_long <- sub_data_long[!is.na(sub_data_long$Timeexcel),]
sub_data_long$Substrate <- sub_data_long$Substrate[!is.na(sub_data_long$Substrate)]

# Print the organized data
sub_data_long <- sub_data_long[,-c(7,8)]
sub_data_long <- sub_data_long %>%
  filter(Timeexcel != "")
sub_data_long <- sub_data_long %>%
  filter(Substrate != "")
sub_data_long <- sub_data_long %>%
  filter(grepl("\\(LTM\\)", ZoneType))
sub_data_long <- sub_data_long %>%
  filter(Substrate != "missed")
sub_data_long <- sub_data_long %>%
  filter(Substrate != "-")

replacement_values <- c("e", "pt", "pc", "r", "rd", "s", "v", "b", "d", "f", 
                        "tw", "o", "p", "s", "t", "g")

descriptions <- c(rep('Anthropogenic structure', 14), rep('Natural structure', 2))

sub_data_long <- sub_data_long %>%
  mutate(Zone = case_when(
    Zone %in% c("Zone A", "Zone B") ~ "Low traffic area",
    Zone %in% c("Zone C", "Zone E", "Zone F", "Zone K") ~ "Medium traffic area",
    Zone %in% c("Zone D", "Zone G", "Zone H", "Zone I", "Zone J") ~ "High traffic area",
    TRUE ~ Zone
  ))

sub_data_long <- sub_data_long %>%
  mutate(Substrate = descriptions[match(Substrate, replacement_values)])

sub_data_long <- sub_data_long %>%
  mutate(HumanPresenceText = case_when(
    HumanPresenceText %in% c("-", "") ~ "No",
    TRUE ~ HumanPresenceText
  ))
zone_time <- data %>% count(Zone, ZoneType)

beh_data_long$Weather <- ifelse(grepl("cloudy", beh_data_long$Weather, ignore.case = TRUE), 
                                "Cloudy", 
                                beh_data_long$Weather)

beh_data_long$Weather <- ifelse(grepl("sunny", beh_data_long$Weather, ignore.case = TRUE), 
                                "Sunny", 
                                beh_data_long$Weather)

beh_data_long$Weather <- ifelse(!beh_data_long$Weather %in% c("Cloudy", "Sunny"), 
                                NA, 
                                beh_data_long$Weather)

#### GLM ####

library(broom)

GLM_data <- data.frame(behaviour = rep(0,8086), structure = 0, weather = 0, human = 0, zone = 0)
GLM_data$behaviour <- ifelse(beh_data_long$Behaviour == "Feeding Unnatural" & !is.na(beh_data_long$Behaviour), 1, 0)
pattern <- "e|pt|pc|r|rd|s|v|b|d|f|tw|o|p|s"
GLM_data$structure <- ifelse(grepl(pattern, beh_data_long$Substrate) & !is.na(beh_data_long$Behaviour), 1, 0)
GLM_data$weather <- as.factor(beh_data_long$Weather)
GLM_data$human <- ifelse(beh_data_long$HumanPresenceText == "Yes" & !is.na(beh_data_long$Behaviour), 1, 0)
GLM_data$zone <- factor(beh_data_long$Zone, levels = c("Low traffic area", "Medium traffic area", "High traffic area"))

FU_model <- glm(behaviour ~ structure + weather + human + zone, data = GLM_data, family = "binomial")
summary(FU_model)
FU_model_coef <- tidy(FU_model)
null_model <- glm(behaviour ~ 1, data = GLM_data, family = "binomial")
mcfadden_r_squared <- 1 - (logLik(FU_model)/logLik(null_model))
FU_model_coef$Mcfadden <- mcfadden_r_squared
#plot(FU_model)

GLM_data <- data.frame(behaviour = rep(0,8086), structure = 0, weather = 0, human = 0, zone = 0)
GLM_data$behaviour <- ifelse(beh_data_long$Behaviour == "Inactive" & !is.na(beh_data_long$Behaviour), 1, 0)
pattern <- "e|pt|pc|r|rd|s|v|b|d|f|tw|o|p|s"
GLM_data$structure <- ifelse(grepl(pattern, beh_data_long$Substrate)& !is.na(beh_data_long$Substrate), 1, 0)
GLM_data$weather <- as.factor(beh_data_long$Weather)
GLM_data$human <- as.factor(beh_data_long$HumanPresenceText)
GLM_data$zone <- factor(beh_data_long$Zone, levels = c("Low traffic area", "Medium traffic area", "High traffic area"))

I_model <- glm(behaviour ~ structure + weather + human + zone, data = GLM_data, family = "binomial")
summary(I_model)
I_model_coef <- tidy(I_model)
null_model <- glm(behaviour ~ 1, data = GLM_data, family = "binomial")
mcfadden_r_squared <- 1 - (logLik(I_model)/logLik(null_model))
I_model_coef$Mcfadden <- mcfadden_r_squared
#plot(I_model)

GLM_data <- data.frame(behaviour = rep(0,8086), structure = 0, weather = 0, human = 0, zone = 0)
GLM_data$behaviour <- ifelse(beh_data_long$Behaviour == "Interaction", 1, 0)
pattern <- "e|pt|pc|r|rd|s|v|b|d|f|tw|o|p|s "
GLM_data$structure <- ifelse(grepl(pattern, beh_data_long$Substrate)& !is.na(beh_data_long$Substrate), 1, 0)
GLM_data$weather <- as.factor(beh_data_long$Weather)
GLM_data$human <- as.factor(beh_data_long$HumanPresenceText)
GLM_data$zone <- factor(beh_data_long$Zone, levels = c("Low traffic area", "Medium traffic area", "High traffic area"))

IN_model <- glm(behaviour ~ structure + weather + human + zone, data = GLM_data, family = "binomial")
summary(IN_model)
IN_model_coef <- tidy(IN_model)
null_model <- glm(behaviour ~ 1, data = GLM_data, family = "binomial")
mcfadden_r_squared <- 1 - (logLik(IN_model)/logLik(null_model))
IN_model_coef$Mcfadden <- mcfadden_r_squared
#plot(In_model)

GLM_data$behaviour <- ifelse(beh_data_long$Behaviour == "Feeding Natural", 1, 0)
pattern <- "e|pt|pc|r|rd|s|v|b|d|f|tw|o|p|s "
GLM_data$structure <- ifelse(grepl(pattern, beh_data_long$Substrate)& !is.na(beh_data_long$Substrate), 1, 0)
GLM_data$weather <- as.factor(beh_data_long$Weather)
GLM_data$human <- as.factor(beh_data_long$HumanPresenceText)
GLM_data$zone <- factor(beh_data_long$Zone, levels = c("Low traffic area", "Medium traffic area", "High traffic area"))

FN_model <- glm(behaviour ~ structure + weather + human + zone, data = GLM_data, family = "binomial")
summary(FN_model)
FN_model_coef <- tidy(FN_model)
null_model <- glm(behaviour ~ 1, data = GLM_data, family = "binomial")
mcfadden_r_squared <- 1 - (logLik(FN_model)/logLik(null_model))
FN_model_coef$Mcfadden <- mcfadden_r_squared
#plot(FN_model)

GLM_data$behaviour <- ifelse(beh_data_long$Behaviour == "Travel", 1, 0)
pattern <- "e|pt|pc|r|rd|s|v|b|d|f|tw|o|p|s "
GLM_data$structure <- ifelse(grepl(pattern, beh_data_long$Substrate)& !is.na(beh_data_long$Substrate), 1, 0)
GLM_data$weather <- as.factor(beh_data_long$Weather)
GLM_data$human <- as.factor(beh_data_long$HumanPresenceText)
GLM_data$zone <- factor(beh_data_long$Zone, levels = c("Low traffic area", "Medium traffic area", "High traffic area"))

T_model <- glm(behaviour ~ structure + weather + human + zone, data = GLM_data, family = "binomial")
summary(T_model)
T_model_coef <- tidy(T_model)
null_model <- glm(behaviour ~ 1, data = GLM_data, family = "binomial")
mcfadden_r_squared <- 1 - (logLik(T_model)/logLik(null_model))
T_model_coef$Mcfadden <- mcfadden_r_squared
#plot(FN_model)

GLM_data$behaviour <- ifelse(beh_data_long$Behaviour == "Autogrooming", 1, 0)
pattern <- "e|pt|pc|r|rd|s|v|b|d|f|tw|o|p|s "
GLM_data$structure <- ifelse(grepl(pattern, beh_data_long$Substrate)& !is.na(beh_data_long$Substrate), 1, 0)
GLM_data$weather <- as.factor(beh_data_long$Weather)
GLM_data$human <- as.factor(beh_data_long$HumanPresenceText)
GLM_data$zone <- factor(beh_data_long$Zone, levels = c("Low traffic area", "Medium traffic area", "High traffic area"))

A_model <- glm(behaviour ~ structure + weather + human + zone, data = GLM_data, family = "binomial")
summary(A_model)
A_model_coef <- tidy(A_model)
null_model <- glm(behaviour ~ 1, data = GLM_data, family = "binomial")
mcfadden_r_squared <- 1 - (logLik(A_model)/logLik(null_model))
A_model_coef$Mcfadden <- mcfadden_r_squared

T_model_coef$Model <- "Travel"
I_model_coef$Model <- "Inactive"
IN_model_coef$Model <- "Interaction"
FN_model_coef$Model <- "Feeding Natural"
FU_model_coef$Model <- "Feeding Unnatural"
A_model_coef$Model <- "Autogrooming"
GLM_table <- rbind(T_model_coef, I_model_coef, IN_model_coef, FN_model_coef, FU_model_coef, A_model_coef)
write.csv(GLM_table, "GLM output.csv")