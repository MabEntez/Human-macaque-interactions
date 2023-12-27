### M. Entezami
rm(list=objects())  #Erase workspace
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(rstudioapi)
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
                        "tw", "o", "p", "s ", "t", "g")

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
#### Data Vis ####
library(ggplot2)
library(ggpattern)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(boot)

# Bootstrapping for confidence intervals
bootstrap_frequency <- function(data, indices) {
  # Resample the data
  resampled_data <- data[indices]
  
  # Compute frequencies of each unique item
  table(resampled_data)
}
behaviours <- beh_data_long$Behaviour

# Unique items
unique_items <- unique(behaviours)

# Initialize a list to store results
results <- list()

# Bootstrap for each unique item
for(item in unique_items) {
  # Create a function to extract the frequency of the item
  item_frequency <- function(data, indices) {
    freq_table <- bootstrap_frequency(data, indices)
    return(freq_table[item])
  }
  
  # Apply the bootstrap
  boot_result <- boot(behaviours, item_frequency, R = 1000)
  
  # Calculate the mean and quantiles
  mean_freq <- mean(boot_result$t)
  quantiles <- quantile(boot_result$t, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Store results
  results[[item]] <- c(mean = mean_freq, lower = quantiles[1], upper = quantiles[2])
}

activity_data <- do.call(rbind, results)
rownames(activity_data) <- names(results)
activity_data <- activity_data[-c(8),]
activity_data[,1] <- round(activity_data[,1])
activity_data <- as.data.frame(activity_data)
activity_data[,4] <- rownames(activity_data)
activity_data[,5] <- activity_data[,1]
activity_data[,1:3] <- activity_data[,1:3] / 8086

colnames(activity_data) <- c("mean", "lower", "upper", "Behaviour", "count")
activity_data[,1:3] <- activity_data[,1:3] * 100

# Activity budget graph
ab_vis <- ggplot(activity_data, aes(x = Behaviour, y = mean)) +
  geom_bar(stat = "identity", fill = "darkgrey", colour = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(aes(label = sprintf("n = %.0f", count)), vjust = -1.6, position = position_dodge(width = 0.9)) +
  scale_x_discrete(labels = c("Aggression", "Autogrooming",  "Feeding\nNatural","Feeding\nUnnatural",
                              "Inactive", "Interaction", "Sexual", "Travel")) +
  labs(title = "Activity budget of Long-Tailed Macaques",
       y = "Percentage of total observed behaviours",
       x = "Behaviour") +
  theme_bw() +
  scale_y_continuous(labels = percent_format(scale = 1))
ab_vis

# Activity budget - time graph 
time_category_data <- beh_data_long %>%
  count(TimeCategory, Behaviour) %>%
  group_by(TimeCategory) %>%
  mutate(ind_percentage = n / sum(n) * 100)

total_count <- sum(time_category_data$n)
time_category_data <- time_category_data %>%
  mutate(percentage = n / total_count * 100)
time_category_data <- time_category_data %>%
  filter(!is.na(Behaviour))

time_category_summary <- time_category_data %>%
  group_by(Behaviour) %>%
  summarise(
    Mean_Percentage = mean(ind_percentage),
    Min_Percentage = min(ind_percentage),
    Max_Percentage = max(ind_percentage)
  )

unique_behaviours <- unique(time_category_data$Behaviour)
palette_name <- "Greys" # or any other sequential palette like "Greens", "Purples", etc.
color_values <- brewer.pal(length(unique_behaviours), palette_name)

desired_order <- c("Early morning", "Late morning", "Afternoon", "Early evening", "Late evening")
time_category_data$TimeCategory <- factor(time_category_data$TimeCategory, levels = desired_order)

my_colors <- setNames(color_values, unique_behaviours)
new_labels <- c(
  "Early morning" = "Early\nmorning",
  "Late morning" = "Late\nmorning",
  "Afternoon" = "Afternoon",
  "Early evening" = "Early\nevening",
  "Late evening" = "Late\nevening"
)



tb_vis <- ggplot(time_category_data, aes(x = TimeCategory, y = percentage, fill = Behaviour)) +
  geom_bar_pattern(stat = "identity", position = "stack", colour = "black",
                   pattern = ifelse(time_category_data$Behaviour %in% unique_behaviours[c(TRUE, FALSE)], "stripe", "none")) +
  scale_fill_manual(values = my_colors) +  # Apply custom colors
    labs(title = "(A) Activity budget of Long-Tailed Macaques by\ntime of day", y = "Percentage oftotal observed behaviour", x = "Time of day") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = new_labels)

tb_vis <- tb_vis +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none", "stripe", "none",  "stripe", "none",  "stripe", "none"), pattern_colour = "grey60" )))

tb_vis

time_category_total <- time_category_data %>%
  group_by(TimeCategory) %>%
  summarise(total_n = sum(n))

tb_ivis <- ggplot() +
  geom_bar_pattern(data = time_category_data, aes(x = TimeCategory, y = ind_percentage, fill = Behaviour), stat = "identity", position = "stack", colour = "black",
                   pattern = ifelse(time_category_data$Behaviour %in% unique_behaviours[c(TRUE, FALSE)], "stripe", "none")) +
  geom_text(data = time_category_total, aes(x = TimeCategory, y = 100, label = paste("n =", total_n)), vjust = -0.5, colour = "black") +  # Add total count on top of each bar
  scale_fill_manual(values = my_colors) +
  labs(title = " (A) Activity budget proportions of Long-Tailed Macaques by\ntime of day", y = "Proportion of observed behaviour", x = "Human presence") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = c(0, 25, 50, 75, 100))+
  coord_cartesian(ylim = c(0, 110))
tb_ivis

#Print categorical percentage for bars
time_category_data <- time_category_data %>%
  arrange(TimeCategory, desc(Behaviour)) %>%
  group_by(TimeCategory) %>%
  mutate(cumulative_perc = cumsum(percentage),
         midpoint = cumulative_perc - 0.5 * percentage)

ggplot(time_category_data, aes(x = TimeCategory, y = percentage, fill = Behaviour)) +
  geom_bar(stat = "identity", position = position_stack(vjust = 2), colour = "black") + 
  scale_fill_manual(values = my_colors) +
  labs(title = "Activity budget proportions of Long-Tailed Macaques by\ntime of day", y = "Percentage of total observed behaviour") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text_repel(aes(label = sprintf("%.1f%%", ind_percentage), y = midpoint),
                  size = 3, 
                  color = "black",
                  direction = "y",          # Allow movement only in y-axis
                  box.padding = 0.2,
                  point.padding = 0.2)

# Activity budget - time line graph 
beh_data_long$Timeexcel <- as.POSIXct(beh_data_long$Timeexcel, format = "%H:%M")  # Adjust format as needed
library(lubridate)
beh_data_long$RoundedTime <- floor_date(beh_data_long$Timeexcel, "15 minutes")

time_data <- beh_data_long %>%
  group_by(RoundedTime, Behaviour) %>%
  tally() %>%
  group_by(RoundedTime) %>%
  mutate(ind_percentage = n / sum(n) * 100)

total_count <- sum(time_data$n)
time_data <- time_data %>%
  mutate(percentage = n / total_count * 100)
time_data <- time_data %>%
  filter(!is.na(Behaviour))

unique_behaviours <- unique(time_data$Behaviour)
palette_line_name <- "Dark2" # or any other sequential palette like "Greens", "Purples", etc.
line_color_values <- brewer.pal(length(unique_behaviours), palette_line_name)
my_colors_line <- setNames(line_color_values, unique_behaviours)

ltb_vis <- ggplot(time_data, aes(x = RoundedTime, y = percentage, group = Behaviour, color = Behaviour)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = my_colors_line) +  # Apply custom colors
  labs(title = "Activity budget of Long-Tailed Macaques by\ntime of day", y = "Percentage of total observed behaviour", x = "Time of day") +
  theme_bw() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_datetime()

ltb_vis

ltb_ivis <- ggplot(time_data, aes(x = RoundedTime, y = ind_percentage, group = Behaviour, color = Behaviour)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = my_colors_line) +  # Apply custom colors
  labs(title = "Activity budget proportions of Long-Tailed Macaques by\ntime of day", y = "Proportion of observed behaviour", x = "Time of day") +
  theme_bw() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_datetime()

ltb_ivis

# Activity budget - area graph
zone_data <- beh_data_long %>%
  count(Zone, Behaviour)%>%
  group_by(Zone) %>%
  mutate(ind_percentage = n / sum(n) * 100)

zone_summary <- zone_data %>%
  group_by(Behaviour) %>%
  summarise(
    Mean_Percentage = mean(ind_percentage),
    Min_Percentage = min(ind_percentage),
    Max_Percentage = max(ind_percentage)
  )

total_count <- sum(zone_data$n)
zone_data <- zone_data %>%
  mutate(percentage = n / total_count * 100)
zone_data <- zone_data %>%
  filter(!is.na(Behaviour))

desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
zone_data$Zone <- factor(zone_data$Zone, levels = desired_order)
new_labels <- c(
  "Low traffic area" = "Low\ntraffic area",
  "Medium traffic area" = "Medium\ntraffic area",
  "High traffic area" = "High\ntraffic area"
)

zb_vis <- ggplot(zone_data, aes(x = Zone, y = percentage, fill = Behaviour)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.6) +
  scale_fill_manual(values = my_colors) +
  labs(title = "(B) Activity budget of Long-Tailed Macaques in\nlow, medium, and high human traffic areas", y = "Percentage of total observed behaviour", x = "Area") +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_x_discrete(labels = new_labels)

zb_vis

zone_data_total <- zone_data %>%
  group_by(Zone) %>%
  summarise(total_n = sum(n))

zb_ivis <- ggplot() +
  geom_bar_pattern(data = zone_data, aes(x = Zone, y = ind_percentage, fill = Behaviour), stat = "identity", position = "stack", colour = "black", width = 0.6,
           pattern = ifelse(zone_data$Behaviour %in% unique_behaviours[c(TRUE, FALSE)], "stripe", "none")) +
  geom_text(data = zone_data_total, aes(x = Zone, y = 100, label = paste("n =", total_n)), vjust = -0.5, colour = "black") +  # Add total count on top of each bar
  scale_fill_manual(values = my_colors) +
  labs(title = "(B) Activity budget proportions of Long-Tailed Macaques in\nlow, medium, and high traffic tourism areas", y = "Proportion of observed behaviour", x = "Human presence") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = c(0, 25, 50, 75, 100))+
  coord_cartesian(ylim = c(0, 110))
zb_ivis

# Activity budget - Weather graph
beh_data_long$Weather <- ifelse(grepl("cloudy", beh_data_long$Weather, ignore.case = TRUE), 
                                "Cloudy", 
                                beh_data_long$Weather)

beh_data_long$Weather <- ifelse(grepl("sunny", beh_data_long$Weather, ignore.case = TRUE), 
                                "Sunny", 
                                beh_data_long$Weather)

beh_data_long$Weather <- ifelse(!beh_data_long$Weather %in% c("Cloudy", "Sunny"), 
                                NA, 
                                beh_data_long$Weather)

weather_data <- beh_data_long %>%
  count(Weather, Behaviour)%>%
  group_by(Weather) %>%
  mutate(ind_percentage = n / sum(n) * 100)

weather_summary <- weather_data %>%
  group_by(Behaviour) %>%
  summarise(
    Mean_Percentage = mean(ind_percentage),
    Min_Percentage = min(ind_percentage),
    Max_Percentage = max(ind_percentage)
  )

total_count <- sum(weather_data$n)
weather_data <- weather_data %>%
  mutate(percentage = n / total_count * 100)
weather_data <- weather_data %>%
  filter(!is.na(Behaviour))
weather_data <- weather_data %>%
  filter(!is.na(Weather))

desired_order <- c("Sunny", "Cloudy", "Other/Non specified")
weather_data$Weather <- factor(weather_data$Weather, levels = desired_order)

wb_vis <- ggplot(weather_data, aes(x = Weather, y = percentage, fill = Behaviour)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.45) +
  scale_fill_manual(values = my_colors) +
  labs(title = "(C) Activity budget of Long-Tailed Macaques in\nsunny and cloudy weather", y = "Percentage of total observed behaviour") +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))
wb_vis

weather_data_total <- weather_data %>%
  group_by(Weather) %>%
  summarise(total_n = sum(n))

wb_ivis <- ggplot() +
  geom_bar_pattern(data = weather_data, aes(x = Weather, y = ind_percentage, fill = Behaviour), stat = "identity", position = "stack", colour = "black", width = 0.45,
           pattern = ifelse(weather_data$Behaviour %in% unique_behaviours[c(TRUE, FALSE)], "stripe", "none")) +
  geom_text(data = weather_data_total, aes(x = Weather, y = 100, label = paste("n =", total_n)), vjust = -0.5, colour = "black") +  # Add total count on top of each bar
  scale_fill_manual(values = my_colors) +
  labs(title = "(C) Activity budget proportions of Long-Tailed Macaques in\nsunny and cloudy weather", y = "Proportion of observed behaviour", x = "Human presence") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = c(0, 25, 50, 75, 100))+
  coord_cartesian(ylim = c(0, 110))
wb_ivis
# Activity budget - Human presense graph
Human_beh_data <- beh_data_long %>%
  count(Behaviour, HumanPresenceText)%>%
  group_by(HumanPresenceText) %>%
  mutate(ind_percentage = n / sum(n) * 100)

Human_beh_summary <- Human_beh_data %>%
  group_by(Behaviour) %>%
  summarise(
    Mean_Percentage = mean(ind_percentage),
    Min_Percentage = min(ind_percentage),
    Max_Percentage = max(ind_percentage)
  )

total_count <- sum(Human_beh_data$n)
Human_beh_data <- Human_beh_data %>%
  mutate(percentage = n / total_count * 100)

hb_vis <- ggplot(Human_beh_data, aes(x = HumanPresenceText, y = percentage, fill = Behaviour)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.9) +
  scale_fill_manual(values = my_colors) +
  labs(title = "(D) Activity budget of Long-Tailed Macaques in\npresence of humans", y = "Percentage of total observed behaviour", x = "Human presence") +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))
hb_vis

Human_beh_total <- Human_beh_data %>%
  group_by(HumanPresenceText) %>%
  summarise(total_n = sum(n))

hb_ivis <- ggplot() +
  geom_bar_pattern(data = Human_beh_data, aes(x = HumanPresenceText, y = ind_percentage, fill = Behaviour), stat = "identity", position = "stack", colour = "black", width = 0.6,
           pattern = ifelse(Human_beh_data$Behaviour %in% unique_behaviours[c(TRUE, FALSE)], "stripe", "none")) +
  geom_text(data = Human_beh_total, aes(x = HumanPresenceText, y = 100, label = paste("n =", total_n)), vjust = -0.5, colour = "black") +  # Add total count on top of each bar
  scale_fill_manual(values = my_colors) +
  labs(title = "(D) Activity budget proportions of Long-Tailed Macaques in\npresence of humans", y = "Proportion of observed behaviour", x = "Human presence") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = c(0, 25, 50, 75, 100))+
  coord_cartesian(ylim = c(0, 110))

hb_ivis <- hb_ivis +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none", "stripe", "none",  "stripe", "none",  "stripe", "none"), pattern_colour = "grey60" )))

hb_ivis

legend_plot_beh <- cowplot::get_legend(hb_ivis)
# Substrate budget
Human_sub_data <- sub_data_long %>%
  count(Substrate, HumanPresenceText) %>%
  group_by(HumanPresenceText) %>%
  mutate(percentage = n / sum(n) * 100)
new_labels <- c(
  "Anthropogenic structure" = "Low\ntraffic area",
  "Natural structure" = "Medium\ntraffic area",
  "High traffic area" = "High\ntraffic area"
)

hs_vis <- ggplot(Human_sub_data, aes(x = HumanPresenceText, y = percentage, fill = Substrate)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.9) + 
  geom_text(aes(label=sprintf("n = %.0f", n)), vjust=-0.5, position = position_dodge(width = 0.9)) +  
  scale_fill_manual(values = c("Anthropogenic structure" = "grey",
                               "Natural structure" = "grey52"),
                    labels = c("Anthropogenic", "Natural")) +
  labs(title = "(B) Monkey interactions with structures in\npresence of humans", y = "Percentage of interactions with structures", x = "Human presence") +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))
hs_vis

Zone_sub_data <- sub_data_long %>%
  count(Substrate, Zone) %>%
  group_by(Zone) %>%
  mutate(percentage = n / sum(n) * 100)

desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
Zone_sub_data$Zone <- factor(Zone_sub_data$Zone, levels = desired_order)
new_labels <- c(
  "Low traffic area" = "Low\ntraffic area",
  "Medium traffic area" = "Medium\ntraffic area",
  "High traffic area" = "High\ntraffic area"
)

zs_vis <- ggplot(Zone_sub_data, aes(x = Zone, y = percentage, fill = Substrate)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.9) + 
  geom_text(aes(label=sprintf("n = %.0f", n)), vjust=-0.5, position = position_dodge(width = 0.9)) +  
  scale_fill_manual(values = c("Anthropogenic structure" = "grey",
                               "Natural structure" = "grey52"),
                    labels = c("Anthropogenic", "Natural")) +
  labs(title = "(A) Monkey interactions with structures in\nlow, medium, and high human traffic areas", 
       y = "Percentage of interactions with structures", 
       x = "Zone") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = new_labels)
zs_vis

legend_plot_sub <- cowplot::get_legend(zs_vis)

# Combined plot
library(cowplot)
library(gridExtra)

combined_plot_beh <- plot_grid(
  tb_ivis + theme(legend.position = "none"),
  zb_ivis + theme(legend.position = "none"),
  wb_ivis + theme(legend.position = "none"),
  hb_ivis,
  ncol = 2, 
  nrow = 2,
  align = "h"
)
combined_plot_beh

combined_plot_sub <- plot_grid(
  zs_vis + theme(legend.position = "none"),
  hs_vis,
  ncol = 2, 
  align = "h"
)
combined_plot_sub
