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

#### initiator percentage per species ####

library(RColorBrewer)
library(cowplot)
library(gridExtra)

df <- dt[, c(14,17)]

df <- df %>%
  filter(ini != "" & ini != " " & init != "" & init != " ")

df_count <- df %>%
  group_by(ini, init) %>%
  summarize(count = n())

df_count <- df_count[-7,]

total_count <- sum(df_count$count)

df_percentage <- df_count %>%
  mutate(percentage = (count / total_count) * 100)

df_percentage$percentage <- ifelse(df_percentage$ini == 'H', -df_percentage$percentage, df_percentage$percentage)

df_percentage <- df_percentage %>%
  mutate(ini = case_when(
    ini == "H" ~ "Human",
    ini == "M" ~ "Monkey",
    TRUE ~ ini  # Default case to keep existing values for other ini
  )) %>%
  mutate(init = case_when(
    init == "F" ~ "Fear",
    init == "C" ~ "Curiosity",
    init == "A" ~ "Aggression",
    TRUE ~ init  # Default case to keep existing values for other ini
  ))


unique_behaviours <- unique(df_percentage$Initiator)
palette_name <- "Greys" # or any other sequential palette like "Greens", "Purples", etc.
color_values <- brewer.pal(length(unique_behaviours), palette_name)
my_colors <- setNames(color_values, unique_behaviours)
colnames(df_percentage) <- c("Initiator", "Behaviour", "count", "percentage")
# Plotting
ggplot(df_percentage, aes(x = Behaviour, y = percentage, fill = Initiator)) +
  geom_col(colour = "black") +
  coord_flip() +
  scale_fill_manual(values = c("grey70", "grey30")) +
  scale_y_continuous(
    labels = abs,
    breaks = seq(-100, 100, by = 10),
    limits = c(-62, 10)) +
  labs(y = "Percentage", x = "Behaviour", title = "Percentage of initiating behaviour by initator")+
  theme_bw()

#### Provisioning ####

pdf <- dt[, c(14:17)]

pdf_a <- pdf %>%
  filter(Prov != "" & Prov != " " & init != "" & init != " " & ini == "M")

pdf_a_count <- pdf_a %>%
  group_by(Prov, init) %>%
  summarize(count = n())


pdf_a_count <- pdf_a_count %>%
  mutate(init = case_when(
    init == "F" ~ "Fear",
    init == "C" ~ "Curiosity",
    init == "A" ~ "Aggression"
  ))

a <- ggplot(pdf_a_count, aes(x = Prov, y = count, fill = init)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  scale_y_continuous(breaks = seq(0, 200, by = 10))+
  labs(y = "Count",
       x = "Provisioning",
       title = "(A) Context with and without provisioning\nwith monkeys as initiators",
       fill = "Initial behaviour")+
  theme_bw()

pdf <- dt[, c(14:17)]

pdf_b <- pdf %>%
  filter(Prov != "" & Prov != " " & init != "" & init != " " & ini == "H")

pdf_b_count <- pdf_b %>%
  group_by(Prov, init) %>%
  summarize(count = n())


pdf_b_count <- pdf_b_count %>%
  mutate(init = case_when(
    init == "F" ~ "Fear",
    init == "C" ~ "Curiosity",
    init == "A" ~ "Aggression"
  ))

b <- ggplot(pdf_b_count, aes(x = Prov, y = count, fill = init)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  scale_y_continuous(breaks = seq(0, 2000, by = 50))+
  labs(y = "Count",
       x = "Provisioning",
       title = "(B) Context with and without provisioning\nwith humans as initiators",
       fill = "Initial behaviour")+
  theme_bw()


combined_plot_prov <- plot_grid(
  a + theme(legend.position = "none"),
  b,
  ncol = 2, 
  nrow = 1,
  align = "h",
  rel_widths = c(1, 1.5)
)

combined_plot_prov
#Zones

adf <- dt[, c(14:17)]

adf_a <- adf %>%
  filter(Prov != "" & Prov != " " & init != "" & init != " "  & ini == "M")

adf_a_count <- adf_a %>%
  group_by(zone, Prov, init) %>%
  summarize(count = n())

desired_order <- c("Low traffic area", "Medium traffic area", "High traffic area")
adf_a_count$zone <- factor(adf_a_count$zone, levels = desired_order)

ggplot(adf_a_count, aes(x = zone, y = count, fill = Prov)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("grey70", "grey30")) +
  labs(y = "Count",
       x = "Zone",
       title = "Aggression counts in high and medium traffic zones\nwith and without provisioning",
       fill = "Provisioning")+
  theme_bw()

#### Barplot Diagram ####

library(ggplot2)
library(RColorBrewer)
library(scales)
# Create a table of counts
hdt_count_table <- table(hdt$HAprimcat, hdt$MAprimcat)

# Convert to dataframe
hdf <- as.data.frame(as.table(hdt_count_table))
total_count <- sum(hdf$Freq)
hdf <- hdf %>%
  mutate(percentage = Freq / total_count * 100)

unique_behaviours <- unique(hdf$Var2)
palette_name <- "Greys" # or any other sequential palette like "Greens", "Purples", etc.
color_values <- brewer.pal(length(unique_behaviours), palette_name)
my_colors <- setNames(color_values, unique_behaviours)
sum_hdf <- hdf %>%
  group_by(Var1) %>%
  summarize(total_percentage = sum(percentage),
            total_freq = sum(Freq))
# Plot
ggplot(hdf, aes(Var1, percentage, fill = Var2)) +
  geom_bar(stat="identity", position="stack", colour = "black") +
  geom_text(data = sum_hdf, aes(x=Var1, y=total_percentage, label=sprintf("n = %.f", total_freq)), inherit.aes = FALSE, vjust=-0.5) +  
  labs(x = "Human primary action", y = "Percent of total interactions", fill = "Monkey secondary action") +
  scale_fill_manual(values = my_colors) + 
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Create a table of counts
mdt_count_table <- table(mdt$HAprimcat, mdt$MAprimcat)

# Convert to dataframe
mdf <- as.data.frame(as.table(mdt_count_table))
total_count <- sum(mdf$Freq)
mdf <- mdf %>%
  mutate(percentage = Freq / total_count * 100)

unique_behaviours <- unique(mdf$Var1)
palette_name <- "Greys" # or any other sequential palette like "Greens", "Purples", etc.
color_values <- brewer.pal(length(unique_behaviours), palette_name)
my_colors <- setNames(color_values, unique_behaviours)
sum_mdf <- mdf %>%
  group_by(Var2) %>%
  summarize(total_percentage = sum(percentage),
            total_freq = sum(Freq))

# Plot
ggplot(mdf, aes(x=Var2, y=percentage, fill=Var1)) +
  geom_bar(stat="identity", position="stack", colour = "black") + 
  geom_text(data = sum_mdf, aes(x=Var2, y=total_percentage, label=sprintf("n = %.f", total_freq)), inherit.aes = FALSE, vjust=-0.5) + 
  labs(x = "Monkey primary action", y = "Percent of total interactions", fill = "Human secondary action") +
  scale_fill_manual(values = my_colors) + 
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))


#### Spearmans Pair-wise ####

hdt_transformed_temp <- hdt %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = HAprimcat, values_from = HAprimcat,
              values_fill = list(HAprimcat = 0), values_fn = list(HAprimcat = ~1)) %>%
  select(-id)
original_cols <- names(hdt)
new_cols <- setdiff(names(hdt_transformed_temp), original_cols)
hdt_transformed <- hdt_transformed_temp %>%
  rename_with(~ paste0("H_", .), new_cols)

hdt_transformed_temp <- hdt_transformed %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = MAprimcat, values_from = MAprimcat,
              values_fill = list(MAprimcat = 0), values_fn = list(MAprimcat = ~1)) %>%
  select(-id)
original_cols <- names(hdt_transformed)
new_cols <- setdiff(names(hdt_transformed_temp), original_cols)
hdt_transformed <- hdt_transformed_temp %>%
  rename_with(~ paste0("M_", .), new_cols)

# Identify columns starting with "H_" and "M_"
H_columns <- grep("^H_", names(hdt_transformed), value = TRUE)
M_columns <- grep("^M_", names(hdt_transformed), value = TRUE)

# Initialize an empty dataframe for results
results <- data.frame(
  H_Column = character(),
  M_Column = character(),
  Spearman_Correlation = numeric(),
  stringsAsFactors = FALSE
)

# Compute Spearman's rank correlations
for (h_col in H_columns) {
  for (m_col in M_columns) {
    correlation_result <- cor(hdt_transformed[[h_col]], hdt_transformed[[m_col]], method = "spearman")
    results <- rbind(results, c(h_col, m_col, correlation_result))
  }
}

# Set column names for results
colnames(results) <- c("H_Column", "M_Column", "Spearman_Correlation")

# Print the results
print(results)

library(ggplot2)
library(reshape2)

# Assuming 'results' is your dataframe with correlations

# Reshape data for visualization
results_reshaped <- reshape2::melt(results, id.vars = c("H_Column", "M_Column"))

# Create heatmap
ggplot(results_reshaped, aes(x = H_Column, y = M_Column, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Spearman\nCorrelation", x = "H Columns", y = "M Columns", title = "Heatmap of Spearman's Rank Correlation")

#### Plotting the Sankey Diagram ####
library(networkD3)
library(htmlwidgets)

links_ini_HAprimcat <- data.frame(source = hdt$ini, target = hdt$HAprimcat)
links_HAprimcat_MAprimcat <- data.frame(source = hdt$HAprimcat, target = hdt$MAprimcat)

# Aggregate the links to create the value column for each transition
links_ini_HAprimcat <- links_ini_HAprimcat %>% group_by(source, target) %>% summarise(value = n()) %>% ungroup()
links_HAprimcat_MAprimcat <- links_HAprimcat_MAprimcat %>% group_by(source, target) %>% summarise(value = n()) %>% ungroup()

# Calculate percentages for each transition

# For hdt$ini to hdt$HAprimcat transition
total_ini <- nrow(hdt)
links_ini_HAprimcat$value_percentage <- (links_ini_HAprimcat$value / total_ini) * 100

# For hdt$HAprimcat to hdt$MAprimcat transition
total_HAprimcat <- nrow(data.frame(source = hdt$HAprimcat))
links_HAprimcat_MAprimcat$value_percentage <- (links_HAprimcat_MAprimcat$value / total_HAprimcat) * 100

# Combine the transitions
links <- rbind(links_ini_HAprimcat, links_HAprimcat_MAprimcat)

nodes <- data.frame(name = unique(c(as.character(links$source), as.character(links$target))))

# Updating links with index values
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Creating the sankey diagram with units as "percentage"
humansankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                             Target = "target", Value = "value_percentage", NodeID = "name",
                             units = "percentage", fontSize = 12, nodeWidth = 30)

print(humansankey)

links_ini_MAprimcat <- data.frame(source = hdt$ini, target = hdt$MAprimcat)
links_MAprimcat_HAprimcat <- data.frame(source = hdt$MAprimcat, target = hdt$HAprimcat)

# Aggregate the links to create the value column for each transition
links_ini_MAprimcat <- links_ini_MAprimcat %>% group_by(source, target) %>% summarise(value = n()) %>% ungroup()
links_MAprimcat_HAprimcat <- links_MAprimcat_HAprimcat %>% group_by(source, target) %>% summarise(value = n()) %>% ungroup()

# Calculate percentages for each transition

# For hdt$ini to hdt$HAprimcat transition
total_ini <- nrow(hdt)
links_ini_MAprimcat$value_percentage <- (links_ini_MAprimcat$value / total_ini) * 100

# For hdt$HAprimcat to hdt$MAprimcat transition
total_MAprimcat <- nrow(data.frame(source = hdt$MAprimcat))
links_MAprimcat_HAprimcat$value_percentage <- (links_MAprimcat_HAprimcat$value / total_MAprimcat) * 100

# Combine the transitions
links <- rbind(links_ini_MAprimcat, links_MAprimcat_HAprimcat)

nodes <- data.frame(name = unique(c(as.character(links$source), as.character(links$target))))

# Updating links with index values
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Creating the sankey diagram with units as "percentage"
monkeysankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                              Target = "target", Value = "value_percentage", NodeID = "name",
                              units = "percentage", fontSize = 12, nodeWidth = 30)

print(monkeysankey)


saveWidget(humansankey, "humansankey_prim_diagram.html")
saveWidget(monkeysankey, "monkeysankey_prim_diagram.html")