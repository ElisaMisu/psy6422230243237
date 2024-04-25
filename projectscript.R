# List of packages to check and install if required

packages <- c("here", "readxl", "tidyverse", "shiny", "vcd", "dplyr", "readr", "purrr", "viridis")

# Loop to check and install packages if required (quietly- without warnings)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define the path to the Excel file using the here() function
# starting with the file path

excel_file <- here("sexualorientationfurtherpersonalcharacteristicsenglandandwalescensus2021.xlsx")

# Reading the data from the sheet 2a of the Excel file

sexualorientationreligion <- read_excel(excel_file, 
                                        sheet = "2a", 
                                        col_types = c("skip", "text", "text", "text", "text", "text", "numeric"), 
                                        skip = 4)
# View the loaded data

View(sexualorientationreligion)

# Error message resulting from [c] in numeric column, explanation: Values under 10 have been suppressed and are displayed as "[c]".
# Solution: Remove rows with "[c]" in any column 

sexualorientationreligion_clean <- sexualorientationreligion[!apply(sexualorientationreligion, 1, function(x) any(grepl("\\[c\\]", x))), ]

# rename the columns

sexualorientationreligion_clean <- rename(sexualorientationreligion_clean,    
                                          c( "sexual_orientation" = "Sexual orientation", 
                                             "religion" = "Religion", 
                                             "age" = "Age group [note 2]" , 
                                             "sex" = "Sex [note 1]", 
                                             "percentage" = "Percentage estimate of group \r\n[note 3] [note 4]"))

view(sexualorientationreligion_clean)

# Filtering data using dplyr's filter

filtered_data <- sexualorientationreligion_clean %>%
  filter(sexual_orientation != "All usual residents", #exclude all usual residents line
         age == "All ages 16 years and over", #include all ages (instead of age group breakdowns)
         sex == "People")  #just include all people instead of breakdown of sex
view(filtered_data)

# stacked plot with purple-yellow gradient

ggplot(filtered_data, aes(x = sexual_orientation, y = percentage / 100, fill = religion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(title = "Percentage Breakdown of Religious Identity by Sexual Orientation",
       x = "Sexual Orientation",
       y = "Percentage") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(legend.position = "bottom")

# plot 2 with horizontal bars and pastel colors

ggplot(filtered_data, aes(x = sexual_orientation, y = percentage / 100, fill = religion)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Religious Identity by Sexual Orientation",
       x = "Sexual Orientation",
       y = "Percentage") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_light() +
  theme(legend.position = "bottom")

# Plot 3 with conditional labeling (otherwise the same as plot 2)

# Adjusting filtered_data to include relative size per group for conditional labeling
filtered_data1 <- filtered_data %>%
  group_by(sexual_orientation) %>%
  mutate(total = sum(percentage),
         relative_size = percentage / total)

ggplot(filtered_data1, aes(x = sexual_orientation, y = percentage / 100, fill = religion)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  geom_text(aes(label = ifelse(relative_size > 0.1, scales::percent(percentage / 100), "")),  # Only label significant segments
            position = position_stack(vjust = 0.5),
            color = "black",  # Using white for contrast; adjust based on your palette
            size = 3.5) +
  labs(title = "Religious Identity by Sexual Orientation",
       x = "Sexual Orientation",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_light() +
  theme(legend.position = "bottom")

# plot 4 (custom) 70's colour scheme (don't like order of colours and labels are too messy-- don't keep)

ggplot(filtered_data, aes(x = sexual_orientation, y = percentage / 100, fill = religion)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage Breakdown of Religious Identity by Sexual Orientation",
       x = "Sexual Orientation",
       y = "Percentage") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
                               "#e377c2", "#7f7f7f", "#bcbd22")) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

# plot 5 greyscale (don't keep)

ggplot(filtered_data, aes(x = sexual_orientation, y = percentage / 100, fill = religion)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Religious Identity by Sexual Orientation",
       x = "Orientation",
       y = "%") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  theme_classic() +
  theme(legend.position = "top")



# further adjustments to plot 3
# Assuming filtered_data is already loaded and contains the necessary columns
# Create a modified data frame with additional metrics for labeling

  