# List of packages to check and install if required

packages <- c("here", "readxl", "tidyverse", "shiny", "vcd", "viridis", "scales", "plotly")

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

# Reading the data from the sheet 2a of the Excel file, skipping the first 4 rows
# and assigning appropriate column types
sexualorientationreligion <- read_excel(excel_file, 
                                        sheet = "2a", 
                                        col_types = c("skip", "text", "text", "text", "text", "text", "numeric"), 
                                        skip = 4)
# View the loaded data

View(sexualorientationreligion)

# Error message resulting from [c] in numeric column, explanation: Values under 10 have been suppressed and are displayed as "[c]".
# Solution: Remove rows with "[c]" in any column/ keep all that do not contain [c] 

sexualorientationreligion_clean <- sexualorientationreligion[!apply(sexualorientationreligion, 1, function(x) any(grepl("\\[c\\]", x))), ]

# rename the columns, leave "Religion" w/ a capital R ready for the final plot labels

sexualorientationreligion_clean <- rename(sexualorientationreligion_clean,    
                                          c( "sexual_orientation" = "Sexual orientation", 
                                             "age" = "Age group [note 2]" , 
                                             "sex" = "Sex [note 1]", 
                                             "percentage" = "Percentage estimate of group \r\n[note 3] [note 4]"))

view(sexualorientationreligion_clean)

# Filtering data using dplyr's filter 

filtered_data <- sexualorientationreligion_clean %>%
  filter(sexual_orientation != "All usual residents", #exclude all usual residents line
         age == "All ages 16 years and over", #include all ages (instead of age group breakdowns)
         sex == "People")  #just include all "people" instead of breakdown of sex

view(filtered_data)

#create plot with horizontal bars and pastel colors

p <- ggplot(filtered_data, aes(x = sexual_orientation, y = percentage / 100, fill = Religion, text = paste("Percentage:", scales::percent(percentage / 100)))) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Religious Identity by Sexual Orientation",
       x = "Sexual Orientation",
       y = "Percentage") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_light() +
  theme(legend.position = "bottom")

# Convert to interactive plotly object

interactive_plot <- ggplotly(p, tooltip = "text")  # specify tooltip to display percentage values

interactive_plot


