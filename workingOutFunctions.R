
data_input <- read_csv("U:/Projects/R package - Grant Idea/GenePattern/pat_count_zips_G1.csv")
data_input <- read_csv("U:/Projects/R package - Grant Idea/GenePattern/TestDataWState.csv")

draw_plot_1 <- function(data_input){
  # HERE I SAY ZIP, BUT IT SHOULD BE THE INPUT WE DESIGNATE AS ZIP
  data_input <- data_input |>
    mutate(ZIP = as.character(ZIP))
  # FILTER FOR THE YEAR TO USE
  cleantable19 <- cleantable |>
    filter(Year == 2019)
  # JOIN THE NEIGHBORHOOD VARS TO MY UPLOADED DATA, AND CALCULATE THE POVERTY CATEGORIES
  joined_data <- left_join(data_input, cleantable19, by = c("ZIP" = "Zipcode")) |>
    mutate(p_level_4 = cut(Poverty, breaks = c(0,10,20,30,100),
                           labels = c("Low (<10%)", "Medium (10-20%)", "High (20-30%)", "Very High (>30%)")))
  # COUNT OF PATIENTS PER POV LEVEL - PULL FROM OTHER CODE TO MAKE THIS FANCIER- THIS IS A PLACEHOLDER FOR NOW
  pats <- joined_data |>
    group_by(p_level_4)|>
    summarize(count = sum(Pat_count, na.rm = TRUE)) |>
    mutate(total = sum(count, na.rm = TRUE),
           perc = round(count/total*100, 1),
           unit = "pats")

state_list <- unique(joined_data$State)

  zips <- cleantable19[which(cleantable19$State%in%state_list),]|>
    mutate(p_level_4 = cut(Poverty, breaks = c(0,10,20,30,100),
                           labels = c("Low (<10%)", "Medium (10-20%)", "High (20-30%)", "Very High (>30%)")))|>
    group_by(p_level_4)|>
    summarize(count = n()) |>
    mutate(total = sum(count, na.rm = TRUE),
           perc = round(count/total*100,0),
           unit = "zips")

both <- rbind(pats, zips) |>
  mutate(labels = paste0(round(perc, 0), "%"))

# SUBSET
pats <- both[which(both$unit == "pats"),] |>
  mutate(fraction = count / total,
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         category = sapply(strsplit(as.character(p_level_4), split = "\\("), "[[", 1),
         label = paste0(category, "\n", labels))

pats <- pats[1:4,]

# Make the plot
colors <- c("#DEE7EF", "#A09FCE", "#8C62AA", "#533445")
donut_chart <- ggplot(pats, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= p_level_4)) +
  geom_rect(color = "white", size = 0.25) +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=3.5, color = c("black", "black", "black", "white")) +
  scale_fill_manual(values = colors)+
  # scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

donut_chart

}
