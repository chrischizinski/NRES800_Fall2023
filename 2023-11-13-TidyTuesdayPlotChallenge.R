
# 📚 load libraries ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(colorspace)
library(patchwork)

# 💾 load data ---------------------------------------------------------------

tt_data <- tt_load(2023, week = 37)

global_human_day <- tt_data$global_human_day |> 
  janitor::clean_names()

all_countries <- tt_data$all_countries |> 
  janitor::clean_names()


# colors ------------------------------------------------------------------

pal <- c('#8cb369', '#f4e285', '#f4a259', '#5b8e7d', "#bc4b51")
pal <- colorRampPalette(pal)(8)
vizoR::show_pal(pal)

col_cat_label <- "dark grey"

lighten_amount <- seq(0, 1, length = 7)[2:6]

# 🤼 wrangle -----------------------------------------------------------------
box_width <- 0.25
tolerance <- 0.5

# create a data.frame of the categories and the subcategories
all_countries |> 
  distinct(category, subcategory) -> categories

categories |> 
  right_join(global_human_day, join_by('subcategory')) |> 
  select(-uncertainty) |> 
  arrange(category, desc(hours_per_day)) |> 
  mutate(category = ifelse(category == "Food provision", "Food\nprovision", str_wrap(category, 14)),
         category = factor(category)) -> cat_subcat

# work on the first bar - categories
cat_subcat |> 
  group_by(category) |> 
  summarise(hours_per_day = sum(hours_per_day)) |> 
  mutate(category_num = as.numeric(category),
         cuml_hrs = cumsum(hours_per_day),
         x_label_point = ifelse(row_number() == 1, hours_per_day/2, lag(cuml_hrs) + hours_per_day/2),
         xmin = ifelse(row_number() == 1, 0, lag(cuml_hrs)),
         xmax = cuml_hrs,
         ymin = 0,
         ymax = ymin + box_width,
         segment_y = ifelse(row_number() %% 2 == 0,ymax + 0.05, ymax + 0.15),
         segment_y = case_match(category,
                                "Nonfood\nprovision" ~ 0.61,
                                .default = segment_y),
         col = pal[category_num],
         hrs = as.integer(hours_per_day),
         mins = str_pad(round((hours_per_day - hrs)*60), 2, side = "left", pad = "0"),
         time_label = ifelse(hrs > 0, paste0(as.character(hrs),"h"," ", mins, "m"),paste0(mins, "m"))) -> category_color

cat_subcat |> 
  as.data.frame()

# work on the second bar - subcategories
cat_subcat |> 
  mutate(category_num = as.numeric(category),
         subcategory = str_wrap(subcategory, 14),
         cuml_hrs = cumsum(hours_per_day),
         x_label_point = ifelse(row_number() == 1, hours_per_day/2, lag(cuml_hrs) + hours_per_day/2),
         xmin = ifelse(row_number() == 1, 0, lag(cuml_hrs)),
         xmax = cuml_hrs,
         ymax = ( -0.01 ), 
         ymin = ymax - box_width,
         segment_y = ifelse(row_number() %% 2 == 0,ymin - 0.05, ymin - 0.25),
         segment_y = case_match(subcategory,
                                "Energy" ~ -1.01,
                                "Food growth &\ncollection"~ -0.61,
                                "Human\ntransportation" ~ -0.61,
                                "Material\ntransportation" ~ -0.81,
                                "Food\nprocessing" ~ -0.41,
                                "Inhabited\nenvironment" ~ -0.81,
                                "Artifacts" ~ -0.61,
                                "Buildings" ~ -0.81,
                                "Infrastructure" ~ -1.01,
                                "Allocation"~ -1.21,
                                "Hygiene &\ngrooming"~ -1.21,
                                .default = segment_y),
         col = pal[category_num],
         hrs = as.integer(hours_per_day),
         mins = str_pad(round((hours_per_day - hrs)*60), 2, side = "left", pad = "0"),
         time_label = ifelse(hrs > 0, paste0(as.character(hrs),"h"," ", mins, "m"),paste0(mins, "m"))) |> 
  group_by(category) |> 
  mutate(sub_category = 1:n(),
         sub_col = lighten(col, amount = lighten_amount[sub_category])) |>
  ungroup() -> sub_category_color

## Third bar differences

all_countries |> 
  filter(country_iso3 == "USA") |> 
  select(category, subcategory, hours_per_day_combined) |> 
  arrange(category, desc(hours_per_day_combined)) |> 
  mutate(category = ifelse(category == "Food provision", "Food\nprovision", str_wrap(category, 14)),
         category = factor(category)) |> 
  group_by(category) |> 
  summarise(hours_per_day_USA = sum(hours_per_day_combined)) |> 
  arrange(category, desc(hours_per_day_USA)) |> 
  mutate(category = ifelse(category == "Food provision", "Food\nprovision", str_wrap(category, 14)),
         category = factor(category)) |> 
  left_join((cat_subcat |> 
              group_by(category) |> 
              summarise(hours_per_day_GBL = sum(hours_per_day))), join_by('category')) |> 
  ungroup() |> 
  mutate(diff_hrs_from_global = hours_per_day_GBL - hours_per_day_USA,
         category_num = as.numeric(category),
         cuml_hrs = cumsum(hours_per_day_USA),
         x_label_point = ifelse(row_number() == 1, hours_per_day_USA/2, lag(cuml_hrs) + hours_per_day_USA/2),
         xmin = ifelse(row_number() == 1, 0, lag(cuml_hrs)),
         xmax = cuml_hrs,
         ymin = 0,
         ymax = ymin + box_width,
         segment_y = ifelse(row_number() %% 2 == 0,ymax + 0.05, ymax + 0.15),
         segment_y = case_match(category,
                                "Nonfood\nprovision" ~ 0.61,
                                .default = segment_y),
         col = pal[category_num],
         hrs = floor(hours_per_day_USA),
         mins = str_pad(round((hours_per_day_USA - hrs)*60), 2, side = "left", pad = "0"),
         diff_hrs = as.integer(diff_hrs_from_global),
         diff_mins = str_pad(round((diff_hrs_from_global - diff_hrs)*60), 2, side = "left", pad = "0"),
         diff_time_label = ifelse(diff_hrs_from_global == 0, paste("0m"),paste0(diff_hrs,"h"," ", diff_mins, "m")),
         diff_time_label = str_remove(diff_time_label, "0h "),
         hrs = as.integer(hours_per_day_USA),
         mins = str_pad(round((hours_per_day_USA - hrs)*60), 2, side = "left", pad = "0"),
         time_label = ifelse(hrs > 0, paste0(as.character(hrs),"h"," ", mins, "m"),paste0(mins, "m"))) -> category_color_USA

## Fourth bar differences

all_countries |> 
  filter(country_iso3 == "USA") |> 
  select(category, subcategory, hours_per_day_combined) |> 
  arrange(category, desc(hours_per_day_combined)) |> 
  mutate(category = ifelse(category == "Food provision", "Food\nprovision", str_wrap(category, 14)),
         category = factor(category)) |> 
  left_join((cat_subcat |> 
               group_by(category, subcategory) |> 
               summarise(hours_per_day_GBL = sum(hours_per_day))), join_by('category', 'subcategory')) |> 
  mutate(diff_hrs_from_global = hours_per_day_GBL - hours_per_day_combined,
         category_num = as.numeric(category),
         subcategory = str_wrap(subcategory, 14),
         cuml_hrs = cumsum(hours_per_day_combined),
         x_label_point = ifelse(row_number() == 1, hours_per_day_combined/2, lag(cuml_hrs) + hours_per_day_combined/2),
         xmin = ifelse(row_number() == 1, 0, lag(cuml_hrs)),
         xmax = cuml_hrs,
         ymax = ( -0.01 ), 
         ymin = ymax - box_width,
         segment_y = ifelse(row_number() %% 2 == 0,ymin - 0.05, ymin - 0.25),
         segment_y = case_match(subcategory,
                                "Energy" ~ -1.01,
                                "Food growth &\ncollection"~ -0.61,
                                "Human\ntransportation" ~ -0.61,
                                "Material\ntransportation" ~ -0.81,
                                "Food\nprocessing" ~ -0.41,
                                "Inhabited\nenvironment" ~ -0.81,
                                "Artifacts" ~ -0.61,
                                "Buildings" ~ -0.81,
                                "Infrastructure" ~ -1.01,
                                "Allocation"~ -1.21,
                                "Hygiene &\ngrooming"~ -1.21,
                                .default = segment_y),
         col = pal[category_num],
         hrs = as.integer(hours_per_day_combined),
         mins = str_pad(round((hours_per_day_combined - hrs)*60), 2, side = "left", pad = "0"),
         time_label = ifelse(hrs > 0, paste0(as.character(hrs),"h"," ", mins, "m"),paste0(mins, "m")),
         diff_hrs = as.integer(diff_hrs_from_global),
         diff_mins = str_pad(round((diff_hrs_from_global - diff_hrs)*60), 2, side = "left", pad = "0"),
         diff_time_label = ifelse(diff_hrs_from_global == 0, paste("0m"),paste0(diff_hrs,"h"," ", diff_mins, "m")),
         diff_time_label = str_remove(diff_time_label, "0h ")) |> 
  group_by(category) |> 
  mutate(sub_category = 1:n(),
         sub_col = lighten(col, amount = lighten_amount[sub_category])) |>
  ungroup() -> sub_category_color_USA

sub_category_color_USA |> 
  select(hours_per_day_combined, hrs, mins,diff_hrs_from_global, diff_hrs ,diff_mins,diff_time_label)

  
# 📊 plot --------------------------------------------------------------------
x_adj <- 0.09
point_size <- 1.5


global_plot <- 
ggplot() + 
  # shading rect
  annotate("rect", xmin = -0.5, xmax = 26.5, ymin = -1.55, ymax = 1, fill = "grey95") +
  annotate("text", x = -0.45, y = -1.45, label = "GLOBAL", vjust = 0, hjust = 0, fontface = "bold", size = 12) +
  
  geom_segment(data = category_color, aes(x  = x_label_point, xend = x_label_point,
                                          y = ymax, yend = segment_y, color = col)) +
  
  geom_segment(data = sub_category_color, aes(x  = x_label_point, xend = x_label_point,
                                          y = ymax, yend = segment_y, color = sub_col)) +
  geom_point(data = category_color, aes(x = x_label_point, y = ymax, color = col), size = point_size) +
  geom_point(data = sub_category_color, aes(x = x_label_point, y = ymin, color = sub_col), size = point_size) +
  
  geom_point(data = category_color, aes(x = x_label_point, y = segment_y, color = col), size = point_size) +
  geom_point(data = sub_category_color, aes(x = x_label_point, y = segment_y, color = sub_col), size = point_size) +
  
  geom_rect(data = category_color, aes(xmin  = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col)) +
  geom_rect(data = sub_category_color, aes(xmin  = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sub_col)) +
  
  # Category and subcategory labels
  geom_text(data = category_color, 
            aes(x  = x_label_point,
                y = segment_y,
                label = category), 
            size = 2.5, 
            nudge_y = 0.12,
            lineheight = 0.75,
            vjust = 0.5,
            hjust = 0.5,
            color = col_cat_label) + 
  
  geom_text(data = sub_category_color, 
            aes(x  = x_label_point,
                y = segment_y,
                label = subcategory), 
            size = 2.5, 
            nudge_y = -0.04,
            lineheight = 0.75,
            vjust = 0.5,
            hjust = 0.5,
            color = col_cat_label) +
  # Time labels
  geom_text(data = category_color, 
            aes(x  = x_label_point, 
                y = segment_y, 
                label = time_label), 
            size = 3, 
            nudge_y = 0.02, 
            lineheight = 0.75, 
            vjust = 0, 
            hjust = 0.5, 
            fontface = "bold") +
  
  geom_text(data = sub_category_color, 
            aes(x  = x_label_point, y = segment_y, label = time_label), 
            size = 3, 
            nudge_y = -0.13, 
            lineheight = 0.75, 
            vjust = 0, 
            hjust = 0.5, 
            fontface = "bold") +


  scale_fill_identity() +
  scale_colour_identity() +
  coord_cartesian(clip = "off") +
  
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 

## USA plot 

USA_plot <- 
ggplot() + 
  # shading rect
  annotate("text", x = -0.45, y = -1.45, label = "USA", vjust = 0, hjust = 0, fontface = "bold", size = 12) +
  
  geom_segment(data = category_color_USA, aes(x  = x_label_point, xend = x_label_point,
                                          y = ymax, yend = segment_y, color = col)) +
  
  geom_segment(data = sub_category_color_USA, aes(x  = x_label_point, xend = x_label_point,
                                              y = ymax, yend = segment_y, color = sub_col)) +
  geom_point(data = category_color_USA, aes(x = x_label_point, y = ymax, color = col), size = point_size) +
  geom_point(data = sub_category_color_USA, aes(x = x_label_point, y = ymin, color = sub_col), size = point_size) +
  
  geom_point(data = category_color_USA, aes(x = x_label_point, y = segment_y, color = col), size = point_size) +
  geom_point(data = sub_category_color_USA, aes(x = x_label_point, y = segment_y, color = sub_col), size = point_size) +
  
  geom_rect(data = category_color_USA, aes(xmin  = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col)) +
  geom_rect(data = sub_category_color_USA, aes(xmin  = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sub_col)) +
  
  # Category and subcategory time labels
  geom_text(data = category_color_USA, 
            aes(x  = x_label_point,
                y = segment_y,
                label = time_label), 
            size = 3, 
            nudge_y = 0.10,
            lineheight = 0.75,
            vjust = 0.5,
            hjust = 0.5,
            color = "black",
            fontface = "bold") + 
  
  geom_text(data = sub_category_color_USA, 
            aes(x  = x_label_point,
                y = segment_y,
                label = time_label), 
            size = 3, 
            nudge_y = -0.04,
            lineheight = 0.75,
            vjust = 0.5,
            hjust = 0.5,
            color = "black") +
  # Difference Time labels
  geom_text(data = category_color_USA, 
            aes(x  = x_label_point, 
                y = segment_y, 
                label = diff_time_label), 
            size = 2.5, 
            nudge_y = 0.03, 
            lineheight = 0.75, 
            vjust = 0, 
            hjust = 0.5, 
            color = col_cat_label) +
  
  geom_text(data = sub_category_color_USA, 
            aes(x  = x_label_point, y = segment_y, label = diff_time_label), 
            size = 2.5, 
            nudge_y = -0.10, 
            lineheight = 0.75, 
            vjust = 0, 
            hjust = 0.5, 
            color = col_cat_label
) +
  
  
  scale_fill_identity() +
  scale_colour_identity() +
  coord_cartesian(clip = "off") +
  
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 

global_plot/USA_plot
