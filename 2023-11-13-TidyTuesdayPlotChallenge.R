
# 📚 load libraries ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(colorspace)

# 💾 load data ---------------------------------------------------------------

tt_data <- tt_load(2023, week=37)

global_human_day <- tt_data$global_human_day |> 
  janitor::clean_names()

all_countries <- tt_data$all_countries |> 
  janitor::clean_names()


# colors ------------------------------------------------------------------

pal <- c('#8cb369', '#f4e285', '#f4a259', '#5b8e7d', "#bc4b51")
pal <- colorRampPalette(pal)(8)
vizoR::show_pal(pal)

lighten_amount <- seq(0, 1, length = 7)[2:6]

# 🤼 wrangle -----------------------------------------------------------------
box_width <- 0.25
tolerance <- 0.5

push_aside <- function(x, tolerance = 0.5, direction = "positive"){

  direct_num <- ifelse(direction == "positive", 1, -1)
  
  diff <- x - lag(x)
  new_x <- ifelse(diff < tolerance, x + abs(diff - tolerance)*direct_num, x)
         
  return(x)
}

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
         segment_y = push_aside(segment_y, tolerance = tolerance, direction = "positive"),
         col = pal[category_num],
         hrs = floor(hours_per_day),
         mins = str_pad(round((hours_per_day - hrs)*60), 2, side = "left", pad = "0"),
         time_label = paste0(hrs,"h"," ", mins, "m")) -> category_color

# work on the second bar - subcategories
cat_subcat |> 
  mutate(category_num = as.numeric(category),
         cuml_hrs = cumsum(hours_per_day),
         x_label_point = ifelse(row_number() == 1, hours_per_day/2, lag(cuml_hrs) + hours_per_day/2),
         xmin = ifelse(row_number() == 1, 0, lag(cuml_hrs)),
         xmax = cuml_hrs,
         ymax = ( -0.01 ), 
         ymin = ymax - box_width,
         segment_y = ifelse(row_number() %% 2 == 0,ymin - 0.05, ymin - 0.15),
         segment_y = push_aside(segment_y, tolerance = tolerance, direction = "negative"),
         col = pal[category_num],
         hrs = floor(hours_per_day),
         mins = str_pad(round((hours_per_day - hrs)*60), 2, side = "left", pad = "0"),
         time_label = paste0(hrs,"h"," ", mins, "m")) |> 
  group_by(category) |> 
  mutate(sub_category = 1:n(),
         sub_col = lighten(col, amount = lighten_amount[sub_category])) |>
  ungroup() -> sub_category_color
         
###########  
#   mutate(sub_cat = 1:n(),
#          category = factor(category),
#          order = as.numeric(category),
#          cuml_hrs = cumsum(ttl_hrs),
#          y_lo = lag(cuml_hrs) + ttl_hrs/2,
#          y_lo = ifelse(is.na(y_lo), ttl_hrs/2, y_lo),
#          y_lo_hrs = floor(ttl_hrs),
#          ylo_mins = str_pad(round((ttl_hrs - y_lo_hrs)*60), 2, side = "left", pad = "0"),
#          y_lo_label = paste0(y_lo_hrs,"h"," ", ylo_mins, "m"),
#          x_lo = case_match(category,
#                            "Nonfood\nprovision" ~ -2.5,
#                            .default = -2),
#          col = recode(category, !!!pal_names),
#          sub_col = lighten(col, amount = lighten_amount[sub_cat]),
#          type = 'subcategory',
#          col = sub_col) |> 
#   ungroup()  |> 
#   bind_rows(cat_with_colors)
# 
# cat_subcat |> 
#   group_by(category) |> 
#   summarise(ttl_hrs = sum(hours_per_day)) |> 
#   arrange(category, desc(ttl_hrs)) |>
#   ungroup() |> 
#   mutate(category = factor(category),
#          order = as.numeric(category),
#          cuml_hrs = cumsum(ttl_hrs),
#          y_lo = lag(cuml_hrs) + ttl_hrs/2,
#          y_lo = ifelse(is.na(y_lo), ttl_hrs/2, y_lo),
#          y_lo_hrs = floor(ttl_hrs),
#          ylo_mins = str_pad(round((ttl_hrs - y_lo_hrs)*60), 2, side = "left", pad = "0"),
#          y_lo_label = paste0(y_lo_hrs,"h"," ", ylo_mins, "m"),
#          x_lo = case_match(category,
#                            "Nonfood\nprovision" ~ 2.5,
#                            .default = 2),
#          col = recode(category, !!!pal_names),
#          type = 'category') -> cat_with_colors
# 
# cat_subcat |> 
#   rename(ttl_hrs = hours_per_day) |> 
#   arrange(category, desc(ttl_hrs)) |>
#   group_by(category) |> 
#   mutate(sub_cat = 1:n(),
#          category = factor(category),
#          order = as.numeric(category),
#          cuml_hrs = cumsum(ttl_hrs),
#          y_lo = lag(cuml_hrs) + ttl_hrs/2,
#          y_lo = ifelse(is.na(y_lo), ttl_hrs/2, y_lo),
#          y_lo_hrs = floor(ttl_hrs),
#          ylo_mins = str_pad(round((ttl_hrs - y_lo_hrs)*60), 2, side = "left", pad = "0"),
#          y_lo_label = paste0(y_lo_hrs,"h"," ", ylo_mins, "m"),
#          x_lo = case_match(category,
#                            "Nonfood\nprovision" ~ -2.5,
#                            .default = -2),
#          col = recode(category, !!!pal_names),
#          sub_col = lighten(col, amount = lighten_amount[sub_cat]),
#          type = 'subcategory',
#          col = sub_col) |> 
#   ungroup()  |> 
#   bind_rows(cat_with_colors) -> cat_subcat_with_colors
#   


  
# 📊 plot --------------------------------------------------------------------
x_adj <- 0.09
point_size <- 1.5



ggplot() + 
  
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
  
  scale_fill_identity() +
  scale_colour_identity() +
  
  theme_void() + 
  theme(legend.position = "none", 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 
  


ggplot(data = cat_subcat_with_colors) + 
  geom_col(aes(x = type, y = ttl_hrs, fill = forcats::fct_rev(col)), 
           width = 0.99, position = "stack") + 
  
  geom_point(aes(x = 1.5, y = y_lo, color = forcats::fct_rev(col)), size = point_size) +
  geom_point(aes(x = x_lo - x_adj, y = y_lo, color = forcats::fct_rev(col)), size = point_size) +
  geom_segment(aes(x = 1.5, y = y_lo, xend = x_lo - x_adj, yend = y_lo, color = forcats::fct_rev(col))) + 
  geom_text(aes(x = x_lo, y = y_lo, label = y_lo_label), 
            size = 3.5) +
  geom_text(aes(x = x_lo, y = y_lo, label = category), 
            size = 3.0, nudge_x = 0.30, lineheight = 0.75, vjust = 0.75) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_flip(xlim = c(0,3)) +
  
  theme_void() + 
  theme(legend.position = "none", plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 

