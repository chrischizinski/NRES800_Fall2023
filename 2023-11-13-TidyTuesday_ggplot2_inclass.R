# Tidy tuesday challenge
# Chris Chizinski
# 11-13-23

# 📚 load libraries ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(colorspace)

# install.packages(c('tidytuesdayR','colorspace'))
# pak::pak("thebioengineer/tidytuesdayR")

# 💾 load data ---------------------------------------------------------------
tt_data <- tt_load(2023, week = 37)

global_human_day <- tt_data$global_human_day |> 
  janitor::clean_names()

all_countries <- tt_data$all_countries |> 
  janitor::clean_names()

# 🖍️ colors --------------------------------------------------------------

pal <- c('#8cb369', '#f4e285', '#f4a259', '#5b8e7d', "#bc4b51")
pal <- colorRampPalette(pal)(8)
vizoR::show_pal(pal)
# devtools::install_github("aneuraz/vizoR")

lighten_amount <- seq(0, 1, length = 7)[2:6]

# 🤼 wrangle -----------------------------------------------------------------

box_width <- 0.25

# create a data.frame of the categories and the subcategories
all_countries |> 
  distinct(category, subcategory) -> categories

# working on the first bar chart

categories |> 
  right_join(global_human_day, join_by('subcategory')) |>
  select(-uncertainty) |> 
  arrange(category, desc(hours_per_day)) -> cat_subcat


cat_subcat |> 
  group_by(category) |> 
  summarise(hours_per_day = sum(hours_per_day)) |> 
  ungroup() |> 
  mutate(category = str_wrap(category, 14),
         category = factor(category),
         category_num = as.numeric(category),
         cuml_hrs = cumsum(hours_per_day),
         xmin = ifelse(row_number() ==1,0,lag(cuml_hrs)),
         xmax = cuml_hrs,
         ymin = 0,
         ymax = ymin + box_width,
         col = pal[category_num],
         midpoint = (xmax + xmin)/2,
         segment = ifelse(row_number() %% 2 == 0, 0.05, 0.15),
         point_height = ymax + segment,
         point_height = case_match(category,
                                   "Food provision" ~ ymax + 0.25,
                                   "Maintenance of\nsurroundings" ~ ymax + 0.35,
                                   .default = point_height),
         hrs = as.integer(hours_per_day),
         mins = as.integer((hours_per_day - hrs)*60),
         mins = str_pad(mins, 2, pad = "0", side = "left"),
         time_label = ifelse(hrs>0, paste0(hrs, "h ", mins, "m"), paste0(mins, "m"))
         ) -> category_color

category_color |> 
  select(hours_per_day, hrs, mins, time_label)

unique(category_color$category)


# 📊 plot --------------------------------------------------------------------

ggplot() +
  # main bar
  geom_rect(data = category_color,
            aes(xmin = xmin, 
                xmax = xmax, 
                ymin = ymin, 
                ymax = ymax, 
                fill = col)) + 
  # points on main bar
  geom_point(data = category_color, 
              aes(x = midpoint,y = ymax, color = col), size = 3) +
  #points above bar
  geom_point(data = category_color, 
             aes(x = midpoint,y = point_height, color = col), size = 3) +
  # line connecting points
  geom_segment(data = category_color,
               aes(x = midpoint,
                   xend = midpoint,
                   y = ymax,
                   yend = point_height,
                   color = col)) +
  # time label
  geom_text(data = category_color,
            aes(x = midpoint,
                y = point_height,
                label = time_label),
            nudge_y = 0.01,
            size = 3,
            vjust = 0) +
  # category label
  geom_text(data = category_color,
            aes(x = midpoint,
                y = point_height,
                label = category),
            nudge_y = 0.02,
            size = 2.5,
            vjust = 0,
            lineheight=0.75) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void()


