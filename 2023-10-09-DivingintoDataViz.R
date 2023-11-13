# Christopher CHizinski
# 2023-10-09
# Using ggplot2 for visualization

# Load libraries 
library(tidyverse)
library(palmerpenguins)

## Basic plot

## Add some color -- all the same color
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm),
             color = "#e41c38")

## Add some color -- color varies by species
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species))

## add labels
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       title = "Penguins",
       subtitle = "Bill depth and length",
       color = "Species",
       caption = "Some caption information")

## using themes
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       title = "Penguins",
       subtitle = "Bill depth and length",
       color = "Species",
       caption = "Some caption information") +
  theme_classic()


ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       title = "Penguins",
       subtitle = "Bill depth and length",
       color = "Species",
       caption = "Some caption information") +
  theme_void()

ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  scale_color_brewer(palette = "Set1") +
  theme_classic()

## Changing colors manually
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  scale_color_manual(values = c("Adelie" = "chartreuse", "Chinstrap" = "firebrick", "Gentoo"= "darkblue")) +
  theme_classic()

## Changing the coordinates of the plot
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  coord_cartesian(xlim = c(0,25),
                  ylim = c(0,60),
                  expand = FALSE) +
  theme_classic()

## Flipping the x and y axes
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  coord_flip() +
  theme_classic()

# Polar coordinates
ggplot(data = penguins) +
  geom_point(aes(x = bill_depth_mm,
                 y = bill_length_mm,
                 color = species)) +
  coord_polar() +
  theme_classic()
