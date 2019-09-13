###############################################################################
#
# This script takes and transforms the data
# provided at https://data.world/makeovermonday/2019w28 
# to make a plot that shows the number of applications
# of assylum in Spain and acceptance/denegation
# from 2009 to 2018.
# Contains data by Eurostat, courtesy of the European Asylum Support Office,
# that can be found on the International protection in the EU+: 2018 overview
# (https://www.easo.europa.eu/asylum-trends-annual-report-2018)
#
###############################################################################

# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(httr)
library(glue)
library(rcartocolor)
library(cowplot)

# get and read data ----------------------------------------------------------------

if(!file.exists("./2019_w28_data_decision.xls")) {
  GET("https://query.data.world/s/bgxvtr2aubwuin35e2tkkno2chbew5", write_disk("./2019_w28_data_decision.xls"))
  }

if(!file.exists("./2019_w28_data_details.xls")) {
  GET("https://query.data.world/s/mhs27cb3mrvqd2tieztnvzqoait7eo", write_disk("./2019_w28_data_details.xls"))
}

custom_repair <- function(x) {gsub("[\\s/]", "_", tolower(x), perl = TRUE)}

data_decision <- read_xls("./2019_w28_data_decision.xls", skip = 1, .name_repair = custom_repair)
  
data_details <- read_xls("./2019_w28_data_details.xls", .name_repair = custom_repair)


# transform ---------------------------------------------------------------

spain_assyl_decision <-
  data_decision %>%
  filter(
    geo_time == "Spain",
    !decision %in% c("Geneva Convention status", "Humanitarian status", "Total")
  ) %>%
  select(decision, `2009`:`2018`) %>%
  pivot_longer(-decision, names_to = "year", values_to = "count") %>%
  mutate(
    decision = if_else(decision == "Total positive decisions", "accepted", "rejected"),
    count = as.numeric(count),
    dummy_orig = 1,
    year = as.Date(ISOdate(as.numeric(year), 1, 1)),
  )

# plot --------------------------------------------------------------------

year_count_plot <- 
spain_assyl_decision %>% 
  group_by(year) %>% 
  summarise(sum = sum(count)) %>% 
  mutate(colors = carto_pal(10, "Safe")) %>% 
ggplot() +
  geom_curve(aes(x = 1, y = as.Date("2025-01-01"), xend = 1, yend = year, size = sum, color = colors), 
             lineend = "round") +
  scale_size(range = c(0.5, 4), guide = "none") +
  scale_x_continuous(position = "top") +
  scale_y_date(position = "right", date_breaks = "1 year", date_labels = "%Y") +
  geom_text(aes(x = 1, y = year, label = glue("{strftime(year, '%Y')}"), color = colors), 
            hjust = -0.3, family = "Ubuntu Mono") +
  scale_color_identity() +
  theme_void() +
  theme(text = element_text(family = "Ubuntu Mono"),
        plot.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "transparent", fill = "transparent"))

accept_reject_plot <- 
spain_assyl_decision %>% 
  group_by(year) %>% 
  mutate(total_y = sum(count)) %>% 
  ungroup() %>% 
  mutate(
    prop_dec = count / total_y,
    colors = rep(carto_pal(10, "Safe"), 2)
    ) %>% 
  ggplot() +
  geom_curve(aes(x = "ab", y = year, xend = decision, yend = as.Date("2002-01-01"), 
                 size = prop_dec, color = colors), lineend = "round", curvature = -.5) +
  scale_size(range = c(0.5, 4), guide = "none") +
  scale_y_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_x_discrete(breaks = c("accepted", "rejected"), 
                   labels = c("Aceptada", "Rechazada")) +
  scale_color_identity() +
  theme_void() +
  theme(text = element_text(family = "Ubuntu Mono"),
        plot.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "transparent", fill = "transparent"),
        axis.text.x = element_text())

assembled_plot <- 
ggdraw() +
  draw_plot(year_count_plot, x = -.11, y = .35, height = .7) +
  draw_plot(accept_reject_plot, x = .37, y = 0.06, height = 0.71, width = .55) +
  draw_text("Solicitues de asilo a España desde 2009", .95, .9, family = "Ubuntu Mono", 
            hjust = 1, size = 16, fontface = "bold", color = "#747e8f") +
  annotate("text", x = .95, y = .85, size = 4, family = "Ubuntu Mono", hjust = 1,
           label = "Grosor de línea representa volumen de solicitues o\nproporción de aceptación/denegación",) +
  annotate("curve", x = .2, y = .85, xend = .15, yend = .95, curvature = -.4, 
           linetype = "dotted") +
  annotate("text", x = .15, y = .96, size = 4, family = "Ubuntu Mono",
           label = "Volumen de solicitudes") +
  annotate("curve", x = .55, y = .34, xend = .5, yend = .27, linetype = "dotted", curvature = -.4) +
  annotate("text", x = .5, y = .27, label = "Proporción de solicutes\naceptadas/denegadas",
           family = "Ubuntu Mono", hjust = 1) +
  annotate("text", x = .05, y = 0.05, label = "Gráfico: @pabrodez | Fuente: Eurostat", family = "Ubuntu Mono", size = 3.5, hjust = 0) +
  theme(plot.background = element_rect(fill = "grey85"))


ggsave(plot = assembled_plot, filename = "./plots/spain_assyl.png", dpi = "retina", height = 12, width = 8)
