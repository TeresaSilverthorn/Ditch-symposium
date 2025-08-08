#### Publication + year data for ditch symposium output ####
# visualize the publication # per year for ditches, streams and rivers, and lakes
# 
# packages
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)

# Load your data (modify filenames and column names as needed)
ditch <- fread("C:/Users/teres/Documents/Conferences/Ditch symposium/Ditch symposium output data/ditch_canal_data_table.txt")  %>%
mutate(term = "Ditch")

head(ditch)

stream_river <- fread("C:/Users/teres/Documents/Conferences/Ditch symposium/Ditch symposium output data/river_streams_data_table2.txt") %>%
mutate(term = "Stream/River")

lake <- fread("C:/Users/teres/Documents/Conferences/Ditch symposium/Ditch symposium output data/lake_data_table2.txt") %>%   
mutate(term = "Lake")

dat <- bind_rows(ditch, stream_river, lake)

head(dat)

# Calculate publication ratio per term (system) N(i+1) / N(i)
#where N is the number of publications on streams/rivers, lakes, or ditches in a given year (i). This would produce a plot for 1990–2025 in which points are distributed above and below 1


dat_ratio <- dat %>%
  arrange(term, `Publication Years`) %>%
  group_by(term) %>%
  mutate(Pub_Ratio = lead(`Record Count`) / `Record Count`) %>% 
  ungroup()


# Plot

jpeg("Publications_by_year.jpg", units="in", width=8, height=6, res=300)

plot <- ggplot(subset(dat, `Publication Years` >= 1990 & `Publication Years` <= 2024 ), aes(x = `Publication Years`, y = `Record Count`, fill = factor(term, levels = c( "Stream/River", "Lake", "Ditch" ) )))  +
  geom_col(color = "black") +
  labs( y = "# of Publications" ) +
   scale_x_continuous(breaks = seq(1990, 2025, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c( "Stream/River"=  "#00798c", "Lake"= "#edae49", "Ditch" = "#ff6f61" )) +   #d1495b  "#f17c67",
  theme_minimal() + theme(legend.position = "top",  axis.title.x = element_blank(), legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=14, colour="black"),  axis.text.x =  element_text(size=14,  colour="black"), axis.title = element_text(size = 16,  colour="black"), legend.text = element_text(size = 14), panel.grid.major.y = element_line(color = "gray80", linewidth  = 0.5)  )
plot

dev.off()

#Note I subset data from 1990 


# compare slopes for rates of increase between inland water types

lm_model <- lm(`Record Count` ~ `Publication Years` * term, data = dat)
summary(lm_model)

# Plots of yearly increase publication ratio as sugegsted by Jose
# Values near 1 indicate no change in publication numbers from the previous year.
# Values above 1 indicate an increase in publications for that type of system compared with the previous year.
# Values below 1 indicate a decrease.

jpeg("Publications_ratio_plot.jpg", units="in", width=8, height=6, res=300)

ratio_plot <- ggplot(subset(dat_ratio, `Publication Years` >= 1990 & `Publication Years` <= 2023 ), aes(x = `Publication Years`, y = Pub_Ratio, color = factor(term, levels = c( "Stream/River", "Lake", "Ditch" ))) ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_colour_manual(values = c( "Stream/River"=  "#00798c", "Lake"= "#edae49", "Ditch" = "#ff6f61" )) +
  labs(   x = "Year",   y = "Publication ratio") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  theme_minimal() + theme(legend.position = "top", axis.title.x = element_blank(), legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=14, colour="black"),  axis.text.x =  element_text(size=14,  colour="black"), axis.title = element_text(size = 16,  colour="black"), legend.text = element_text(size = 14), panel.grid.major.y = element_line(color = "gray80", linewidth  = 0.5) ) +
  geom_point(alpha=0.7, size=3.5) 
ratio_plot

dev.off()
