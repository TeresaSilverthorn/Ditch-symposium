#### Publication + year data for ditch symposium output ####
# visualize the publication # per year for ditches, streams and rivers, and lakes
# 
# packages
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggpubr)
#
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

pub_plot <- ggplot(subset(dat, `Publication Years` >= 1990 & `Publication Years` <= 2024 ), aes(x = `Publication Years`, y = `Record Count`, fill = factor(term, levels = c( "Stream/River", "Lake", "Ditch" ) )))  +
  geom_col(color = "black") +
  labs( y = "# of publications" ) +
   scale_x_continuous(breaks = seq(1990, 2025, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c( "Stream/River"=  "#00798c", "Lake"= "#edae49", "Ditch" = "#ff6f61" )) +   #d1495b  "#f17c67",
  theme_minimal() + theme(legend.position = "top",  axis.title.x = element_blank(), legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=14, colour="black"),  axis.text.x =  element_text(size=14,  colour="black"), axis.title = element_text(size = 18,  colour="black"), legend.text = element_text(size = 18), panel.grid.major.y = element_line(color = "gray80", linewidth  = 0.5)  )
pub_plot

dev.off()

#Note I subset data from 1990 


# compare slopes for rates of increase between inland water types

lm_model <- lm(`Record Count` ~ `Publication Years` * term, data = dat)
summary(lm_model)

# Plots of yearly increase publication ratio as sugegsted by Jose
# Values near 1 indicate no change in publication numbers from the previous year.
# Values above 1 indicate an increase in publications for that type of system compared with the previous year.
# Values below 1 indicate a decrease.

jpeg("Publications_ratio_plot_point.jpg", units="in", width=8, height=6, res=300)

ratio_plot_point <- ggplot(subset(dat_ratio, `Publication Years` >= 1990 & `Publication Years` <= 2023 ), aes(x = `Publication Years`, y = Pub_Ratio, color = factor(term, levels = c( "Stream/River", "Lake", "Ditch" ))) ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_colour_manual(values = c( "Stream/River"=  "#00798c", "Lake"= "#edae49", "Ditch" = "#ff6f61" )) +
  labs(   x = "Year",   y = "Publication ratio") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  theme_minimal() + theme(legend.position = "top", axis.title.x = element_blank(), legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=14, colour="black"),  axis.text.x =  element_text(size=14,  colour="black"), axis.title = element_text(size = 16,  colour="black"), legend.text = element_text(size = 14), panel.grid.major.y = element_line(color = "gray80", linewidth  = 0.5) ) +
  geom_point(alpha=0.7, size=3.5) 
ratio_plot_point

dev.off()



jpeg("Publications_ratio_plot_bar.jpg", units="in", width=8, height=6, res=300)

ratio_plot_bar <- ggplot(subset(dat_ratio, `Publication Years` >= 1990 & `Publication Years` <= 2023 ), aes(x = `Publication Years`, y = Pub_Ratio, fill = factor(term, levels = c( "Stream/River", "Lake", "Ditch" ))) ) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c( "Stream/River"=  "#00798c", "Lake"= "#edae49", "Ditch" = "#ff6f61" )) +
  labs(   x = "Year",   y = "Publication ratio") +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  theme_minimal() + theme(legend.position = "top", axis.title.x = element_blank(), legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=14, colour="black"),  axis.text.x =  element_text(size=14,  colour="black"), axis.title = element_text(size = 16,  colour="black"), legend.text = element_text(size = 14), panel.grid.major.y = element_line(color = "gray80", linewidth  = 0.5) ) +
  scale_y_continuous(breaks = seq(0, 4, by = 1))
ratio_plot_bar

dev.off()


# GHG pie chart of contributions in GHG equivalents

# Data from GCB article, based on 22 studies that measured all 3 gases CH4 is ebul+diff
emissions <- data.frame(
  gas = c("CO2", "CH4", "N2O"),
  percentage = c(51, 43.5, 5.5))

# Create pie chart
pie <- ggplot(emissions, aes(x = "", y = percentage, fill = gas)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("CO2" = "#edae49", "CH4" = "#00798c", "N2O" = "#ff6f61")) +
  theme_void()
pie


jpeg("Contribution_by_gas.jpg", units="in", width=6, height=6, res=300)

bar_gas <- ggplot(emissions, aes(x = reorder(gas, -percentage), y = percentage, fill = gas)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(percentage, "%")),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("CO2" = "#edae49", "CH4" = "#00798c", "N2O" = "#ff6f61")) +
  scale_x_discrete(labels = c(expression(CO[2]), expression(CH[4]), expression(N[2]*O))) +
  labs( y = "% contribution to ditch emissions") +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 60))+
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank(), legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=14, colour="black"),  axis.text.x =  element_text(size=14,  colour="black"), axis.title = element_text(size = 18,  colour="black"), legend.text = element_text(size = 14) ) 
bar_gas

dev.off()

# combine

jpeg("Combined.jpg", units="in", width=12, height=5, res=300)

combine1 <- ggarrange(pub_plot, bar_gas, 
                      nrow = 1, ncol = 2, align = "v", widths = c( 1.5, 1))  
combine1

dev.off()
