#### Publication + year data for ditch symposium output ####
# visualize the publication # per year for ditches, streams and rivers, and lakes
# 
# packages
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)

# Load your data (modify filenames and column names as needed)
ditch <- fread("C:/Users/teres/Documents/Conferences/Ditch symposium/Ditch symposium output data/ditch_data_table.txt")  %>%
mutate(term = "Ditch")

head(ditch)

stream_river <- fread("C:/Users/teres/Documents/Conferences/Ditch symposium/Ditch symposium output data/river_streams_data_table.txt") %>%
mutate(term = "Stream/River")

lake <- fread("C:/Users/teres/Documents/Conferences/Ditch symposium/Ditch symposium output data/lake_data_table.txt") %>%   
mutate(term = "Lake")

dat <- bind_rows(ditch, stream_river, lake)

head(dat)

# Plot

jpeg("Publications_by_year.jpg", units="in", width=8, height=6, res=300)

plot <- ggplot(subset(dat, `Publication Years` >= 1990), aes(x = `Publication Years`, y = `Record Count`, fill = factor(term, levels = c("Stream/River", "Lake", "Ditch") )))  +
  geom_col(color = "black") +
  labs( x = "Year", y = "# of Publications" ) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  theme_minimal() + theme(legend.title = element_blank(), axis.ticks = element_line(color = "black"), panel.grid = element_blank(), axis.line = element_line(color = "black"), axis.text.y = element_text(size=12, colour="black"),  axis.text.x =  element_text(size=12,  colour="black"), axis.title = element_text(size = 14,  colour="black"), legend.text = element_text(size = 12), panel.grid.major.y = element_line(color = "gray80", linewidth  = 0.5),  )
plot

dev.off()

#Note I subset data from 1990, and only used the first 1000 entries for lake and streams

