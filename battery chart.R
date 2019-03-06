library(dplyr)
library(ggplot2)


###################Battery plot
# example data: 
df <- tibble(player = c("Player1", "Player2", "Player3", "Player4"),
                 value = c(2500, 3500, 4000, 4900),
                 max = sample(5000:5500, 4))

# scale data: 
df <- df %>% 
  mutate(value_scale = (value / max)*100,
         aim = rep(100, nrow(.)))


###function for colour code
colour_func <- function(x){ifelse(x$value_scale > 90, '#800000', 
                                  ifelse(x$value_scale < 60, '#000080', '#008000'))
}

ggplot(df, aes(x = player)) + 
  geom_bar(aes(y = aim), width = 0.85, stat = "identity", colour = "grey68") +
  geom_bar(width = 0.85, stat = "identity", 
                                aes(y = value_scale, fill = player), 
                                colour = "black") +
  geom_text(aes(x = player, y = 100), label = paste0(round(df$value_scale,0),'%'),
            colour = "azure4", vjust = -1) +
  scale_fill_manual(values = colour_func(df)) +
  labs(title ='Indication of Distance Compared to Highest',
       subtitle = 'Blue under 60%, Green 60-90%, Red over 90%') +
  xlab("") + ylab("") +
  coord_polar(theta ='y', start= -1.57) +
  bbplot::bbc_style()


###################Gauge style plot
# example data: 
df <- tibble(player = c("Player1", "Player2", "Player3", "Player4"),
             value = c(2500, 3500, 4000, 4900),
             max = sample(5000:5500, 4))

# scale data: 
df <- df %>% 
  add_row(player = "A", `value` = 0, max=0,.before = 1) %>%
  mutate(value_scale = (value / max)*100,
         value_sc_half = value_scale/2,
         aim = rep(50, nrow(.)))

df_sub <- filter(df, player != "A")
  
###function for colour code
colour_func <- function(x){ifelse(x$value_scale > 90, '#800000', 
                                  ifelse(x$value_scale < 60, '#000080', '#008000'))
}

ggplot(df, aes(x = player)) + 
  geom_bar(width = 0.85, stat="identity", aes(y = value_scale), 
           colour = "white", fill = "white") +
  geom_bar(data=df_sub, aes(y = aim), width = 0.85, stat = "identity", colour = "grey68") +
  geom_bar(data=df_sub, width = 0.85, stat = "identity", 
           aes(y = value_sc_half, fill = player), 
           colour = "black") +
  geom_text(data=df_sub, aes(x = player, y = 50), label = paste0(round(df_sub$value_scale,0),'%'),
            colour = "azure4", vjust = -1) +
  scale_fill_manual(values = colour_func(df)) +
  labs(title ='Indication of Distance Compared to Highest',
       subtitle = 'Blue under 60%, Green 60-90%, Red over 90%') +
  xlab("") + ylab("") +
  theme_classic()+
  coord_polar(theta ='y', start= -1.57) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
