library(dplyr)
library(ggplot2)


# example data: 
df <- tibble(player = c("Player1", "Player2", "Player3", "Player4"),
                 value = c(2500, 3500, 4000, 4900),
                 max = sample(5000:5500, 4))

# scale date: 
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
  bbplot::bbc_style()