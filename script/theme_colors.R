library(ggthemes)
library(purrr)
library(tidyverse)
library(santoku)

# extract the paletets;
canava_palettes <- 
ggthemes::canva_palettes                       # retruns a list hex value and names


# get table of hex, id, palette names and create three groups so that 200 paletts fit per page 
# for readability
df_palettes <-                                
purrr::map2_df(canava_palettes, names(canava_palettes),  #loop thru and return df
                 ~tibble(colors = .x, 
                             id = seq_along(colors), 
                        palette = .y)) %>%
     mutate(id_grp = 1:n(), 
             chopr = chop_equally(id_grp, 3, labels = LETTERS[1:3]))  

# function that plots the palletees per grp range
# there are 6
theme_colors <- function(x) {
df_palettes %>%
     arrange(palette) %>% 
     filter(chopr == x) %>% 
ggplot(aes(x = id, y = palette)) + 
     geom_tile(aes(fill = colors, label = colors), col = "gray56", size = 0.5, show.legend = FALSE) + 
     geom_label(aes( label = colors), col = "gray40",stat = "unique", face = "bold", fill = "gray88") +
     scale_fill_identity() +
     theme_void() + 
     facet_wrap(~ palette, ncol = 4, scales = "free_y") + 
     scale_y_discrete(expand = expansion(mult = c(0, 0.1))) + 
     theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", colour = "skyblue4"),
           plot.caption =  element_text(hjust = 0.5, size = 12, face = "bold", colour = "skyblue4")
           strip.text.x = element_text(size = 14, colour = "slategray4" , margin = margin(0,0,0,0, "cm")))
}

# Chop because I want to only have 200 palletes per page for redablity
sub_grp <- LETTERS[1:3]

# Use the function to plot each and add title and caption  
gg_a <-
theme_colors("A") + labs(title = "ggthemes colors 1-200", caption = "Source: ggthemese canavas palettes | Graphics: @datarecode")

gg_b <-
theme_colors("B") + labs(title = "ggthemes colors 201-400", caption = "Source: ggthemese canavas palettes | Graphics: @datarecode")


gg_c <-
theme_colors("C") + labs(title = "ggthemes colors 401-600", caption = "Source: ggthemese canavas palettes | Graphics: @datarecode")

# plot/save
