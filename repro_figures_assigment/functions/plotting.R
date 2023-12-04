plot_exploratory <- function(culmen_data_clean){
  culmen_data_clean %>%
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm)) +
    geom_point(aes(color = species)) +
    labs(x = "Culmen length (mm)", y = "Culmen depth (mm)", title = "Graph to show the correlation\nbetween culmen length and depth") +
    scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107"))+ #colourblind friendly palette 
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
}

plot_lm <- function(culmen_data_clean){
  culmen_data_clean %>%
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm)) +
    geom_point(aes(color = species)) +
    geom_smooth(method = "lm", se = TRUE, color = "black", size = 1) +
    labs(x = "Culmen length (mm)", y = "Culmen depth (mm)", title = "Graph to show the correlation between\nculmen length and depth across species") +
    scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107"))+ #colourblind friendly palette 
    stat_poly_eq(label.y = 1.5,label.x = 0.01, size = 2.75, use_label(c("eq", "adj.R2", "f", "p")))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}

plot_anova <- function(culmen_data_clean){
  culmen_data_clean%>%
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm, colour = species)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    labs(x = "Culmen length (mm)", y = "Culmen depth (mm)", title = "Graph to show the effect of species onculmen depth\nand the effect of culmen length on depth within species") +
    scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107"))+ #colourblind friendly palette 
    theme(plot.title = element_text(hjust = 0.5))+
    stat_poly_eq(
      label.y = c(1.0, 0.95, 0.9),  
      label.x = c(0.01, 0.01, 0.01),  
      size = 2.75,
      use_label(labels = c("eq", "adj.R2", "f", "p"))
    )+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 10))+
    ylim(13.5, 23)
}