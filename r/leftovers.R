# areadistr_count_plot <- ggplot(data = df_filtered) +
#                           geom_line(
#                             stat= "bin", 
#                             binwidth = 0.25,
#                             mapping = aes(color = od_factor, x = log10(Area), y = log10(stat(count)))
#                           ) +
#                           ggtitle ("Area Distribution _ Counts") +
#                           facet_grid(~ compound, scale = "fixed")



# areadistr_density_plot <- ggplot(data = df_filtered) +
#                             geom_line(
#                               stat= "bin", 
#                               binwidth = 0.25,
#                               mapping = aes(color = od_factor, x = log10(Area), y = 0.25*(stat(density)))
#                             ) +
#                             ggtitle ("Area Distribution _ Density") +
#                             facet_grid(~ compound, scale = "fixed")
## oplossing proportions@https://stackoverflow.com/questions/22181132/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion-by-group/22181949#22181949?newreg=80f4ac5944204e8ab23b6a0c61fc6304
