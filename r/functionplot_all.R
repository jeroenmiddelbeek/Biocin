#' @title A function that plots it all
#' @param n A character vector, currenly implemented for "od" or "comp_dil"  
#'
#'


plot_all <- function(n){ 

##function for  n = comp_dil

count_plot <-       ggplot(data = sum_image_rep, mapping = aes(x = log10(get(n)), y = count)) +
                      geom_line(
                              mapping = aes(color = image_rep)) +
                      ggtitle("Particle Count") +
                      facet_grid(well_rep ~ od, scale = "fixed")

density_plot <-  ggplot(data = df_filtered, mapping = aes(x=log10(Area), y = comp_dil_factor, fill = od)) +
                                geom_density_ridges(
                                  mapping = aes(height=(stat(density))),
                                  bandwidth = 0.05,
                                  alpha = 0.5
                                ) +
                                ggtitle ("Area Distribution (Density)") +
                                theme_ridges(center_axis_labels = TRUE) +
                                scale_x_continuous(expand = c(0.01, 0)) +
                                scale_y_discrete(expand = c(0.01, 0)) 
 
particlecolor_plot <- ggplot(data = df_filtered, mapping = aes(x = log10(get(n)), y = log10(Area))) +
                        geom_point(
                                  mapping = aes(color = well_rep, group = get(n)),
                                  alpha = 0.1,
                                  position = "jitter" ) +
                        ggtitle("Particle Size (log)") +
                        facet_wrap(~ od)

particle_plot <-      ggplot(data = df_filtered, mapping = aes(x = log10(get(n)), y = log10(Area))) +
                        geom_point(
                                  alpha = 0.1,
                                  position = "jitter" ) +
                        ggtitle("Particle Size (log)") +
                        facet_wrap(~ od)

#INTRAWELL REPRODUCIBILITY
intrawell_plot <- ggplot(data = sum_image_rep, mapping = aes(x = log10(get(n)), y = log10(mean_area), color = image_rep)) +
                    geom_line() +
                    #geom_errorbar(mapping = aes(x = log10(get(n)), ymin = log10(mean_area - sd_area) , ymax = log10(mean_area + sd_area))) +
                    ggtitle("Intrawell Reproducibility") +
                    facet_grid(well_rep ~ od)

#INTERWELL REPRODUCIBILITY
interwell_plot <- ggplot(data = sum_well_rep, mapping = aes(x = log10(get(n)), y = log10(mean_image))) +
                    geom_ribbon(mapping = aes(x = log10(get(n)), ymin=log10(mean_image - sem_image), ymax=log10(mean_image + sem_image), fill = well_rep, alpha=0.1)) +
                    geom_line(mapping = aes(color = well_rep)) +
                    ggtitle("Interwell Reproducibility") +
                    facet_wrap(~ od)

#EXPERIMENT
experiment_plot <-ggplot(data = sum_exp, mapping = aes(x = log10(get(n)), y = log10(mean_well))) +
                    geom_ribbon(mapping = aes(x = log10(get(n)), ymin = log10(mean_well - sem_well), ymax = log10(mean_well + sem_well), alpha = 0.1)) +
                    geom_line() +
                    ggtitle("Experiment") +
                    facet_wrap(~ od)

##FUNCTION FOR n = od
# count_plot <- ggplot(data = sum_image_rep, mapping = aes(x = od, y = count)) +
#                 geom_line(
#                   mapping = aes(color = image_rep)) +
#                 ggtitle("Particle Count") + 
#                 facet_grid(well_rep ~ compound, scale = "fixed")
# 
# density_plot <-  ggplot(data = df_filtered, mapping = aes(x=log10(Area), y = od_factor, fill = compound)) +
#                                 geom_density_ridges(
#                                   bandwidth = 0.075,
#                                   alpha = 0.5
#                                 ) +
#                                 ggtitle ("Area Distribution (Density)") +
#                                 theme_ridges(center_axis_labels = TRUE) +                    
#                                 scale_x_continuous(expand = c(0.01, 0)) +
#                                 scale_y_discrete(expand = c(0.01, 0))
#                               
#                                 #ggsave(filename = here::here("images", "test.svg"), height = 6, width = 9, units = "in", dpi = 300)
#                                 #ggsave(filename = here::here("images", "test.png"), height = 6, width = 9, units = "in", dpi = 300)
# 
#                                 ##https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# 
# particlecolor_plot <- ggplot(data = df_filtered, mapping = aes(x = od, y = log10(Area))) +
#                         geom_point(
#                           mapping = aes(color = well_rep, group = od),
#                           alpha = 0.1,
#                           position = "jitter" ) +
#                         ggtitle("Particle Size (log)") +
#                         facet_wrap(~ compound)
# 
# particle_plot <- ggplot(data = df_filtered, mapping = aes(x = od, y = log10(Area))) +
#                   geom_point(
#                     alpha = 0.1,
#                     position = "jitter" ) +
#                   ggtitle("Particle Size (log)") +
#                   facet_wrap(~ compound)
# 
# #INTRAWELL REPRODUCIBILITY
# intrawell_plot <- ggplot(data = sum_image_rep, mapping = aes(x = od, y = log10(mean_area), color = image_rep)) +
#                     geom_line() +
#                     ggtitle("Intrawell Reproducibility") +
#                     facet_grid(well_rep ~ compound)
# 
# #INTERWELL REPRODUCIBILITY
# interwell_plot <- ggplot(data = sum_well_rep, mapping = aes(x = od, y = log10(mean_image))) +
#                     geom_ribbon(mapping = aes(x = od, ymin=log10(mean_image - sem_image), ymax=log10(mean_image + sem_image), fill = well_rep, alpha=0.1)) +
#                     geom_line(mapping = aes(color = well_rep)) +
#                     ggtitle("Interwell Reproducibility") +
#                     facet_wrap(~ compound)
# 
# #EXPERIMENT
# experiment_plot <- ggplot(data = sum_exp, mapping = aes(x = od, y = log10(mean_well))) +
#                     geom_ribbon(mapping = aes(x = od, ymin = log10(mean_well - sem_well), ymax = log10(mean_well + sem_well), alpha = 0.1)) +
#                     geom_line() +
#                     ggtitle("Experiment") +
#                     facet_wrap(~ compound)

#RETURN
return(all_plots <- list(count_plot,
                        density_plot,
                        particlecolor_plot,
                        particle_plot, 
                        intrawell_plot,
                        interwell_plot,
                        experiment_plot 
                        )
      )
}



