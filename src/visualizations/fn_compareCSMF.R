fn_compareCSMF <- function(DAT1, DAT2, DAT3 = NULL, DAT4 = NULL, DAT5 = NULL,
                           REGIONAL = FALSE, SAMPLE = NULL, AGG = FALSE, 
                           LEVEL1 = NULL, 
                           LEVEL2 = NULL, 
                           LEVEL3 = NULL, 
                           LEVEL4 = NULL,
                           LEVEL5 = NULL,
                           CTRYGRP = NULL){
  
  #' @title Compare CSMFs
  # 
  #' @description Compare CSMFs for each cause to another set of results
  #
  #' @param DAT1 Data frame 1 with all identifying columns and formatted point estimates
  #' @param DAT2 Data frame 2 with all identifying columns and formatted point estimates
  #' @param REGIONAL Boolean with true/false value if regional estimates
  #' @param SAMPLE Vector of sample of ISO3 codes to plot
  #' @param CTRYGRP NULL will include all countries. Can limit to HMM, LMM, or VR
  #' @return PDF plots with one country/region per page and facets for COD. Each facet is a line graph for the CSMF over the years being estimated.
  
  # # testing
  # DAT1 <- point_PanchoResultsFRMT
  # DAT2 <- point6
  # DAT3 <- NULL
  # DAT4 <- NULL
  # DAT5 <- NULL
  # LEVEL1 = "Simple2021"
  # LEVEL2 = "Full2023 - new HP"
  # REGIONAL = FALSE
  # SAMPLE = NULL
  # AGG = FALSE
  # CTRYGRP = NULL

  # DAT1 <- point_PanchoResults
  # DAT2 <- point2
  # DAT3 <- point4
  # DAT4 <- point5
  # DAT5 <- point6
  # REGIONAL = FALSE
  # SAMPLE = NULL
  # AGG = FALSE
  # LEVEL1 = "Simple2021"
  # LEVEL2 = "Full2023 - same covar set"
  # LEVEL3 = "Full2023 - new covar set"
  # LEVEL4 = "Full2023 - ctry RE"
  # LEVEL5 = "Full2023 - new HP"
  # CTRYGRP = c("HMM", "VR")
  
  if(REGIONAL == FALSE){
    DAT2$name <- DAT2$ISO3
    DAT1$name <- DAT1$ISO3
  }else{
    DAT2$name <- DAT2$Region
    DAT1$name <- DAT1$Region
  }
  if(!is.null(DAT3)){
    if(REGIONAL == FALSE){
      DAT3$name <- DAT3$ISO3
    }else{
      DAT3$name <- DAT3$Region
    }
  }
  if(!is.null(DAT4)){
    if(REGIONAL == FALSE){
      DAT4$name <- DAT4$ISO3
    }else{
      DAT4$name <- DAT4$Region
    }
  }
  if(!is.null(DAT5)){
    if(REGIONAL == FALSE){
      DAT5$name <- DAT5$ISO3
    }else{
      DAT5$name <- DAT5$Region
    }
  }
  
  
  # Harmonize Sex names
  DAT1$Sex[DAT1$Sex == "Total"] <- sexLabels[1]
  DAT1$Sex[DAT1$Sex == "T"] <- sexLabels[1]
  DAT1$Sex[DAT1$Sex == "B"] <- sexLabels[1]
  DAT1$Sex[DAT1$Sex == "F"] <- sexLabels[2]
  DAT1$Sex[DAT1$Sex == "M"] <- sexLabels[3]
  DAT2$Sex[DAT2$Sex == "Total"] <- sexLabels[1]
  DAT2$Sex[DAT2$Sex == "T"] <- sexLabels[1]
  DAT2$Sex[DAT2$Sex == "B"] <- sexLabels[1]
  DAT2$Sex[DAT2$Sex == "F"] <- sexLabels[2]
  DAT2$Sex[DAT2$Sex == "M"] <- sexLabels[3]
  
  if(AGG == FALSE){
    DAT1 <- subset(DAT1, Sex == sexLabel)
    DAT2 <- subset(DAT2, Sex == sexLabel)
  }
  
  DAT1$update <- LEVEL1
  DAT2$update <- LEVEL2
  
  dat <- bind_rows(DAT1, DAT2)
  dat$update <- factor(dat$update, levels = c(LEVEL1, LEVEL2))
  
  if(!is.null(DAT3)){
    DAT3$Sex[DAT3$Sex == "Total"] <- sexLabels[1]
    DAT3$Sex[DAT3$Sex == "T"] <- sexLabels[1]
    DAT3$Sex[DAT3$Sex == "B"] <- sexLabels[1]
    DAT3$Sex[DAT3$Sex == "F"] <- sexLabels[2]
    DAT3$Sex[DAT3$Sex == "M"] <- sexLabels[3]
    if(AGG == FALSE){
      DAT3 <- subset(DAT3, Sex == sexLabel)
    }
    DAT3$update <- LEVEL3
    dat <- bind_rows(DAT1, DAT2, DAT3)
    dat$update <- factor(dat$update, levels = c(LEVEL1, LEVEL2, LEVEL3))
  }
  if(!is.null(DAT4)){
    DAT4$Sex[DAT4$Sex == "Total"] <- sexLabels[1]
    DAT4$Sex[DAT4$Sex == "T"] <- sexLabels[1]
    DAT4$Sex[DAT4$Sex == "B"] <- sexLabels[1]
    DAT4$Sex[DAT4$Sex == "F"] <- sexLabels[2]
    DAT4$Sex[DAT4$Sex == "M"] <- sexLabels[3]
    if(AGG == FALSE){
      DAT4 <- subset(DAT4, Sex == sexLabel)
    }
    DAT4$update <- LEVEL4
    dat <- bind_rows(DAT1, DAT2, DAT3, DAT4)
    dat$update <- factor(dat$update, levels = c(LEVEL1, LEVEL2, LEVEL3, LEVEL4))
  }
  if(!is.null(DAT5)){
    DAT5$Sex[DAT5$Sex == "Total"] <- sexLabels[1]
    DAT5$Sex[DAT5$Sex == "T"] <- sexLabels[1]
    DAT5$Sex[DAT5$Sex == "B"] <- sexLabels[1]
    DAT5$Sex[DAT5$Sex == "F"] <- sexLabels[2]
    DAT5$Sex[DAT5$Sex == "M"] <- sexLabels[3]
    if(AGG == FALSE){
      DAT5 <- subset(DAT5, Sex == sexLabel)
    }
    DAT5$update <- LEVEL5
    dat <- bind_rows(DAT1, DAT2, DAT3, DAT4, DAT5)
    dat$update <- factor(dat$update, levels = c(LEVEL1, LEVEL2, LEVEL3, LEVEL4, LEVEL5))
  }
  
  
  # Delete unnecessary columns
  dat <- dat[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx"), names(dat))]
  
  # Reshape mortality fractions into long format
  dat <- melt(setDT(dat), id.vars = c("update","name","Model","AgeLow","AgeUp", "Sex","Year"))
  
  # Sample countries for national results
  if(length(SAMPLE) > 0){
    dat <- subset(dat, name %in% SAMPLE)
  }
  
  # PATCH ----------#
  # 2024-10-03
  # To only show countries/regions that are present in the current results (we only estimated HMM countries for Barcelona)
  # LEVEL2 if i only want hmm
  # LEVEL3 if i want hmm and lmm
  #v_reported_current <- unique(subset(dat, update ==  LEVEL3)$name)
  #dat <- subset(dat, name %in% v_reported_current)
  # ---------------#
  
  if(ageSexSuffix == "15to19yM"){
    dat <- subset(dat, variable != "Maternal")
  }
  
  # # To test single plot
  # dattest <- subset(dat, name == "AFG")
  # ggplot(data = dattest) +
  #   geom_line(aes(x=Year, y=value, color = update, linetype = update), linewidth = 1) +
  #   #labs(title = x$name, subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) +
  #   xlab("") + ylab("") +
  #   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
  #   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  #   scale_color_manual(values = c("gray", "firebrick2")) +
  #   scale_linetype_manual(values = c("solid", "longdash")) +
  #   facet_wrap(~variable) +
  #   theme_classic() +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_blank(),
  #         panel.border = element_rect(colour = "black", fill = NA),
  #         plot.subtitle = element_text(hjust = 0),
  #         axis.text = element_text(size = 8))
  
  if(!is.null(CTRYGRP)){
    dat <- subset(dat, Model %in% CTRYGRP)
  }
  
  # Sometimes a country was VR last round and LMM this round. Create combined model label.
  dat <- dat %>%
    group_by(name) %>%
    dplyr::summarise(Model_comb = paste(unique(Model), collapse = " and ")) %>%
    left_join(dat, ., by = "name")
  
  
  # Order plots alphabetically by world region and then nation
  # commenting out to avoid plyr
  # plots <- dlply(dat, ~name,
  #                function(x)
  #                  ggplot(data = x) + 
  #                  geom_line(aes(x=Year, y=value, color = update), linewidth = 1) +
  #                  labs(title = x$name, subtitle = paste(x$Model_comb,", ", x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
  #                  xlab("") + ylab("") +
  #                  coord_cartesian(xlim = c(2000,2024), ylim = c(0,.8)) +
  #                  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  #                  scale_color_manual(values = c("gray", "firebrick2", "dodgerblue3", "darkorchid2", "forestgreen")) +
  #                  facet_wrap(~variable) +
  #                  theme_classic() +
  #                  theme(panel.grid.major = element_blank(),
  #                        panel.grid.minor = element_blank(),
  #                        strip.background = element_blank(),
  #                        panel.border = element_rect(colour = "black", fill = NA),
  #                        plot.subtitle = element_text(hjust = 0),
  #                        axis.text = element_text(size = 8))
  #                )
  plots <- lapply(split(dat, dat$name), function(x) {
    ggplot(data = x) + 
      geom_line(aes(x = Year, y = value, color = update), linewidth = 1) +
      labs(
        title = x$name[1],  # use first since all rows same
        subtitle = paste(x$Model_comb[1], ", ", x$AgeLow[1], "-", x$AgeUp[1], ", ", x$Sex[1], sep = "")
      ) + 
      xlab("") + ylab("") +
      coord_cartesian(xlim = c(2000, 2024), ylim = c(0, .8)) +
      scale_x_continuous(breaks = c(2000, 2010, 2020)) +
      scale_color_manual(values = c("gray", "firebrick2", "dodgerblue3", "darkorchid2", "forestgreen")) +
      facet_wrap(~variable) +
      theme_classic() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.subtitle = element_text(hjust = 0),
        axis.text = element_text(size = 8)
      )
  })
  
  mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
  
  return(mg)
  
}
