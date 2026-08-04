rm(list = ls())

##libraries

library(tidyverse)
library(patchwork)
library(here)
library(flowchart)
library(sf)
library(forcats)

#### Data

load(here("output","all_data.Rdata"))
regimeclassification <- readRDS(here("output","regimeclassification.rds"))
##global map for visualizations

worldmap <- st_read(here("data/shapefile/WorldMap_Continents/"))

theme_set(theme_bw())

all_data <- all_data_summ
#---------------------------------------
#### Flow Chart ####
#---------------------------------------

all_data_summ <- all_data|>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         ###create three indexes describing if the time series meets the inclusion criteria
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,  
                          true = 1, false = 0),
         index2 = if_else(completeness.10yrs >= 0.75, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0)) |> 
  left_join(regimeclassification |> ungroup()|>select(plot, group, class), by = c("plot","group")) |> 
  mutate(class.index1 = case_when(is.na(class)~NA_character_,
                                  class == "\novershoot"~"\novershoot\n",
                                  class == "\nestablished"~"\nestablished\n ",
                                  .default = ">90% decline"),
         class.index2 = case_when(class == "boom &\nbust"~"fast rate",
                                 class =="boom &\n sust. unk"~"fast rate",
                                 class =="unk rate &\nbust"~"fast rate",
                                 class =="unk rate &\nnot sust."~"fast rate",
                                 class =="slow rate &\nbust"~"slow rate",
                                 class =="slow rate &\nnot sust."~"slow rate",
                                 class =="\ncrash"~"slow rate",
                                 .default = NA_character_
                                 ),
         class.index3 = case_when(class == "boom &\nbust"~"boom &\nbust",
                                  class =="boom &\n sust. unk"~"boom &\nbust sust. unk.",
                                  class =="unk rate &\nbust"~"unk rate &\nbust",
                                  class =="unk rate &\nnot sust."~"unk rate &\nbust sust. unk",
                                  class =="slow rate &\nbust"~"slow rate",
                                  class =="slow rate &\nnot sust."~"slow rate",
                                  class =="\ncrash"~"slow rate",
                                  .default = NA_character_
         ),
         class.forfigure = case_when(class == "boom &\nbust"~"boom &\nbust",
                                  class =="boom &\n sust. unk"~"boom &\nbust sust. unk.",
                                  class =="unk rate &\nbust"~"unk rate &\nbust",
                                  class =="unk rate &\nnot sust."~"unk rate &\nbust sust. unk",
                                  class =="slow rate &\nbust"~"slow rate",
                                  class =="slow rate &\nnot sust."~"slow rate",
                                  class =="\ncrash"~"slow rate",
                                  class =="\nestablished" ~"\nestablished",
                                  class =="\novershoot"~"\novershoot",
                                  .default = NA_character_
         ))
 

index1.label <- "years surveyed < 10 years or \n < 8 years if longevity < 1"
index2.label <- "10 consecutive years < 75 % complete"
index3.label <- "harvest"


sum(all_data_summ[["index1"]]==0)
label_exc <- paste(
  c(str_glue("{sum(all_data_summ$index1 == 0 | all_data_summ$index2 == 0 | all_data_summ$index3 == 0, na.rm = T)} excluded:"),
    str_glue("- {sum(all_data_summ$index1 == 0, na.rm = TRUE)}: {index1.label}"),
    str_glue("- {sum(all_data_summ$index2 == 0, na.rm = TRUE)}: {index2.label}"),
    
    str_glue("- {sum(all_data_summ$index3 == 0, na.rm = TRUE)}: {index3.label}")),
  collapse = "\n")

all_data_summ |> 
  as_fc(label = "compiled \n timeseries",
        text_pattern = "{N} {label}") |> 
  fc_filter((index1==1 & index2 == 1 & index3 == 1),
            label = "met inclusion \n criteria",
            text_pattern = "{n} {label}",
            show_exc = TRUE,
            label_exc = label_exc,
            text_pattern_exc = "{label}",
            just_exc = "left",
            offset_exc = -0.1,
            direction_exc = "left") |> 
  fc_filter(!is.na(class),
            label = "regime shifts detected",
            text_pattern = "{n} {label}",
            show_exc = TRUE,
            label_exc = "no regime\nshifts detected",
            text_pattern_exc = "{n} {label}",
            just_exc = "left",
            offset_exc = -0.1,
            direction_exc = "left") |> 
  fc_split(class.index1, text_pattern = "{n} {label}") |> 
  fc_split(class.index3, text_pattern = "{n} {label}") |> 
  fc_draw() |> 
  fc_export(filename = here("output/figure_editing","flowchart.pdf"))


#----------------------------------------
  ####taxonomic and geogrpahic summaries###
#------------------------------------------

classification_summary <- regimeclassification |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                                    "boom &\nbust",
                                                    "unk rate &\nbust",
                                                    "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))

###birds
classification_summary_birds <- regimeclassification |> 
  filter(major.group == "Aves") |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))

###fish
classification_summary_fish<- regimeclassification |> 
  filter(major.group %in% c("Actinopterygii","Elasmobranchii","Dipneusti",
                            "Myxini")) |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))

### marine
classification_summary_marine<- regimeclassification |> 
  filter(ecosystem == "Marine") |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))

###freshwater
classification_summary_freshwater<- regimeclassification |> 
  filter(ecosystem == "Freshwater") |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))


##terrestrial
classification_summary_terrestrial<- regimeclassification |> 
  filter(ecosystem == "Terrestrial") |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))


###island

classification_summary_island<- regimeclassification |> 
  filter(island == "Y") |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))

classification_summary_mainland<- regimeclassification |> 
  filter(ecosystem != "Freshwater" & island == "N") |> 
  mutate(collapse = case_when(class %in% c("boom &\n sust. unk",
                                           "boom &\nbust",
                                           "unk rate &\nbust",
                                           "unk rate &\nsust. unk")~"90% decline",
                              class == "\novershoot"~"< 90% decline",
                              class == "\nestablished" ~ "no decline")) |> 
  group_by(collapse,native.species)|> 
  summarise(n = n()) |> 
  drop_na(collapse) |> 
  group_by(native.species) |> 
  mutate(total = sum(n),
         prop = n/total,
         low = prop-(1.96*sqrt((prop*(1-prop)/n))),
         upp = prop+(1.96*sqrt((prop*(1-prop)/n))),
         low = if_else(low <= 0, true = 0, false = low))

#contigency table test

contingency_table <-classification_summary |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_birds <-classification_summary_birds |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_fish <-classification_summary_fish |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_marine <-classification_summary_marine |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_freshwater <-classification_summary_freshwater |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_terrestrial <-classification_summary_terrestrial |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_island <-classification_summary_island |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

contingency_table_mainland <-classification_summary_mainland |> 
  select(n,collapse,native.species) |> 
  pivot_wider(values_from = n, names_from = native.species) |> 
  column_to_rownames(var = "collapse") |> 
  as.matrix() 

##contigency table tests
chi.test <- chisq.test(contingency_table)
chi.test.birds <- chisq.test(contingency_table_birds)
chi.test.fish <- chisq.test(contingency_table_fish)
chi.test.marine <- chisq.test(contingency_table_marine)
chi.test.freshwater <- chisq.test(contingency_table_freshwater)
chi.test.terrestrial <- chisq.test(contingency_table_terrestrial)
chi.test.mainland <- chisq.test(contingency_table_mainland)
chi.test.island <- chisq.test(contingency_table_island)



p1 <- classification_summary |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(0,0.61), breaks = seq(0,0.6, by = 0.2))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == 37.51 * "," ~ italic(P) * " < 0.001"),
  size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL)+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold")) + 
  coord_flip()



  
p2 <-classification_summary_birds |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(0,0.61), breaks = seq(0,0.6, by = 0.2))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "4.70" * "," ~ italic(P) * " = 0.095"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL)+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold")) + 
  coord_flip()

p3 <-classification_summary_fish |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(0,0.61), breaks = seq(0,0.6, by = 0.2))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "27.20" * "," ~ italic(P) * " < 0.001"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL)+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold")) + 
  coord_flip()


p4 <- classification_summary_marine |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(-0.1,0.71), breaks = seq(0,0.6, by = 0.2))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "0.327" * "," ~ italic(P) * " = 0.849"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL,
       title = "Marine")+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold"),
        title = element_text(size = 24, face = "bold")) + 
  coord_flip()


p5 <- classification_summary_freshwater |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(-0.1,0.71), breaks = seq(0,0.6, by = 0.2))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "41.58" * "," ~ italic(P) * " < 0.001"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL,
       title = "Freshwater")+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold"),
        title = element_text(size = 24, face = "bold")) + 
  coord_flip()

p6 <- classification_summary_terrestrial |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(-0.1,0.71), breaks = seq(0,0.6, by = 0.2))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "31.43" * "," ~ italic(P) * " < 0.001"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL,
       title = "Terrestrial")+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold"),
        title = element_text(size = 24, face = "bold")) + 
  coord_flip()

p7 <- classification_summary_island |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.25))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "24.31" * "," ~ italic(P) * " < 0.001"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL,
       title = "Island")+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold"),
        title = element_text(size = 24, face = "bold")) + 
  coord_flip()

p8 <- classification_summary_mainland |> 
  mutate(collapse = factor(collapse, levels = c("90% decline","< 90% decline","no decline"))) |> 
  ggplot(aes(x = collapse,y = prop, color = native.species))+
  geom_pointrange(aes(ymax = upp, ymin = low), size = 0.8, linewidth = 1,
                  position = position_dodge(width = 0.4))+
  scale_color_manual(values = c("#50164aff","cadetblue"),
                     name = NULL,
                     labels = c("Non-native",
                                "Native"))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.25))+
  annotate(
    "text",
    x = 3.4,
    y = 0.3,
    label = expression(chi^2 * "(" * 2 * ")" == "48.24" * "," ~ italic(P) * " < 0.001"),
    size = 5
  )+
  labs(y = "Proportion of Time Series",
       x = NULL,
       title = "Mainland")+
  theme(axis.text = element_text(size = 20, face = 'bold'),
        axis.title = element_text(size = 22, face=  "bold"),
        title = element_text(size = 24, face = "bold")) + 
  coord_flip()


ggsave(filename = here("output/figure_editing","fig4_proportions.png"),
       plot = p1, device = "png", units = "in",
       width = 7, height = 5)


panel.taxa <- p2 + p3 + plot_layout(guides = "collect")

ggsave(filename = here("output/figure_editing","fig4_proportions_taxa.png"),
       plot = panel.taxa, device = "png", units = "in",
       width = 15, height = 5.5)


panel.ecoystem <- p4 + p5 + p6 + plot_layout(guides = "collect")


ggsave(filename = here("output/figure_editing","fig4_proportions_ecosystem.png"),
       plot = panel.ecoystem, device = "png", units = "in",
       width = 20, height = 5.5)

panel.island <- p7+p8+ plot_layout(guides = "collect")


ggsave(filename = here("output/figure_editing","fig4_proportions_island.png"),
       plot = panel.island, device = "png", units = "in",
       width = 15, height = 5.5)
