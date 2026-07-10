
library(here)
library(tidyverse)
library(patchwork)


##time series summaries

spp.mult.pops <- names(table(regimeclassification$species.names))[table(regimeclassification$species.names)>1]

test <- regimeclassification |> 
  filter(species.names %in% spp.mult.pops) |> 
  mutate(class = case_when(class == "boom &\nbust"|
                             class == "boom &\n sust. unk"|
                             class == "unk rate &\nbust"~ "rapid collapse",
                            .default = "no rapid collapse")) |> 
  group_by(species.names,class) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = class,values_from = n, values_fill = 0) |> 
  mutate(total = `no rapid collapse`+ `rapid collapse`,
         percent = `rapid collapse`/total) #|> 
  #drop_na(class)


test |> 
  #filter(class == "rapid collapse") |> 
  ggplot(aes(x = fct_reorder(species.names, percent, median),
             y = percent*100)) + 
  geom_segment(aes(xend = species.names), yend = 0, color = "#666666") + 
  geom_point(size = 2, color = "black", fill = "#50164aff",  shape = 21)+
  geom_text(aes(label = paste0("(",test$total,")")),
            position = position_nudge(y = 4))+
  scale_y_continuous(limits = c(0,104), breaks = seq(0,100,by = 20))+
  coord_flip()+
  labs(y = "Percent (%)", x = "Species")+
  theme(legend.position = c(.6,.25),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank())+
  theme_bw()

test |> 
  filter(class == "overshoot") |> 
  ggplot(aes(x = fct_reorder(species.names, percent, median),
             y = percent*100)) + 
  geom_segment(aes(xend = species.names), yend = 0, color = "#666666") + 
  geom_point(size = 2, color = "black", fill = "#50164aff",  shape = 21)+
  geom_text(aes(label = total),position = position_nudge(y = 3))+
  facet_wrap(~class)+
  scale_y_continuous(limits = c(0,103), breaks = seq(0,100,by = 20))+
  coord_flip()+
  labs(y = "Percent (%)", x = "Species")+
  theme(legend.position = c(.6,.25),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank())+
  theme_bw()

test <- regimeclassification |> 
  filter(species.names %in% spp.mult.pops) |> 
  mutate(class = case_when(class == "boom &\nbust"|
                             class == "boom &\n sust. unk"|
                             class == "unk rate &\nbust"~ "rapid collapse",
                           class == "\novershoot" ~ "overshoot")) |> 
  group_by(class) |> 
  summarise(n = n()) |> 
  mutate(total = sum(n),
         percent = n/total) |> 
  drop_na(class)

