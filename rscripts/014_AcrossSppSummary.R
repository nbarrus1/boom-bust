

library(here)
library(tidyverse)
library(patchwork)


variable.names(regimeclassification)


regimeclassification |> 
  mutate(class = case_when(class == "boom &\nbust"|
                             class == "boom &\n sust. unk"|
                             class == "unk rate &\nbust"~ "rapid collapse",
                           .default = "no rapid collapse"),
         major.group = case_when(major.group == "Bivalvia"|major.group == "Gastropoda"~"Mollusca",
                                 major.group == "Magnoliopsida"~"Traecheophyta",
                                 .default = major.group)) |> 
  dplyr::select(plot,group,species.names,major.group,class) |> 
  group_by(major.group,species.names,class) |> 
  summarise(n = n()) |> 
  group_by(major.group,class) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = class,values_from = n) |> 
  mutate(`no rapid collapse` = if_else(is.na(`no rapid collapse`),true = 0,
                                       false = `no rapid collapse`),
         `rapid collapse` = if_else(is.na(`rapid collapse`),true = 0,
                                       false = `rapid collapse`),
          total = `no rapid collapse`+`rapid collapse`,
         prop = `rapid collapse`/total,
         lci = prop-(1.96*(sqrt(prop*(1-prop)/total))),
         uci = prop+(1.96*(sqrt(prop*(1-prop)/total))),
         lci = if_else(lci <0, true = 0,false = lci),
         uci = if_else(uci>1, true = 1, false = uci),
         lci = case_when(lci == 0 & uci == 1 ~NA_real_,
                         lci == 1 & uci == 1 ~NA_real_,
                         lci == 0 & uci == 0 ~ NA_real_,
                         lci == 0 ~NA_real_,
                         .default = lci),
         uci = case_when(is.na(lci) & uci == 1 ~NA_real_,
                         is.na(lci) & uci == 1 ~NA_real_,
                         is.na(lci) & uci == 0 ~ NA_real_,
                         is.na(lci)~NA_real_,
                         .default = uci))

regimeclassification |> 
  mutate(class = case_when(class == "boom &\nbust"|
                             class == "boom &\n sust. unk"|
                             class == "unk rate &\nbust"~ "rapid collapse",
                           .default = "no rapid collapse"),
         major.group = case_when(major.group == "Bivalvia"|major.group == "Gastropoda"~"Mollusca",
                                 major.group == "Magnoliopsida"~"Traecheophyta",
                                 .default = major.group)) |> 
  dplyr::select(plot,group,species.names,major.group,class) |> 
  group_by(major.group,species.names,class) |> 
  summarise(n = n()) |> 
  group_by(class) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = class,values_from = n) |> 
  mutate(`no rapid collapse` = if_else(is.na(`no rapid collapse`),true = 0,
                                       false = `no rapid collapse`),
         `rapid collapse` = if_else(is.na(`rapid collapse`),true = 0,
                                    false = `rapid collapse`),
         total = `no rapid collapse`+`rapid collapse`,
         prop = `rapid collapse`/total,
         lci = prop-(1.96*(sqrt(prop*(1-prop)/total))),
         uci = prop+(1.96*(sqrt(prop*(1-prop)/total))),
         lci = if_else(lci <0, true = 0,false = lci),
         uci = if_else(uci>1, true = 1, false = uci),
         lci = case_when(lci == 0 & uci == 1 ~NA_real_,
                         lci == 1 & uci == 1 ~NA_real_,
                         lci == 0 & uci == 0 ~ NA_real_,
                         .default = lci),
         uci = case_when(is.na(lci) & uci == 1 ~NA_real_,
                         is.na(lci) & uci == 1 ~NA_real_,
                         is.na(lci) & uci == 0 ~ NA_real_,
                         .default = uci),
         sensitivity = if_else(is.na(lci), true = NA_real_,
                               false = map2_dbl(.x = `rapid collapse`,.y = total,.f = sensitivity.fxn)))  

###sensitivity function









sensitivity.fxn <- function(k,n) {

if (is.na(k)) {
  NA_integer_
  
}else{
  
  
# original CI

#new upper CI below original lower CI
x_upper <- 0
ci <- binom.test(k, n)$conf.int
U.new <- ci[2]
p0 <- if_else(k == 0, true = 0.01, false = k/n)


while (U.new >= p0) {
  ci <- binom.test(k, n + x_upper)$conf.int
  U.new <- ci[2]
  x_upper <- x_upper + 1
}

x_upper-1

}}


sensitivity.fxn(k = 4, n = 11)
