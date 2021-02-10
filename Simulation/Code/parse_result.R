library(tidyverse)
library(reshape)
library(unglue)
library(knitr)


############################# Prediction Error Results ##############################################

sims <- list.files("/data/user/boyiguo1/MOTE/Res",full.names =TRUE)

map_dfr(sims, .f = function(sim){
  # sim <- sims[1]
  sim.df <- unglue_data(sim, 
                        "/data/user/boyiguo1/MOTE/Res/p{p}_q{q}_ntr{n.train}_nte{n.test}_{corr}_trt{trt.f}_lnk{lnk.f}")
  fls <- list.files(sim, full.names = TRUE)
  n <- length(fls)
  
  ret <- fls %>% map_dfr(readRDS) %>% select(-run) %>% 
    dplyr::summarise(MOTE.mse = mean(RF.mdl.MSE),
                     MOTE.sd = sd(RF.mdl.MSE),
                     l1.MSE = mean(susan.MSE),
                     l1.sd = sd(susan.MSE, na.rm = T),
                     RF.mse = mean(margin.RF.MSE),
                     RF.sd = sd(margin.RF.MSE))
  
  data.frame(
    sim.df, n, 
    ret
  )
  
}) %>% 
  arrange(desc(corr)) %>%
  filter(!(trt.f == "Poly" & lnk.f == "Poly")) %>%
  filter(p==10, q==3) %>% 
  data.frame(t(c((strsplit(x, split="_") %>% unlist)[5:7],dat))) %>% 
  transmute(corr = V1,
            Trt = V2,
            Link = V3,
            "MOTTE RF" = glue('{RF.mdl} ({RF.mdl.se})'),
            "L1 PLS" = glue('{susan} ({susan.se})'),
            "Marginal RF" = glue('{margin.RF} ({margin.RF.se})')
  )
# }) 
# %>%
  pivot_wider(
    names_from = corr,
    values_from = c("MOTTE RF", "L1 PLS","Marginal RF")
  ) %>%
  dplyr::select(Trt, Link, 
                c("MOTTE RF_ind",
                  "L1 PLS_ind","Marginal RF_ind",
                  "MOTTE RF_AR",
                  "L1 PLS_AR","Marginal RF_AR")
  ) %>% 
  # knitr::kable(format = format)
  kable(format = "latex")





############################# Computation Efficiency Results ##############################################
sims <- list.files("/data/user/boyiguo1/MOTE/RunTime",full.names =TRUE)

map_dfr(sims, .f = function(sim){
  # sim <- sims[1]
  sim.df <- unglue_data(sim, 
                        "/data/user/boyiguo1/MOTE/RunTime/p{p}_q{q}_ntr{n.train}_nte{n.test}_{corr}_trt{trt.f}_lnk{lnk.f}")
  fls <- list.files(sim, full.names = TRUE)
  n <- length(fls)
  
  ret <- fls %>% map_dfr(readRDS) %>% select(-run) %>% 
    select(ends_with("_fit.user.self"), ends_with("_size")) %>% 
    summarize(MOTE_mean = mean(MOTE_fit.user.self),
              l1_mean = mean(l1_fit.user.self),
              RF_mean = mean(RF_fit.user.self),
              MOTE_size_mean = mean(MOTE_size)/1024,
              l1_size_mean = mean(l1_size)/1024,
              RF_size_mean = mean(RF_size)/1024,
    )
  
  data.frame(
    sim.df, n, 
    ret
  )
  
}) %>% 
  arrange(desc(corr)) %>%
  filter(!(trt.f == "Poly" & lnk.f == "Poly")) %>%
  group_by(p, q) %>% 
  select(-c(n.train, n.test, corr, trt.f, lnk.f, n)) %>% 
  summarise_all(mean) %>% 
  kable(digits =3)
