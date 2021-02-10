library(tidyverse)
library(MOTE.RF)
library(unglue)

###############################################################
#
#   Data Prep
#
###############################################################

# Read In Data
## Please request the data from the corresponding author: Zhu, Ruoqing <rqzhu@illinois.edu>
dat <- read_csv("Data/avocado_gg.csv") %>% 
  arrange(SubjectID, BaselineEnd) %>% data.frame %>% 
  rename(SBP = "A_SystolicBP_mm_Hg", 
         DBP = "A_DiastolicBP_mm_Hg",
         TG = "B_Triglyderides_mg_dl", 
         LDL = "B_LDL_mg_dL",
  )

################# Outcomes ##################################
y_vars <- c("SBP", "DBP", "TG", "LDL")

dat_meta <- dat %>% select(!starts_with("GG_F_16S")) %>% 
  select(SubjectID, Treatment, Period, BaselineEnd, y_vars, bmi = A_BMI_kg_m2)  %>%
  mutate_at(y_vars, scale)


###################### Prepare Data at Family, Genus, Order Level #####################
#
dat_mcb <- dat %>% select(starts_with("GG_F_16S"))
Phylum_indices <- 1:17
family_indices <- 18:103
genus_indices <- 104:211

###################### Organize Genus Level OTU for Analysis ####################
# 108 OTU's at Genus level
dat_otu <- dat_mcb[, genus_indices] %>% data.frame()
all_genus <- colnames(dat_otu)

# Remove Rare OTUs
dat_otu <- dat_otu[,colMeans(dat_otu) > 1e-5]
dat_otu <- dat_otu[,colMeans(dat_otu!=0)>0.8]

genus_names <- names(dat_otu)

# Transformation
OTU_size <- rowSums(dat[, setdiff(all_genus, genus_names)])
dat_otu <- dat_otu %>% mutate_at(genus_names, ~log((.x+0.00000000001)/OTU_size))


############### Prepare Data as Pre- and Post- form ###############
dat <- data.frame(dat_meta, dat_otu) %>%
  dplyr::filter(complete.cases(.)) %>%
  group_by(SubjectID) %>%
  dplyr::filter(n()==2) %>%
  ungroup

baseline.data <- dat %>%
  dplyr::filter(BaselineEnd=="baseline")

end.data <- dat %>%
  dplyr::filter(BaselineEnd=="end")

Treatment <- end.data %>% pull(Treatment) %>% factor


# Check if the data are matched
# sum( (baseline.data[,"SubjectID"]!=end.data[,"SubjectID"])&
#        (baseline.data[,"Period"]!=end.data[,"Period"]) )


###############################################################
#
#   MOTEF Model Fitting
#
###############################################################
set.seed(2)

mdl <-  MOTE(
  x.b = data.matrix(baseline.data[,genus_names]),
  x.e = data.matrix(end.data[,genus_names]),
  treat = as.vector(Treatment) %>% factor(levels = c("Cont", "Avocado")),
  y.b = data.matrix(baseline.data[,y_vars]),
  y.e = data.matrix(end.data[,y_vars]),
  Z = data.matrix(baseline.data[,c("bmi")]),
  min.node.size = 10,
  num.random.splits = 10,
  replace= TRUE,
  seed = 1,num.threads = 1,
  num.trees = 200)


# Out-of-bag Predictions = Avocado - Cont
res_blood <- cbind(
  mdl$predictions %>%
    data.frame %>%
    mutate(id = 1:n()),
  end.data %>% select(c("SubjectID", "Treatment"))
)

colnames(res_blood)[1:length(y_vars)] <- y_vars


############## Plot Prediction Result ##############
# Clustering Individuals Based on 
set.seed(2)
cluster_info <- kmeans(res_blood %>% select(-c(id, SubjectID, Treatment)), centers=3)

# Clustering Individuals Based on 
plot_dat <- pivot_longer(cbind(res_blood, Cluster = cluster_info$cluster %>% factor), cols = SBP:LDL, names_to ="vars") %>%
  mutate(#vars = stringr::str_replace_all(vars, "diff.", ""),
         vars = factor(vars, levels = c("DBP", "SBP", "LDL", "TG")
                       )
  )

ggplot(plot_dat , 
       aes(x=vars, y = value, group = SubjectID, color = Cluster
       )) +
  geom_point() +
  geom_line(alpha = 0.2, size=1) +
  ylab("Predicted Treatment Effect") +
  xlab("") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = -80))

ggsave("Fig4_AvocadoTreatmentEffect_V3.pdf", device="pdf", width = 6, height = 4)




############## Relative Feature Importance ##############
FI_blood <- importance(mdl)

FI_plot_dat <- data.frame(
  vars = names(FI_blood),
  value = FI_blood) %>% 
  transmute(vars %>%
              unglue_data("GG_F_16S_k__{Kingdom}.p__{Phylum}.c__{Class}.o__{Order}.f__{Family}.g__{Genus}"),
            value) %>% 
  dplyr::filter(!is.na(Genus)) %>%
  mutate(Genus = ifelse(Genus=="", "unclassified", Genus)) %>% 
  arrange(desc(value)) %>% 
  select(-Kingdom, -Phylum)

FI_plot_dat %>% head(5) %>% 
  knitr::kable(format="latex", digits=2)

