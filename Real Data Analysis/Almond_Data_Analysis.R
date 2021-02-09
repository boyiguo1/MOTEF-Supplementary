library(tidyverse)
library(MOTE.RF)
library(unglue)
###############################################################
#
#   Data Prep
#
###############################################################

# Factor variables
var_names <- c("Subject", "Period", "Almond", "WholeAlmond", "RoastedAlmond", "SEX=M",
               "BMI_BIN", "CRP_BIN", "SAA_BIN", "CHOL_BIN", "HDL_BIN", "LDL_BIN", "TRIG_BIN", "Glu_BIN",
               "Methanosphaera_BIN", "Bifidobacterium_BIN", "Collinsella_BIN", "Parabacteroides_BIN",
               "Clostridium_BIN", "Lachnospira_BIN", "Roseburia_BIN", "Oscillospira_BIN", "Akkermansia_BIN")


# Read In CSV data
## Please request the data from the corresponding author: Zhu, Ruoqing <rqzhu@illinois.edu>
clean_dat <- read_csv("Data/almond-treatment5.csv") %>%
  # Remove irrelevant variables: SampleID,Reextraction,TrtTime,TrtTime2
  select(-c(SampleID,Reextraction,TrtTime,TrtTime2)) %>%
  mutate_at(var_names, factor) %>%   # Factor some variables
  dplyr::filter(!(Subject=="4517" & Period %in% c(2,4))) %>%  # Remove invalid Entries
  # Delete the entries that don't have corresponding baseline or end
  arrange(Subject, Period)



###################### Prepare Data at Family, Genus, Order Level #####################
#
family.index <- 70:110
genus_indices <- 111:198

# Congregate Family Level ot Order Level
order_dat <- clean_dat[,family.index] %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  rowwise() %>%
  mutate( rowname = (strsplit(rowname, split=";f__", fixed=T) %>% unlist)[1]) %>%
  group_by(rowname) %>%
  summarize(across(where(is.numeric), sum)) %>%
  ungroup() %>%
  arrange(rowname) %>%
  column_to_rownames() %>% t() %>%
  data.frame

order.data.name <- colnames(order_dat)



###################### Organize Genus Level OTU for Analysis ####################
dat_otu <- clean_dat[, genus_indices] %>% data.frame() %>% select_if(is.numeric)
all_genus <- colnames(dat_otu)

# Remove Rare OTUs
dat_otu <- dat_otu[,colMeans(dat_otu) > 1e-5]
dat_otu <- dat_otu[,colMeans(dat_otu!=0) > 0.8]

genus_names <- names(dat_otu)






######################## Organize Outcomes ###########################

#Grep Response Variables
y <- clean_dat %>%
  select(c("BMI_kg_m2","IL_6_pg_mL","CRP_ng_mL",
           "SAA_ng_mL","SICAM_1_ng_mL", "SVCAM_1_ng_mL",
           "CHOL_mg_dL","dLDL_mg_dL","TRIG_mg_dL","Glu_mg_dL")) %>%
  apply(2, scale) %>%
  data.frame %>%
  dplyr::rename( "BMI" = "BMI_kg_m2", "IL6" = "IL_6_pg_mL", "CRP" = "CRP_ng_mL",
                 "SAA" = "SAA_ng_mL", "SICAM1" = "SICAM_1_ng_mL",  "SVCAM1" = "SVCAM_1_ng_mL",
                 "CHOL" = "CHOL_mg_dL", "dLDL" = "dLDL_mg_dL", "TRIG" = "TRIG_mg_dL", "Glu" = "Glu_mg_dL")


y.vars <- c("IL6","CRP", "SAA","SICAM1", "SVCAM1")

meta <- clean_dat %>%
  select("Subject","Period","BaselineEnd","Treatment","WholeAlmond"
         , male = "SEX=M", age = "Age_yr", bmi = BMI_kg_m2
  ) %>%
  mutate(Trt2 = forcats::fct_collapse(Treatment,
                                      control = c("Butter", "control"),
                                      treat = c("Chopped", "WholeRoasted")
  )
  )

######################## Screening Genus Level OTUs ###########################
dat <- data.frame(meta, dat_otu) %>%
  dplyr::filter(complete.cases(.)) %>%
  group_by(Subject, Period) %>%
  dplyr::filter(n()==2) %>%
  ungroup

dat_diff <- dat %>%
  group_by(Subject, Period) %>%
  mutate_at(genus_names, function(x) {lead(x, n=1)}-x) %>%
  dplyr::filter(BaselineEnd!="End") %>%
  ungroup %>% select(genus_names, Trt2) %>%
  dplyr::filter(Trt2 %in% c("control", "treat"))

Treatment <- dat_diff$Trt2

OTU_names <- dat_diff %>%
  select(genus_names) %>%
  map_dbl(~wilcox.test(.~dat_diff$Trt2)$p.value) %>%
  sort() %>% head(15) %>% names()

# Transformation
OTU_size <- rowSums(clean_dat %>% data.frame %>% select(setdiff(all_genus, genus_names)))
dat_otu <- dat_otu %>% mutate_at(genus_names, ~log((.x+0.00000000001)/OTU_size))


############### Prepare Data as Pre- and Post- form ###############
dat <- data.frame(meta, dat_otu[,OTU_names], y) %>%
  dplyr::filter(Trt2!="wholeRaw") %>%
  mutate(Trt2 = forcats::fct_drop(Trt2))

baseline.data <- dat %>%
  dplyr::filter(BaselineEnd=="Baseline") %>%
  data.frame

end.data <- dat %>%
  dplyr::filter(BaselineEnd=="End") %>%
  data.frame

Treatment <- end.data %>% pull(Trt2)

# Check if the data are matched
# sum( (baseline.data[,"Subject"]!=end.data[,"Subject"])&
#        (baseline.data[,"Period"]!=end.data[,"Period"]) )

###############################################################
#
#   MOTEF Model Fitting
#
###############################################################

set.seed(2)

mdl_infl <- MOTE(x.b = data.matrix(baseline.data[,OTU_names]),
                 x.e = data.matrix(end.data[,OTU_names]),
                 treat = as.vector(Treatment) %>% factor(levels = c("control", "treat")),
                 y.b = data.matrix(baseline.data[,y.vars]),
                 y.e = data.matrix(end.data[,y.vars]),
                 Z = data.matrix(baseline.data[,c("male", "age", "bmi")]),
                 min.node.size = 10,
                 num.random.splits = 10,
                 replace= TRUE,
                 seed = 1,num.threads = 1,
                 num.trees = 200)


# Out-of-bag Predictions
res_infl <- cbind(
  mdl_infl$predictions %>% 
    data.frame %>%
    mutate(id = 1:n()),
  end.data %>%    # Matching Individual information
    select(c("Subject","Trt2", "Treatment"))
) 

# Set up column names
colnames(res_infl)[1:length(y.vars)] <- y.vars

############## Plot Prediction Result ##############
# Clustering Individuals Based on 
set.seed(3)
cluster_info <- kmeans(res_infl %>% 
                         select(-c(id, Subject, Trt2, Treatment)),
                       centers=3, nstart = 10)

plot_dat <- pivot_longer(
  cbind(res_infl, Cluster = cluster_info$cluster %>% factor), 
  cols = IL6:SVCAM1, names_to ="vars") %>%
  mutate(vars = factor(vars))

ggplot(plot_dat, 
       aes(x=vars,
           y = value*(-1), # Invert the result as almond group was reference group
           group =id, color = Cluster
       )
) +
  geom_point() +
  geom_line(alpha = 0.2, size=1 ) +
  ylab("Predicted Treatment Effect") +
  xlab("") +
  theme_classic() +
  scale_y_continuous(breaks = c(-0.25, 0, 0.25, 0.50, 0.75))+
  theme(axis.text.x = element_text(angle = -80))

# Please Change Path
ggsave("Fig2_AlmondTreatmentEffect_V3.pdf", device="pdf", width = 6, height = 4)


############## Relative Feature Importance ##############
FI_infl <- importance(mdl_infl)
FI_plot_dat <- data.frame(
  vars = names(FI_infl),
  value = FI_infl) %>% 
  transmute(vars %>%
              unglue_data("k__{Kingdom}.p__{Phylum}.c__{Class}.o__{Order}.f__{Family}.g__{Genus}"),
            value) %>% 
  dplyr::filter(!is.na(Genus)) %>% 
  mutate(Genus = ifelse(Genus=="", "unclassified", Genus)) %>% 
  arrange(desc(value)) %>% 
  select(-Kingdom, -Phylum)

FI_plot_dat %>% head(5) %>% 
  knitr::kable(format="latex", digits=2)