# 00 preparation -----------------------------------------------------------

library(dplyr)
library(igraph)
library(cluster)
library(lsa)
library(readr)
library(vegan)
library(purrr)

# load MaCov21 data, remove duplicate entries (e.g., same party-claim-frame-polarity), and select relevant variables
macov_complete <-  readr::read_csv("MaCov21.csv", show_col_types = F) %>% select(claim_cat, frame_cat, actor_id, Polarity) 

macov <- macov_complete %>% 
          .[,c(1:4)] %>%
          distinct(.keep_all = T) %>% 
          tibble::rowid_to_column() %>%
          select(claim_cat, frame_cat, actor_id, Polarity, rowid) 

# load WoM dataset and select relevant parties and variables
# first download and extract data from here: https://www.wahl-o-mat.de/bundestagswahl2021/wahlomat.zip 
# Wahl-O-Mat Bundestag 2021_Datensatz_v1.02
wom <- readr::read_csv("WoM21.csv", show_col_types = F) 
party_names <- unique(wom$`Partei: Kurzbezeichnung`)[1:6]
wom <- wom %>%   
          select(c(2, 4:8)) %>% 
          rename("party"  = 1,
                 "thesis_number" = 2,
                 "thesis_title" = 3,
                 "thesis_text" = 4,
                 "position" = 5,
                 "opinion" = 6) %>% 
          filter(party %in% party_names) %>% 
          mutate(weight = case_when(position == "stimme nicht zu" ~ -1,
                                    position == "stimme zu" ~ 1,
                                    position == "neutral" ~ 0))

# read Party-Frame cosine distance based on S-Bert WoM (calculations not included)
wom_pf_cos <- read.csv("WoM_avg_justifications_v2.csv", 
                               row.names = 1, check.names = F, fileEncoding = "UTF-8")

# read Party-Claim cosine distance based on S-Bert MaCov (calculations not included)
macov_pf_cos_bert <- read.csv("MaCov_avg_frames_grouped_by_claims.csv",                                
                       row.names = 1, check.names = F, fileEncoding = "UTF-8")

# read Party-Claim cosine distance based on S-Bert MaCov (calculations not included)
macov_pf_cos_bert_byf <- read.csv("MaCov_avg_frames_grouped_by_frames.csv",                                
                                  row.names = 1, check.names = F, fileEncoding = "UTF-8")

# read Party-Claim cosine distance based on S-Bert Macov (calculations not included)
macov_pc_cos_bert <- read.csv("MaCov_avg_claims_v2.csv", 
                              row.names = 1, check.names = F, fileEncoding = "UTF-8")


# 01 a) MaCov calculate similarities --------------------------------------

# create bipartite Party-Claim network (P-C)
rel_pc <- macov[, c(1,3,4)] %>%                      
          mutate(Polarity = ifelse(Polarity == "+", 1, -1)) %>% 
          rename(weight = Polarity) %>%
          distinct(.keep_all = T)                                   # remove redundant rows

g_pc <- graph_from_data_frame(rel_pc, directed = F)
V(g_pc)$type <- bipartite.mapping(g_pc)$type
macov_pc <- get.incidence(g_pc, sparse = F, attr = "weight")
macov_pc_overlap <- t(macov_pc) %*% macov_pc
macov_pc_cos <- lsa::cosine(macov_pc)

# create bipartite Party-Frame network (P-F)
rel_pf <- macov[, c(2,3,4)] %>% 
          mutate(Polarity = 1) %>%                                  # set frame polarity to 1 
          rename(weight = Polarity) %>% 
          distinct(.keep_all = T)                                   # remove redundant rows

g_pf <- graph_from_data_frame(rel_pf, directed = F)
V(g_pf)$type <- bipartite.mapping(g_pf)$type
macov_pf <- get.incidence(g_pf, sparse = F, attr = "weight")
macov_pf_overlap <- t(macov_pf) %*% macov_pf
macov_pf_cos <- lsa::cosine(macov_pf)


# 01 b) WoM calculate similarities ----------------------------------------

# create bipartite Party-Claim network (P-C)
g <- graph_from_data_frame(wom[, c("party", "thesis_title", "weight")], directed = F)
V(g)$type <- bipartite.mapping(g)$type
wom_pc <- t(get.incidence(g, attr = "weight", sparse = F))
wom_pc_overlap <- t(wom_pc) %*% wom_pc
wom_pc_cos <- lsa::cosine(wom_pc)


# 02 calculate significance -----------------------------------------------
set.seed(1)
# Macov correlation between cosine matrices P-C and P-F
identical(rownames(macov_pc_cos), rownames(macov_pf_cos))
vegan::mantel(as.dist(1-macov_pc_cos), as.dist(1-macov_pf_cos),                 # r = 0.68, p = 0.03, r2 = 0.46
              method = "pearson", permutations = 1000)
r2 <- 0.68^2

# WoM correlation between cosine matrices P-C and P-F
identical(rownames(wom_pc_cos), rownames(wom_pf_cos))
vegan::mantel(as.dist(1-wom_pc_cos), as.dist(1-wom_pf_cos),                     # r = 0.53, p = 0.001
              method = "pearson", permutations = 1000)

# Macov correlation between cosine matrices P-C  bert and P-C label
macov_pc_cos_bert2 <- macov_pc_cos_bert[c(1,2,3,4,6,5), c(1,2,3,4,6,5)]
identical(rownames(macov_pc_cos), rownames(macov_pc_cos_bert2))
vegan::mantel(as.dist(1-macov_pc_cos_bert2), as.dist(1-macov_pc_cos),           # r = 0.73, p = 0.001     
              method = "pearson", permutations = 1000)

# Macov correlation between cosine matrices PF Bert and PF label GROUPED BY CLAIMS
macov_pf_cos_bert2 <- macov_pf_cos_bert[c(1,2,3,4,6,5), c(1,2,3,4,6,5)] 
identical(rownames(macov_pf_cos), rownames(macov_pf_cos_bert2))
vegan::mantel(as.dist(1-macov_pf_cos_bert2), as.dist(1-macov_pf_cos),           #r = 0.41, p = 0.07     
              method = "pearson", permutations = 1000)

# Macov correlation between cosine matrices PF Bert and PF label GROUPED BY FRAMES
macov_pf_cos_bert_byf[is.na(macov_pf_cos_bert_byf)] <- 0                        # set NAs to 0: they do not address the same arguments, therefore maximum difference
macov_pf_cos_bert2_byf <- macov_pf_cos_bert_byf[c(1,2,3,4,6,5), c(1,2,3,4,6,5)] 
identical(rownames(macov_pf_cos), rownames(macov_pf_cos_bert2_byf))
vegan::mantel(as.dist(1-macov_pf_cos_bert2_byf), as.dist(1-macov_pf_cos),       # r = 0.52, p = 0.04     
              method = "pearson", permutations = 1000)

# Macov correlation between cosine matrices P-C bert and P-C label
identical(rownames(macov_pc_cos), rownames(macov_pf_cos_bert2))
vegan::mantel(as.dist(1-macov_pc_cos), as.dist(1-macov_pf_cos_bert2),           # r = 0.4, p = 0.09       
              method = "pearson", permutations = 1000)


# Mavoc correlation between P-C label and P-F (grouped by frames)
identical(rownames(macov_pc_cos), rownames(macov_pf_cos_bert2_byf))
vegan::mantel(as.dist(1-macov_pc_cos), as.dist(1-macov_pf_cos_bert2_byf),       # r = 0.32, p = 0.16       
              method = "pearson", permutations = 1000)


# Macvoc correlation between the P-C text and P-F text (grouped by claims)
identical(rownames(macov_pc_cos_bert2), rownames(macov_pf_cos_bert2_byf))
vegan::mantel(as.dist(1-macov_pc_cos_bert2), as.dist(1-macov_pf_cos_bert2_byf), # r = 0.49, p = 0.09       
              method = "pearson", permutations = 1000)



# 03 a) MaCov: clustering based on similarities ------------------------------
set.seed(1)

# Party-Claim Cluster
macov_hclu_pc <- hclust(as.dist(1-macov_pc_cos), method = "ward.D2")     # transform similarity matrix into distance
coef.hclust(macov_hclu_pc)                                               # agglomerative coefficient of ~0.48
cutree(macov_hclu_pc, 2)                                                 # afd 1, cdu 1, fdp 1, gruene 2, spd 2, linke 2
kmeans(t(macov_pc), 2)                                                   # confirms results

# Party-Frame Cluster
macov_hclu_pf <- hclust(as.dist(1-macov_pf_cos), method = "ward.D2")     # transform similarity matrix into distance
coef.hclust(macov_hclu_pf)                                               # agglomerative coefficient of ~0.5
cutree(macov_hclu_pf, 2)                                                 # afd 1, cdu 2, fdp 2, gruene 2, spd 2, linke 1
kmeans(t(macov_pf), 2)                                                   # confirms results

# Party-Frame Cluster Bert
macov_hclu_pf_bert <- hclust(as.dist(1-macov_pf_cos_bert), method = "ward.D2")# transform similarity matrix into distance
coef.hclust(macov_hclu_pf_bert)                                               # agglomerative coefficient of ~0.41
cutree(macov_hclu_pf_bert, 2)                                                 # We don' see a very similar picture emerge as we saw in the MaCov data


# PArty-Claim Cluster Bert
macov_hclu_pc_bert <- hclust(as.dist(1-macov_pc_cos_bert), method = "ward.D2") # transform similarity matrix into distance
coef.hclust(macov_hclu_pc_bert)                                                # agglomerative coefficient of ~0.52
cutree(macov_hclu_pc_bert, 2)                                                  # We don't see a very similar picture emerge as we saw in the MaCov data


# 03 b) WoM: clustering based on similarities ------------------------------

# Party-Claim Cluster WoM
wom_hclu_pc <- hclust(as.dist(1-wom_pc_cos), method = "ward.D2")       # transform similarity matrix into distance
coef.hclust(wom_hclu_pc)                                               # agglomerative coefficient of ~0.79
cutree(wom_hclu_pc, 2)                                                 # We see a similar picture emerge as we saw in the MaCov data, with within clusters differences
kmeans(t(wom_pc), 2)                                                   # confirms results

# Party-Frame Cluster WoM
wom_hclu_pf <- hclust(as.dist(1-wom_pf_cos), method = "ward.D2")       # transform similarity matrix into distance
coef.hclust(wom_hclu_pf)                                               # agglomerative coefficient of ~0.21
cutree(wom_hclu_pf, 3)                                                 # We see a similar picture emerge as we saw in the MaCov data, with within clusters differences and 3 cluster solution


# 04 a) MaCov scale to one dimension ----------------------------------------
macov_mds_c <- cmdscale(1-macov_pc_cos, 1)
macov_mds_f <- cmdscale(1-macov_pf_cos, 1)

macov_mds_df <- data.frame("mds_c" = macov_mds_c, "mds_f" = macov_mds_f,
                     label = rownames(macov_mds_c), group = "MaCov21",
                     coalition = c(0,1,1,1,1,0))

# 04 b) WoM scale to one dimension ------------------------------------------
wom_mds_c <- cmdscale(1-wom_pc_cos, 1)
wom_mds_f <- cmdscale(1-wom_pf_cos, 1)*-1                              # flip scale to match left side

wom_mds_c_df <- data.frame(mds_c = wom_mds_c, mds_f = wom_mds_f, group = "WoM21",
                         label = paste0(rownames(wom_mds_c), "_w"))

# combine scaled distances into dataframe
df_mds <- full_join(macov_mds_df, wom_mds_c_df) %>% mutate(label = tolower(label))
df_mds$label <- c("AfD", "CDU", "FDP", "Greens", "SPD", "Left",
                   "CDU", "SPD", "AfD", "FDP", "Left", "Greens")
