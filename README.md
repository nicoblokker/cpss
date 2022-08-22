Replication files
================

## Contents

This repository contains the replication files of the following
publication:

> Why Justifications of Claims Matter for Understanding Party Positions.
> In: Proceedings of the 2nd Workshop on Computational Linguistics for
> Political Text Analysis. 2022. Nico Blokker, Tanise Ceron, André
> Blessing, Erenay Dayanik, Sebastian Haunss, Jonas Kuhn, Gabriella
> Lapesa and Sebastian Padó.

In detail it contains the main data set (`MaCov21.csv`), the script used
for analysis (`Analysis.R`), as well as the calculations for cosine
similarities based on SBERT (see below).

To interact and visualize the data visit:
<https://mardy.shinyapps.io/MaCov21_dashboard/>

## Cosine similarity

In order to run the cosine similarity among frames grouped by claim
categories in WoM21 (`WoM_avg_justifications_v2.csv`), run:

    python3 cosine_sim_sbert_wom.py 

If you want to run the cosine similarity among claims grouped by claims
in MaCov (`MaCov_avg_claims_v2.csv`), run:

    python3 cosine_sim_sbert_maco.py --column claim_quote --group_by claim

To run cosine similarity among frames grouped by claims in MaCov
(`MaCov_avg_frames_grouped_by_claims.csv`):

    python3 cosine_sim_sbert_maco.py --column frame_quote --group_by claim 

Lastly, for running cosine similarity among frames grouped by frames
(`MaCov_avg_frames_grouped_by_frames.csv`), execute:

    python3 cosine_sim_sbert_maco.py --column frame_quote --group_by frame
