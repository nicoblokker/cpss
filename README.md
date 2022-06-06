## Cosine similarity

In order to run the cosine similarity among frames grouped by claim categories in WoM21, run:
	python3 cosine_sim_sbert_wom.py 


If you want to run the cosine similarity among claims grouped by claims in MaCov, run:
	python3 cosine_sim_sbert_maco.py --column claim_quote --group_by claim

To run cosine similarity among frames grouped by claims in MaCov:
	python3 cosine_sim_sbert_maco.py --column frame_quote --group_by claim 

Lastly, for running cosine similarity among frames grouped by frames, execute:
	python3 cosine_sim_sbert_maco.py --column frame_quote --group_by frame
