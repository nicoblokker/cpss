from sentence_transformers import SentenceTransformer, util
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import torch
from pathlib import Path
import argparse
import glob
import pickle
from collections import defaultdict

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

def plot_cosine_matrix(matrix, thesis, title_, yaxis, xaxis, output_dir):

    fig, ax = plt.subplots()

    matrix = np.tril(matrix).astype('float')
    matrix[matrix == 0] = 'nan'  # or use np.nan

    ax.matshow(matrix, cmap=plt.cm.Blues)

    for i in range(matrix.shape[1]):
        for j in range(matrix.shape[0]):
            c = matrix[j, i]
            if str(c)!="nan":
                ax.text(i, j, str(round(c, 2)), va='center', ha='center')  #parties[i], parties[j], str(float(c)),

    plt.title(title_)
    ax.xaxis.set_ticks_position('bottom')
    ax.set_xticklabels([i for i in xaxis])
    ax.set_yticklabels([i for i in yaxis])

    plt.show()
    Path(f"{output_dir}/plots").mkdir(exist_ok=True, parents=True)
    plt.savefig(f"{output_dir}/plots/{thesis}.jpeg")
    plt.close()

def save_cosine_csv(df_matrix, name_f, output_dir):
    Path(f"{output_dir}/cosine_scores").mkdir(exist_ok=True, parents=True)
    df_matrix.to_csv(f"{output_dir}/cosine_scores/"+str(name_f)+".csv")

def save_cosine_between_sentences(cosine_scores, sentences, parties, name_f, output_dir):
    # Find the pairs with the highest cosine similarity scores
    Path(f"{output_dir}/cos_sentences").mkdir(exist_ok=True, parents=True)
    f_out = open(f"{output_dir}/cos_sentences/{name_f}.txt", "w")
    pairs = []
    for i in range(len(cosine_scores) - 1):
        for j in range(i + 1, len(cosine_scores)):
            pairs.append({'index': [i, j], 'score': cosine_scores[i][j]})

    # Sort scores in decreasing order
    pairs = sorted(pairs, key=lambda x: x['score'], reverse=True)
    for pair in pairs:
        i, j = pair['index']
        f_out.write(f"Sent1 of party {parties[i].upper()}= {sentences[i]}\nSent2 of party {parties[j].upper()}= {sentences[j]}\nScore= {pair['score']}\n\n")
    f_out.close()

def per_justification(df, save_cosine=True, save_sentences=True):
    Path(f"{output_dir}/pickle_files").mkdir(exist_ok=True, parents=True)
    theses = set(df[cat_num].tolist())

    for thesis in theses:
        tmp=df[df[cat_num]==thesis].drop_duplicates(subset=[args.column])

        new_df = pd.DataFrame()
        parties = set(tmp["actor_id"])
        for party in parties:
            tmp2 = tmp[tmp["actor_id"] == party]
            if len(tmp2) > 1:
                sents = " ".join(tmp2[args.column].tolist())
                tmp2[args.column] = [sents] * len(tmp2)
                new_df = pd.concat([new_df, tmp2[:1]])
            else:
                new_df = pd.concat([new_df, tmp2])

        new_df = new_df.to_dict("list")
        sentences = new_df[args.column]
        parties = new_df["actor_id"]

        #Compute embedding for both lists
        embeddings = model.encode(sentences, convert_to_tensor=True)

        #Compute cosine-similarits
        cosine_scores = util.cos_sim(embeddings, embeddings).cpu().detach().numpy()
        plot_cosine_matrix(cosine_scores, new_df[cat_num][0], new_df[category][0],
                       ["1"] + parties, parties, output_dir)  #
        pickle.dump(sentences, open(f"{output_dir}/pickle_files/{new_df[cat_num][0]}.sents", "wb"))
        pickle.dump(cosine_scores, open(f"{output_dir}/pickle_files/{new_df[cat_num][0]}.cos", "wb"))
        pickle.dump(parties, open(f"{output_dir}/pickle_files/{new_df[cat_num][0]}.parties", "wb"))

        if save_sentences:
            save_cosine_between_sentences(cosine_scores, sentences, parties, new_df[cat_num][0], output_dir)

        if save_cosine:
            df_save = pd.DataFrame(cosine_scores)
            df_save.columns, df_save.index = parties, parties
            save_cosine_csv(df_save, new_df[cat_num][0], output_dir)


def avg_justifications_aggreg(df, save_cosine=True):
    avg_scores = defaultdict(list)
    theses = set(df[cat_num].tolist())

    for thesis in theses:
        tmp=df[df[cat_num]==thesis].drop_duplicates(subset=[args.column])

        new_df = pd.DataFrame()
        parties = set(tmp["actor_id"])
        for party in parties:
            tmp2 = tmp[tmp["actor_id"] == party]
            if len(tmp2) > 1:
                sents = " ".join(tmp2[args.column].tolist())
                tmp2[args.column] = [sents] * len(tmp2)
                new_df = pd.concat([new_df, tmp2[:1]])
            else:
                new_df = pd.concat([new_df, tmp2])

        new_df = new_df.to_dict("list")
        sentences = new_df[args.column]
        parties = new_df["actor_id"]
        #Compute embedding for both lists
        embeddings = model.encode(sentences, convert_to_tensor=True)

        # Compute cosine-similarits
        cosine_scores = util.cos_sim(embeddings, embeddings).cpu().detach().numpy()
        for i in range(len(cosine_scores) ):
            for j in range(len(cosine_scores)):
                avg_scores[(parties[i], parties[j])].append(cosine_scores[i][j])

    for k in avg_scores.keys():
        avg_scores[k] = np.mean(avg_scores[k])
    df_matrix = pd.Series(avg_scores).unstack().T
    matrix = np.array(df_matrix)
    yaxis = df_matrix.index.tolist()
    xaxis = df_matrix.columns.tolist()
    plot_cosine_matrix(matrix, name_f_plot, title_plot, [1] + yaxis, [1] + xaxis,  output_dir)
    if save_cosine:
        save_cosine_csv(df_matrix, name_f_plot, output_dir)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--column", type=str, help="frame_quote or claim_quote")
    parser.add_argument("--group_by", type=str, help="claim or frame")
    args = parser.parse_args()

    if "claim" in args.column:
        name_f_plot="avg_claims"
        title_plot="Average over all claims"
    else:
        name_f_plot = "avg_frames"
        title_plot = "Average over all frames"

    if args.group_by =="claim":
        cat_num = "claim_cat_num"
        category = "claim_cat"
    else:
        cat_num="frame_cat_num"
        category="frame_cat"

    df = pd.read_csv("MaCov21.csv", index_col=0)

    df_frame = df.to_dict("records")
    frame_mapping = {}
    for row in df_frame:
        frame_mapping[str(row[cat_num])] = row[category]

    name_model = "paraphrase-multilingual-mpnet-base-v2"
    model = SentenceTransformer(name_model)  # ("all-mpnet-base-v2")   #('./outputs/sem_sim_finetuning')

    output_dir = f"./results/{name_model}/maco_{args.column}_{args.group_by}"

    per_justification(df)
    avg_justifications_aggreg(df)

