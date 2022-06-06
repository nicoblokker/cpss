from sentence_transformers import SentenceTransformer, util
from transformers import AutoTokenizer
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import matplotlib.ticker as mticker
import torch
from pathlib import Path
import argparse

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

def plot_cosine_matrix(matrix, thesis, title_, parties, name_model):

    fig, ax = plt.subplots()
    matrix= np.tril(matrix)
    matrix = matrix.astype('float')
    matrix[matrix == 0] = 'nan'  # or use np.nan

    ax.matshow(matrix, cmap=plt.cm.Blues)

    for i in range(matrix.shape[1]):
        for j in range(matrix.shape[0]):
            c = matrix[j, i]
            if str(c)!="nan":
                ax.text(i, j, str(round(c, 2)), va='center', ha='center')  #parties[i], parties[j], str(float(c)),

    plt.title(title_)
    ax.xaxis.set_ticks_position('bottom')
    ax.set_xticklabels([i for i in parties])
    ax.set_yticklabels([i for i in parties])

    plt.show()
    Path(f"./results/{name_model}/wahl-o-mat/plots").mkdir(exist_ok=True, parents=True)
    plt.savefig(f"./results/{name_model}/wahl-o-mat/plots/{thesis}.jpeg")
    plt.close()

def plot_per_justification(df, save=True, save_sentences=True):
    theses = set(df["These: These"].tolist())
    for thesis in theses:
        tmp=df[df['These: These']==thesis].to_dict("list")
        sentences = tmp["Position: Begründung"]
        #Compute embedding for both lists
        embeddings = model.encode(sentences, convert_to_tensor=True)

        #Compute cosine-similarits
        cosine_scores = util.cos_sim(embeddings, embeddings).cpu().detach().numpy()
        plot_cosine_matrix(cosine_scores, tmp['These: Nr.'][0], tmp['These: Titel'][0],
                           ["1"] + tmp["Partei: Kurzbezeichnung"], name_model)  #
        if save_sentences:
            save_cosine_between_sentences(cosine_scores, sentences, tmp["Partei: Kurzbezeichnung"],  tmp['These: Nr.'][0], name_model)

        if save:
            save_cosine_csv(cosine_scores, tmp["Partei: Kurzbezeichnung"], tmp['These: Nr.'][0], name_model)

def plot_avg_justifications(df, save=True):
    matrices= []
    theses = set(df["These: These"].tolist())
    for thesis in theses:
        tmp = df[df['These: These'] == thesis].to_dict("list")
        sentences = tmp["Position: Begründung"]
        thesis_n = tmp['These: Nr.'][0]
        # Compute embedding for both lists
        embeddings = model.encode(sentences, convert_to_tensor=True)

        # Compute cosine-similarits
        cosine_scores = util.cos_sim(embeddings, embeddings).cpu().detach().numpy()
        matrices.append(cosine_scores)

    avg_cosine_scores=np.mean(matrices, axis=0)
    print(avg_cosine_scores.shape)
    plot_cosine_matrix(avg_cosine_scores, "avg_frames", "Average of all frames", ["1"]+tmp["Partei: Kurzbezeichnung"], name_model)  #tmp['These: Nr.'][0], tmp['These: Titel'][0]
    if save:
        save_cosine_csv(avg_cosine_scores, tmp["Partei: Kurzbezeichnung"], "avg_justifications",  name_model)

def save_cosine_csv(cosine_scores, parties, name_f, name_model):
    df_save = pd.DataFrame(cosine_scores)
    df_save.columns, df_save.index=parties, parties
    Path(f"./results/{name_model}/wahl-o-mat/cosine_scores").mkdir(exist_ok=True, parents=True)
    df_save.to_csv(f"./results/{name_model}/wahl-o-mat/cosine_scores/"+str(name_f)+".csv")

def save_cosine_between_sentences(cosine_scores, sentences, parties, name_f, name_model):
    # Find the pairs with the highest cosine similarity scores
    Path(f"./results/{name_model}/wahl-o-mat/cos_sentences").mkdir(exist_ok=True, parents=True)
    f_out = open(f"./results/{name_model}/wahl-o-mat/cos_sentences/{name_f}.txt", "w")
    pairs = []
    for i in range(len(cosine_scores) - 1):
        for j in range(i + 1, len(cosine_scores)):
            pairs.append({'index': [i, j], 'score': cosine_scores[i][j]})

    # Sort scores in decreasing order
    pairs = sorted(pairs, key=lambda x: x['score'], reverse=True)

    for pair in pairs:
        i, j = pair['index']
        f_out.write(f"Sent1 of party {parties[i]}= {sentences[i]}\nSent2 of party {parties[j]}= {sentences[j]}\nScore= {pair['score']}\n\n")
    f_out.close()


if __name__ == '__main__':

    df = pd.read_csv("Wahl-O-Mat2021.csv", index_col=0)

    print(df.columns)
    df = df[df["Partei: Nr."].isin([1, 2, 3, 4, 5, 6])]

    name_model = "paraphrase-multilingual-mpnet-base-v2" #"distiluse-base-multilingual-cased-v1"
    model = SentenceTransformer(name_model) #("all-mpnet-base-v2")   #('./outputs/sem_sim_finetuning')

    plot_per_justification(df)
    plot_avg_justifications(df)



