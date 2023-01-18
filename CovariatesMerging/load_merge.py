import os
import sys
import numpy as np
import pandas as pd
from tqdm import tqdm

KEPT_VARS_MAP = {#"DEMO_H": ["SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDRETH3", "RIDEXMON", "DMDBORN4", "DMDYRSUS", "DMDEDUC2", "DMDMARTL", "RIDEXPRG", "WTINT2YR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2"],
                "merged_adults": ["SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDRETH3", "RIDEXMON", "DMDBORN4", "DMDYRSUS", "DMDEDUC2", "DMDMARTL", "RIDEXPRG", "WTINT2YR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2", "RA", "L5 midpoint (dec hours)", "M10 midpoint (dec hours)", "L5 (counts)", "M10 (counts)", "IS1", "IS2", "IS3", "IS4", "IS5", "IS6", "IS8", "IS9", "IS10", "IS12", "IS15", "IS16", "IS18", "IS20", "IS24", "IS30", "IS32", "IS36", "IS40", "IS45", "IS48", "IS60", "IV1", "IV2", "IV3", "IV4", "IV5", "IV6", "IV8", "IV9", "IV10", "IV12", "IV15", "IV16", "IV18", "IV20", "IV24", "IV30", "IV32", "IV36", "IV40", "IV45", "IV48", "IV60", "Valid Days", "TST (min)", "TIB (min)", "SE (%)"],
                "BPX_H": ["SEQN", "BPXSY1", "BPXDI1", "BPXSY2", "BPXDI2", "BPXSY3", "BPXDI3"],
                "BMX_H": ["SEQN", "BMXWT", "BMXHT", "BMXBMI", "BMXWAIST"],
                "GLU_H": ["SEQN", "WTSAF2YR", "LBXGLU", "LBDGLUSI"],
                #"CDQ_H": ["SEQN", ""], # Missing? Page 74
                "ALQ_H": ["SEQN", "ALQ130"],
                "BPQ_H": ["SEQN", "BPQ050A"],
                "DIQ_H": ["SEQN", "DIQ050", "DIQ070"],
                "MCQ_H": ["SEQN", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F", "MCQ220", "MCQ230A"],
                #"RXQ_RX_H": ["SEQN", ""], # Missing? Page 221
                "SLQ_H": ["SEQN", "SLD010H", "SLQ050", "SLQ060"],
                "SMQ_H": ["SEQN", "SMQ040"],
                "PFQ_H": ["SEQN", "PFQ061H", "PFQ061J", "PFQ061K", "PFQ061L"],
                "HSQ_H": ["SEQN", "HSD010"]
}

def load_xpt():
    print("Loading XPT Files...")
    fname_lst = list(KEPT_VARS_MAP.keys()) #[f.upper().split("/")[-1].replace(".XPT", "") for f in os.listdir("XPTFiles") if f.upper().endswith(".XPT")]
    for fname in tqdm(fname_lst):
        out = "CSVFiles/" + fname + ".csv"
        in_name = "XPTFiles/" + fname + ".XPT"
        if not os.path.isfile(out) and os.path.isfile(in_name):
            itr = pd.read_sas(in_name, chunksize=10000)
            idx = 0
            for chunk in itr:
                if idx == 0:
                    chunk.to_csv(out, index=False)
                else:
                    with open(out, "a") as f:
                        lines = [",".join([str(y) for y in x]) for x in chunk.values.tolist()]
                        for line in lines:
                            f.write(line + "\n")
                idx += 1

def merge():
    print("Merging...")
    df_all = None
    for key in tqdm(KEPT_VARS_MAP):
        df = pd.read_csv("CSVFiles/" + key + ".csv")
        df = df[list(set(KEPT_VARS_MAP[key] + ["SEQN"]))]
        if df_all is None:
            df_all = df
        else:
            df_all = df_all.merge(df, on="SEQN", how="left")
    return df_all

def process(df):
    print("Processing Dataframe...")
    df["education_dichotomous"] = ["No College" if x in [1, 2, 3] else "College" if x in [4, 5] else None for x in df["DMDEDUC2"]]
    df["marital_status"] = ["Married" if x in [1, 6] else "Not Married" if x in [2, 3, 4, 5] else None for x in df["DMDMARTL"]]
    df["is_pregnant"] = ["Pregnant" if x == 1 else "Not Pregnant" for x in df["RIDEXPRG"]]
    df["systolic_BP"] = (df["BPXSY1"] + df["BPXSY2"] + df["BPXSY3"] ) / 3
    df["diastolic_BP"] = (df["BPXDI1"] + df["BPXDI2"] + df["BPXDI3"] ) / 3
    df["central_adiposity"] = [1 if (x == 1 and y > 102) or (x == 2 and y > 88) else 0 for x, y in zip(df["RIAGENDR"], df["BMXWAIST"])]
    df["HTN_status"] = [1 if x >= 130 or y >= 80 or z == 1 else 0 for x, y, z in zip(df["systolic_BP"], df["diastolic_BP"], df["BPQ050A"])]
    df["HTN_old_status"] = [1 if x >= 140 or y >= 90 or z == 1 else 0 for x, y, z in zip(df["systolic_BP"], df["diastolic_BP"], df["BPQ050A"])]
    df["diabetes_status"] = [1 if x >= 126 or y == 1 or z == 1 else 0 for x, y, z in zip(df["LBXGLU"], df["DIQ070"], df["DIQ050"])]
    df["cvd_status"] = [1 if b == 1 or c == 1 or d == 1 or e == 1 or f == 1 else 0 for b, c, d, e, f in zip(df["MCQ160B"], df["MCQ160C"], df["MCQ160D"], df["MCQ160E"], df["MCQ160F"])]
    #df["smoking_status"] = [1 if x in [1, 2] else 0 if x == 3 else None for x in df["SMQ040"]]
    df["smoking_status"] = [1 if x in [1, 2] else 0 for x in df["SMQ040"]]
    df["gender"] = ["Male" if x == 1 else "Female" for x in df["RIAGENDR"]]
    df["sleep_disorder"] = [1 if x == 1 else 0 for x in df["SLQ060"]]
    df["race"] = [["Hispanic", "Hispanic", "White", "Black", "", "Asian", "Other"][int(x) - 1] for x in df["RIDRETH3"]]
    df["alcohol"] = [x if x <= 25 else None for x in df["ALQ130"]]
    df["L5_cnt"] = df["L5 (counts)"]
    df["L5_midpoint"] = df["L5 midpoint (dec hours)"]
    df["M10_cnt"] = df["M10 (counts)"]
    df["M10_midpoint"] = df["M10 midpoint (dec hours)"]
    df["sleep_duration"] = df["SLD010H"]
    df["Obesity"] = [1 if x >= 30 else 0 for x in df["BMXBMI"]]
    df["inAnalysis"] = [x != 1 and y > 20 for x, y in zip(df["is_pregnant"], df["RIDAGEYR"])]
    df["physical_function"] = [1 if a in [2,3,4] or b in [2,3,4] or c in [2,3,4] or d in [2,3,4] else 0 if a == 1 and b == 1 and c == 1 and d == 1 else None for a,b,c,d in zip(df["PFQ061H"], df["PFQ061J"], df["PFQ061K"], df["PFQ061L"])]
    df["self_rated_1"] = [1 if x in [1,2,3] else 0 if x in [4, 5] else None for x in df["HSD010"]]
    df["self_rated_2"] = [1 if x in [1,2] else 0 if x in [3, 4, 5] else None for x in df["HSD010"]]
    df["self_rated_cat"] = ["Excellent" if x == 1 else "Very Good" if x == 2 else "Good" if x == 3 else "Fair" if x == 4 else "Poor" if x == 5 else None for x in df["HSD010"]]
    df["x"] = np.array(df["Valid Days"].apply(lambda x: len(str(x).split())))
    print(df["x"].value_counts())
    df = df[df["x"] >= 3]
    return df

load_xpt()
df_all = merge()
df_processed = process(df_all)
df_processed.to_csv("CleanedData.csv", index=False)
print("Jobs Done!")
