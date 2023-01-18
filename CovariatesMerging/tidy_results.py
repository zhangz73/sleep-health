import os
import sys
from decimal import Decimal
import numpy as np
import pandas as pd
from texttable import Texttable
from tqdm import tqdm

df = pd.read_csv("regression_results.csv")
df_cutoff = pd.read_csv("cutoff_results.csv")
out_fname = "regression_results.txt"

reg_model_lst = ["linear", "logist"]
digits = 3
ci_coef = 1.96

def tidy_pval(pval):
    pval = float(pval)
    if pval < 1e-3:
        return "p < 0.001"
    else:
        return "p = " + str(round(pval, 3))
    #return f"p = {Decimal(str(pval)):.2E}"

with open(out_fname, "w") as f:
    f.write("----- Descriptive Characteristics -----\n")
    with open("descriptive_results.csv", "r") as f2:
        t = Texttable()
        lines = f2.readlines()
    lines = [x.replace('"', "") for x in lines]
    headers = lines[0].strip().split(",")
    headers[1:4] = [x + "\n (n=)" for x in headers[1:4]]
    cutoff = round(float(lines[1].split(",")[5].strip()), 3)
    sample_size = lines[1].split(",")[6].strip()
    f.write("\t\tRA Cutoff = " + str(cutoff) + " (Sample Size = " + sample_size + ")\n")
    rows = [headers[:5]]
    prev_var = ""
    for i in range(1, len(lines)):
        line = lines[i]
        arr = line.strip().split(",")
        if "-" in str(arr[0]):
            curr_var = str(arr[0]).split("-")[0]
        else:
            curr_var = ""
        if curr_var != "" and curr_var == prev_var:
            for j in range(4):
                rows[-1][j] += "\n" + str(arr[j])
        else:
            row = [str(arr[0]), str(arr[1]), str(arr[2]), str(arr[3]), tidy_pval(arr[4])]
            rows.append(row)
        prev_var = curr_var
    t.add_rows(rows)
    f.write(str(t.draw()) + "\n\n")
    
    for reg_model in reg_model_lst:
        if reg_model == "linear":
            f.write("----- Linear Regression -----\n")
        else:
            f.write("----- Logistic Regression -----\n")
        df_sub = df[df["RegModel"] == reg_model]
        output_lst = list(set(df_sub["Output"]))
        input_lst = list(set(df_sub["Input"]))
        adj_model_lst = sorted(list(set(df_sub["AdjModel"])))
        for output in output_lst:
            sample_size = df[df["Output"] == output]["SampleSize"].iloc[0]
            f.write(output + " (Sample Size = " + str(sample_size) + ")\n")
            t = Texttable()
            if reg_model == "linear":
                rows = [["Input"] + ["Model " + str(int(x)) + "\n Est (p-value)\n(95% C.I.)" for x in adj_model_lst]]
                for input in input_lst:
                    row = [input]
                    for adj_model in adj_model_lst:
                        curr = df_sub[(df_sub["Input"] == input) & (df_sub["AdjModel"] == adj_model) & (df_sub["Output"] == output)]
                        if curr.shape[0] > 0:
                            est = curr.iloc[0]["Estimates"]
                            stderr = curr.iloc[0]["Std.Err"]
                            pval = tidy_pval(curr.iloc[0]["P.Value"])
                            content = str(round(est, digits)) + " (" + pval + ")\n" + "(" + str(round(est - ci_coef * stderr, digits)) + " to " + str(round(est + ci_coef * stderr, digits)) + ")"
                        else:
                            content = ""
                        row.append(content)
                    rows.append(row)
            else:
                rows = [["Input"] + ["Model " + str(int(x)) + "\n Tertile1 OR (95% C.I.)\n Tertile2 OR (95% C.I.)\n Tertile3 OR (95% C.I.)\np-trend" for x in adj_model_lst]]
                for input in input_lst:
                    cutoff1 = round(df_cutoff[df_cutoff["Input"] == input.replace("_tertile", "")]["Cutoff1"].iloc[0], digits)
                    cutoff2 = round(df_cutoff[df_cutoff["Input"] == input.replace("_tertile", "")]["Cutoff2"].iloc[0], digits)
                    row = [input + f"\nTertile1 (<= {cutoff1})\nTertile2 ({cutoff1} ~ {cutoff2})\nTertile3 (> {cutoff2})"]
                    for adj_model in adj_model_lst:
                        curr = df_sub[(df_sub["Input"] == input) & (df_sub["AdjModel"] == adj_model) & (df_sub["Output"] == output)]
                        if curr.shape[0] > 0:
                            est = curr.iloc[0]["Estimates"]
                            stderr = curr.iloc[0]["Std.Err"]
                            ptrend = tidy_pval(curr.iloc[0]["P.Value"])
                            #or1 = curr.iloc[0]["EstimatesTertile1"]
                            #std1 = curr.iloc[0]["Std.ErrTertile1"]
                            or2 = curr.iloc[0]["EstimatesTertile2"]
                            std2 = curr.iloc[0]["Std.ErrTertile2"]
                            or3 = curr.iloc[0]["EstimatesTertile3"]
                            std3 = curr.iloc[0]["Std.ErrTertile3"]
                            content = str(round(np.exp(or2), digits)) + "(" + str(round(np.exp(or2 - ci_coef * std2), digits)) + " to " + str(round(np.exp(or2 + ci_coef * std2), digits)) + ")\n" + str(round(np.exp(or3), digits)) + "(" + str(round(np.exp(or3 - ci_coef * std3), digits)) + " to " + str(round(np.exp(or3 + ci_coef * std3), digits)) + ")\n" + "" + ptrend
                        else:
                            content = ""
                        #content = str(round(or1, digits)) + "(" + str(round(or1 - ci_coef * std1, digits)) + " to " + str(round(or1 + ci_coef * std1, digits)) + ")\n" + str(round(or2, digits)) + "(" + str(round(or2 - ci_coef * std2, digits)) + " to " + str(round(or2 + ci_coef * std2, digits)) + ")\n" + str(round(or3, digits)) + "(" + str(round(or3 - ci_coef * std3, digits)) + " to " + str(round(or3 + ci_coef * std3, digits)) + ")\n" + "p = " + str(round(ptrend, digits))
                        row.append(content)
                    rows.append(row)
            t.add_rows(rows)
            f.write(str(t.draw()) + "\n\n")
