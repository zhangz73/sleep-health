import os
import math
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from dateutil.parser import parse
import sensormotion as sm
from joblib import Parallel, delayed
from tqdm import tqdm

import pandas.io.formats.excel
pandas.io.formats.excel.ExcelFormatter.header_style = None

def convert_datetime(datetime_lst):
    return [parse(x).strftime("%Y-%m-%d %H:%M:%S") for x in datetime_lst]

def expand_datetime(datetime_lst, value_lst, freq_in_min=15):
    ret = []
    value_ret = []
    for ts, val in zip(datetime_lst, value_lst):
        curr_ts = parse(ts)
        for dt in range(freq_in_min - 1, -1, -1):
            curr_t = curr_ts - timedelta(minutes = dt)
            ret.append(curr_t.strftime("%Y-%m-%d %H:%M:%S"))
            value_ret.append(val)
    return ret, value_ret

def get_fname(dir, substr):
    f_lst = [f for f in os.listdir(dir) if os.path.isfile(os.path.join(dir, f)) and substr in f]
    return os.path.join(dir, f_lst[0])

def single_clean(id):
#    df_sleep = pd.read_csv(f"Fitabase/{id}/Year0/{id}_minuteSleep_20201228_20210110.csv")
#    df_step = pd.read_csv(f"Fitabase/{id}/Year0/{id}_minuteStepsNarrow_20201228_20210111.csv")
#    df_heart = pd.read_csv(f"Fitabase/{id}/Year0/{id}_heartrate_15min_20201228_20210111.csv")
    df_sleep = pd.read_csv(get_fname(f"Fitabase/{id}/Year0/", "minuteSleep"))
    df_step = pd.read_csv(get_fname(f"Fitabase/{id}/Year0/", "minuteStepsNarrow"))
    heart_fname = get_fname(f"Fitabase/{id}/Year0/", "heartrate")
    df_heart = pd.read_csv(heart_fname)
    
    df_sleep["ConvertedDatetime"] = convert_datetime(df_sleep.date)
    df_sleep = df_sleep[["ConvertedDatetime", "value"]]
    df_sleep.columns = ["ConvertedDatetime", "SleepValue"]
    df_step["ConvertedDatetime"] = convert_datetime(df_step.ActivityMinute)
    df_step = df_step[["ConvertedDatetime", "Steps"]]
    if "15min" in heart_fname:
        converted_date, value_ret = expand_datetime(df_heart.Time, df_heart.Value)
        df_heart = pd.DataFrame.from_dict({"ConvertedDatetime": converted_date, "HeartRate": value_ret})
    else:
        df_heart.columns = ["ConvertedDatetime", "HeartRate"]
    df = df_step.merge(df_sleep, on = "ConvertedDatetime", how="left").merge(df_heart, on = "ConvertedDatetime", how="left")

    N = df.shape[0]
    time = [x * 60 for x in range(N)]
    dct = {"ID": [id] * N,
           "Date": [x.split()[0] for x in df.ConvertedDatetime],
           "Time": [x.split()[1] for x in df.ConvertedDatetime],
           "Axis1": [0] + list(sm.pa.convert_counts(np.array(df["Steps"]), time, time_scale='s', epoch=60, rectify='full', integrate='simpson', plot=False)), #list(df_sub["PAXMXM"]), #[x for x in list(df_sub["PAXMTSM"])], #list(df_sub["PAXMXM"]),
           "Axis2": [None] * N,
           "Axis3": [None] * N,
           "VM magnitude": [None] * N,
           "Steps": [None] * N,
           "Lux": [None] * N,
           "Awake/Sleep": [1 if x == 1 else 0 for x in list(df["SleepValue"])],
           "Wear/non wear": [1 if x > 0 or y is not None else 0 for x,y in zip(list(df["Steps"]), list(df["HeartRate"]))],
           "Weekday": [1 if parse(x).weekday() <= 4 else 0 for x in list(df["ConvertedDatetime"])],
           "Work/non work": [None] * N,
           "Rest/non rest": [None] * N
    }
    df_ret = pd.DataFrame.from_dict(dct)
    df_ret.to_excel(f"IndividualCounts/{id}.xlsx", index=False)

def clean_all():
    path = "Fitabase/"
    f_lst = [f for f in os.listdir(path) if not os.path.isfile(os.path.join(path, f)) and f.startswith("1")]
    for f in tqdm(f_lst):
        if not os.path.isfile(os.path.join("IndividualCounts", f + ".xlsx")):
            try:
                single_clean(int(f))
            except:
                print(f)

def BP_variable():
    path = "HomeBP/"
    f_lst = [f for f in os.listdir(path) if not os.path.isfile(os.path.join(path, f)) and f.startswith("1")]
    dct = {"Subject": [], "morning_SBP": [], "morning_DBP": [], "evening_SBP": [], "evening_DBP": [], "average_SBP": [], "average_DBP": [], "variability_SBP": [], "variability_DBP": []}
    for f in f_lst:
        print(f)
        try:
            curr_path = os.path.join(path, f)
            data_file = [x for x in os.listdir(curr_path) if not (x.startswith(".") or x.endswith(".pdf"))][0]
            if data_file.upper().startswith("Y"):
                curr_path = os.path.join(curr_path, data_file)
                data_file = [x for x in os.listdir(curr_path) if not (x.startswith(".") or x.endswith(".pdf"))][0]
                #print(data_file)
            if ".xlsx" in data_file:
                df = pd.read_excel(os.path.join(curr_path, data_file))
            else:
                df = pd.read_csv(os.path.join(curr_path, data_file))
            target = [x for x in df.columns if "time" in x.lower()][0]
            df[target] = [str(x) for x in df[target]]
            target_sys = [x for x in df.columns if "systolic" in x.lower()][0]
            target_dia = [x for x in df.columns if "diastolic" in x.lower()][0]
            df["DayNight"] = ["Morning" if parse(x).hour in range(5, 18) else "Evening" for x in df[target]]
            dct["Subject"].append(int(f.replace("/", "")))
            dct["morning_SBP"].append(df[df["DayNight"] == "Morning"][target_sys].mean())
            dct["morning_DBP"].append(df[df["DayNight"] == "Morning"][target_dia].mean())
            dct["evening_SBP"].append(df[df["DayNight"] == "Evening"][target_sys].mean())
            dct["evening_DBP"].append(df[df["DayNight"] == "Evening"][target_dia].mean())
            dct["average_SBP"].append(df[target_sys].mean())
            dct["average_DBP"].append(df[target_dia].mean())
            dct["variability_SBP"].append(df[target_sys].std())
            dct["variability_DBP"].append(df[target_dia].std())
        except Exception as e:
            print(str(e))
    return pd.DataFrame.from_dict(dct)

df_BP = BP_variable()
df_BP = df_BP.sort_values("Subject", ascending=True)
df_BP.to_csv("BP.csv", index=False)

df_rar = pd.read_excel("Outputs/Results.xlsx")
df_merged = df_BP.merge(df_rar, on="Subject")
df_merged["M10_cnt"] = df_merged["M10 (counts)"]
df_merged["L5_cnt"] = df_merged["L5 (counts)"]
df_merged["M10_midpoint"] = df_merged["M10 midpoint (dec hours)"]
df_merged["L5_midpoint"] = df_merged["L5 midpoint (dec hours)"]
df_merged.to_csv("Merged.csv", index=False)

#clean_all()

#single_clean(1023)

#df_sleep = pd.read_csv("Year0/1023_minuteSleep_20201228_20210110.csv")
#print(df_sleep.head(), df_sleep.dtypes)
#print(parse(df_sleep.iloc[0]["date"]).weekday())
