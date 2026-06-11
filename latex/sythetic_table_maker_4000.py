import os
import re
import pandas as pd

folder_path = "..\\csv\\"  # sam se zemin te ukosniki >:(

class_file_names = os.listdir(folder_path)
feature_file_name = "synth_data_notscaled.csv"
class_file_names.remove(feature_file_name)

latex = r'''
\begin{table}[H]
  \centering
\begin{tabular}{l *{5}{c}}
    \toprule
    Seria Syntetycznych danych & 1 & 2 & 3 & 4 & 5 \\
    \midrule'''

feature_file_path = folder_path + feature_file_name
df = pd.read_csv(feature_file_path)

df = df.drop(columns=df.columns[0])  # drop index column
df = df[["blueWins"] + [c for c in df.columns if c != "blueWins"]]  # move blueWins to the beginning
for class_file_name in class_file_names:  # connect dataframes
    class_file_path = folder_path + class_file_name
    class_df = pd.read_csv(class_file_path)
    pattern = r"synth_data_(\w*?).csv"
    class_name = re.findall(pattern, class_file_name)[0]
    df.insert(0, class_name, class_df["x"])

for col_name in df:
    latex_row = "\n"
    latex_row += "\t" + col_name
    for value in df[col_name]:
        latex_row += " & " + str(value)
    latex += latex_row

latex += r'''
\end{tabular}
\caption{5 danych syntetycznych i wyniki predykcji dla nich.}
\end{table}
'''

print(latex)
