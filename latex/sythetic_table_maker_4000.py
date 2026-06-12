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
    \textbf{Nazwa wiersza} & \textbf{Rekord 1} & \textbf{Rekord 2} & \textbf{Rekord 3} & \textbf{Rekord 4} & \textbf{Rekord 5} \\
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
    latex_row += "\t" + col_name.replace("_", r"\_")
    for value in df[col_name]:
        latex_row += " & " + str(round(value, 2))
    latex_row += " \\\\"
    latex += latex_row

latex += r'''
    \bottomrule
\end{tabular}
\caption{5 syntetycznych rekordów i ich wyniki predykcji.}
\end{table}
'''

print(latex)
