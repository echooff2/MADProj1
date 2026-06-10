import os
from ctypes import c_wchar

import pandas as pd

repo_path = r"../csv/"

csv_files = os.listdir(repo_path)

string = r'''
\begin{table}[H]
  \centering
\begin{tabular}{l *{5}{c}}
    \toprule
    Seria Syntetycznych danych & 1 & 2 & 3 & 4 & 5 \\
    \midrule
'''

for file in csv_files:
    file_path = repo_path + "\\" + file

    print(file_path)

    with open(file_path, "r", encoding="utf-8") as f:
        file_content = f.read()


string += '''
\end{tabular}
\caption{5 danych syntetycznych i wyniki predykcji dla nich.}
\end{table}
'''