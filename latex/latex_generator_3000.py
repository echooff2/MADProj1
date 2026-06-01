import os

def reduce_spaces(str):
    for i in range(10):
        str = str.replace("  ", " ")
        str = str.replace("\t", " ")
    return str

def format(str):
    str = str.replace(" ", " & ")
    str = str.replace("\n", r" \\" + "\n")
    return str + r" \\"


def format_var_name(var):
    result = ""
    for c in var:
        result += c if c.lower() == c else " " + c.lower()
    return result.capitalize()


def print_latex_graphs(var_names):
    boxplots = " ".join(os.listdir(r"../Plots/boxplots"))
    hist_and_bar = " ".join(os.listdir(r"../Plots/histograms_and_barplots"))
    for var_name in var_names:
        if var_name not in boxplots:
            continue
        if "hist_" + var_name in hist_and_bar:
            plot_type = "hist"
        else:
            plot_type = "bar"
        print("""
    \\begin{figure}[H]
        \\centering
        \\includegraphics[height=0.33\\textwidth]{../Plots/boxplots/box_""" + var_name + """.png}
        \\hfill
        \\includegraphics[height=0.33\\textwidth]{../Plots/histograms_and_barplots/""" + plot_type + "_" + var_name + """.png}
        \\caption{Boxplot oraz """ + ("histogram z nałożoną funkcją gęstości" if plot_type == "hist" else "barplot") + """ dla zmiennej """ + (var_name.replace(r"_", r"\_") if "PCA" in var_name else var_name) + """.}
        \\label{fig:both_images}
    \\end{figure}
    """)


def print_latex_var_list(var_names):
    for var_name in var_names:
        print(r"    \item {\bf " + format_var_name(var_name) + r"} --- ")


def print_latex_var_table(var_names):
    for var_name in var_names:
        # print(format_var_name(var_name) + " & Numeryczna & Ilościowa & " + ("Ciągła" if "avg" in format_var_name(var_name) else "Skokowa") +" \\\\")
        print(var_name + " & Numeryczna & Ilościowa & " + ("Ciągła" if "avg" in format_var_name(var_name) else "Skokowa") +" \\\\")


summary = """blueWardsPlaced              21.19  16.00  5.00 105.00 13.92  2.94
blueWardsDestroyed            2.83   3.00  0.00  27.00  2.18  2.85
blueDragons                   0.36   0.00  0.00   1.00  0.48  0.57
blueHeralds                   0.19   0.00  0.00   1.00  0.39  1.60
blueTotalJungleMinionsKilled 50.50  50.00  0.00  92.00  9.91  0.12
blueCSPerMin                 21.67  21.80  9.00  28.30  2.19 -0.26
redWardsPlaced               21.14  16.00  6.00 104.00 13.55  2.81
redWardsDestroyed             2.73   2.00  0.00  24.00  2.15  2.95
redFirstBlood                 0.50   0.00  0.00   1.00  0.50  0.02
redDragons                    0.41   0.00  0.00   1.00  0.49  0.35
redHeralds                    0.16   0.00  0.00   1.00  0.37  1.86
redTotalJungleMinionsKilled  51.33  51.00  4.00  92.00 10.05  0.23
redCSPerMin                  21.74  21.80 10.70  28.90  2.19 -0.29
blueKA                       12.82  12.00  0.00  51.00  6.75  0.69
redKA                        12.79  12.00  0.00  43.00  6.66  0.63
diff_PCA_Component_1          0.00   0.01 -6.36   6.55  1.38 -0.03
"""

summary = summary.strip()
summary = reduce_spaces(summary)
summary = format(summary)
print(summary)
print()

var_names = [line.split(" ")[0] for line in summary.splitlines()]
# print(var_names)
# print()

# print_latex_graphs(var_names)
# print_latex_var_list(var_names)
# print_latex_var_table(var_names)
# print()

# results = """Cluster blueKills blueDeaths blueAssists blueTotalGold blueAvgLevel blueTotalExperience blueTotalMinionsKilled blueTotalJungleMinionsKilled redAssists redTotalGold redAvgLevel redTotalExperience redTotalMinionsKilled redTotalJungleMinionsKilled
# 1  4.530545   7.496208    4.654870      15537.83     6.757884            17255.45               210.3110                     49.25181   8.096831     17326.60    7.088774           18648.18              224.6253                    52.72263
# 2  7.845526   4.661706    8.434302      17476.90     7.088920            18652.44               223.7194                     51.65116   4.884424     15583.77    6.763035           17268.16              210.0955                    49.59875"""
# results = results.strip()
# results = reduce_spaces(results)
# chuj = [line.split(" ") for line in results.split("\n")]
# for i in range(14):
#     print(chuj[0][i], end=" & ")
#     print(chuj[1][i], end=" & ")
#     print(chuj[2][i], end=" \\\\\n")

