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
    for var_name in var_names:
        print("""
    \\begin{figure}[H]
        \\centering
        \\includegraphics[width=0.43\\textwidth]{../wykresy/boxploty/box_""" + var_name + """.png}
        \\hfill
        \\includegraphics[width=0.43\\textwidth]{../wykresy/histogramy/hist_""" + var_name + """.png}
        \\caption{Boxplot oraz histogram dla zmiennej """ + var_name + """.}
        \\label{fig:both_images}
    \\end{figure}
    """)


def print_latex_var_list(var_names):
    for var_name in var_names:
        print(r"    \item {\bf " + format_var_name(var_name) + r"} --- ")


def print_latex_var_table(var_names):
    for var_name in var_names:
        print(format_var_name(var_name) + " & Numeryczna & Ilościowa & " + ("Ciągła" if "avg" in format_var_name(var_name) else "Skokowa") +" \\\\")


summary = """blueWardsPlaced                 22.29    16.0      5.0   250.0   18.02  4.14
blueWardsDestroyed               2.82     3.0      0.0    27.0    2.17  2.85
blueFirstBlood                   0.50     1.0      0.0     1.0    0.50 -0.02
blueKills                        6.18     6.0      0.0    22.0    3.01  0.54
blueDeaths                       6.14     6.0      0.0    22.0    2.93  0.51
blueAssists                      6.65     6.0      0.0    29.0    4.06  0.89
blueEliteMonsters                0.55     0.0      0.0     2.0    0.63  0.69
blueDragons                      0.36     0.0      0.0     1.0    0.48  0.57
blueHeralds                      0.19     0.0      0.0     1.0    0.39  1.60
blueTowersDestroyed              0.05     0.0      0.0     4.0    0.24  5.59
blueTotalGold                16503.46 16398.0  10730.0 23701.0 1535.45  0.47
blueAvgLevel                     6.92     7.0      4.6     8.0    0.31 -0.34
blueTotalExperience          17928.11 17951.0  10098.0 22224.0 1200.52 -0.25
blueTotalMinionsKilled         216.70   218.0     90.0   283.0   21.86 -0.27
blueTotalJungleMinionsKilled    50.51    50.0      0.0    92.0    9.90  0.12
blueGoldDiff                    14.41    14.0 -10830.0 11467.0 2453.35  0.03
blueExperienceDiff             -33.62   -28.0  -9333.0  8348.0 1920.37  0.02
blueCSPerMin                    21.67    21.8      9.0    28.3    2.19 -0.27
blueGoldPerMin                1650.35  1639.8   1073.0  2370.1  153.54  0.47
redWardsPlaced                  22.37    16.0      6.0   276.0   18.46  4.56
redWardsDestroyed                2.72     2.0      0.0    24.0    2.14  2.95
redFirstBlood                    0.50     0.0      0.0     1.0    0.50  0.02
redKills                         6.14     6.0      0.0    22.0    2.93  0.51
redDeaths                        6.18     6.0      0.0    22.0    3.01  0.54
redAssists                       6.66     6.0      0.0    28.0    4.06  0.82
redEliteMonsters                 0.57     0.0      0.0     2.0    0.63  0.62
redDragons                       0.41     0.0      0.0     1.0    0.49  0.35
redHeralds                       0.16     0.0      0.0     1.0    0.37  1.85
redTowersDestroyed               0.04     0.0      0.0     2.0    0.22  5.34
redTotalGold                 16489.04 16378.0  11212.0 22732.0 1490.89  0.41
redAvgLevel                      6.93     7.0      4.8     8.2    0.31 -0.40
redTotalExperience           17961.73 17974.0  10465.0 22269.0 1198.58 -0.28
redTotalMinionsKilled          217.35   218.0    107.0   289.0   21.91 -0.29
redTotalJungleMinionsKilled     51.31    51.0      4.0    92.0   10.03  0.23
redGoldDiff                    -14.41   -14.0 -11467.0 10830.0 2453.35 -0.03
redExperienceDiff               33.62    28.0  -8348.0  9333.0 1920.37 -0.02
redCSPerMin                     21.73    21.8     10.7    28.9    2.19 -0.29
redGoldPerMin                 1648.90  1637.8   1121.2  2273.2  149.09  0.41
"""

summary = summary.strip()
summary = reduce_spaces(summary)
summary = format(summary)
print(summary)
print()

var_names = [line.split(" ")[0] for line in summary.splitlines()]
# print(var_names)
print()

# print_latex_graphs(var_names)
# print_latex_var_list(var_names)
# print_latex_var_table(var_names)


results = """Cluster blueKills blueDeaths blueAssists blueTotalGold blueAvgLevel blueTotalExperience blueTotalMinionsKilled blueTotalJungleMinionsKilled redAssists redTotalGold redAvgLevel redTotalExperience redTotalMinionsKilled redTotalJungleMinionsKilled
1  4.530545   7.496208    4.654870      15537.83     6.757884            17255.45               210.3110                     49.25181   8.096831     17326.60    7.088774           18648.18              224.6253                    52.72263
2  7.845526   4.661706    8.434302      17476.90     7.088920            18652.44               223.7194                     51.65116   4.884424     15583.77    6.763035           17268.16              210.0955                    49.59875"""

results = results.strip()
results = reduce_spaces(results)

chuj = [line.split(" ") for line in results.split("\n")]

for i in range(14):
    print(chuj[0][i], end=" & ")
    print(chuj[1][i], end=" & ")
    print(chuj[2][i], end=" \\\\\n")

