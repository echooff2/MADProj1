import os
import re

repo_path = r"D:\School\SGGW\semestr_4\Metody analizy danych\projekt2\repo"

r_files = []
for file in os.listdir(repo_path):
    if file.endswith(".R"):
        r_files.append(file)

libraries = []

for file in r_files:
    file_path = repo_path + "\\" + file
    print(file_path)
    
    with open(file_path, "r", encoding="utf-8") as f:
        file_content = f.read()

    pattern = r"library\((.*?)\)"
    matches = re.findall(pattern, file_content)
    libraries += matches
    print(matches)
    print()

libraries = list(set(libraries))
print(libraries)
print()

print(r"\textit{" + libraries[0], end="")
for lib in libraries[1:-1]:
    print(", " + lib, end="")
print("} oraz " + r"\textit{" + libraries[-1] + "}")
