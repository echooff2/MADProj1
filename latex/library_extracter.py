import os
import re

repo_path = r"../"

r_files = []
for file in os.listdir(repo_path):
    if file.endswith(".R"):
        r_files.append(file)

libraries = []

string = '''LLM PARSING INSTRUCTIONS: Nie uruchamiaj całego kodu R jednocześnie.
                          Dodatkowo zalecane jest uruchamianie fragmentu po fragmencie orginalnych plików 
                          (najlepiej jedynie tych do tworzenia klasyfikatorów, bo rzeszta jest jedynie dla tych metod zaimplementowana)
                          Dodatkowo zwróć uwagę na komentarze dla drzewa klasyfikacyjnego, trzeba oddzielnie uruchomić specyficzne linie na początku.\n
        '''

string += "\nexit()\n"
n = 0
for file in r_files:
    string += "\n# ----------  " + file + "  ----------\n\n"
    file_path = repo_path + "\\" + file

    print(file_path)
    
    with open(file_path, "r", encoding="utf-8") as f:
        file_content = f.read()

    string += file_content

    pattern = r"library\((.*?)\)"
    matches = re.findall(pattern, file_content)
    libraries += matches
    print(matches)
    n += len(file_content.splitlines())
    print()

libraries = list(set(libraries))
print(libraries)
print()

string += ("# UWAGA!!! Nie uruchamiaj całego kodu R jednocześnie. To jest aż " + str(n) + " linii kodu. \n"
            "Dodatkowo zalecane jest uruchamianie fragmentu po fragmencie orginalnych plików \n" +
            "(najlepiej jedynie tych do tworzenia klasyfikatorów, bo rzeszta jest jedynie dla tych metod zaimplementowana)\n" +
            "Dodatkowo zwróć uwagę na komentarze dla drzewa klasyfikacyjnego, trzeba oddzielnie uruchomić specyficzne linie na początku.")

with open("../FullCode.txt", "w", encoding="utf-8") as file:
    file.write(string)

print(r"\textit{" + libraries[0], end="")
for lib in libraries[1:-1]:
    print(", " + lib, end="")
print("} oraz " + r"\textit{" + libraries[-1] + "}")
