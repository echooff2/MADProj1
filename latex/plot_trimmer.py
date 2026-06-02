import os
from PIL import Image

matrices = os.listdir(r"../Plots/confusion_matrixes")
plot_paths = [r"../Plots/confusion_matrixes/" + i for i in matrices]

for plot_path in plot_paths:
    img = Image.open(plot_path)

    if (size := img.size) != (3000, 3000):
        print("Skipping", plot_path)
        continue
    else:
        print("Trimming", plot_path)
        width, height = size

    crop_left = 133
    crop_right = 133
    crop_top = 775
    crop_bottom = 775

    img = img.crop((crop_left, crop_top, width - crop_right, height - crop_bottom))
    img.save(plot_path)
