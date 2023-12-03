directory <- here::here()

download.file(url = "https://zenodo.org/record/8136961/files/Experiment_output.zip?download=1",
              destfile = file.path(directory,"GOTM", "Experiment_output.zip"), method = "curl")
unzip(file.path(directory,, "GOTM", "Experiment_output.zip"))
