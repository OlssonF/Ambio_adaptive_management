directory <- here::here()

download.file(url = "https://sandbox.zenodo.org/records/6078/files/Experiment_output.zip?download=1",
              destfile = file.path(directory,"GOTM",'Output', "Experiment_output.zip"))

unzip(file.path(directory,"GOTM",'Output', "Experiment_output.zip"))
