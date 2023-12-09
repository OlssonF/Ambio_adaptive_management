directory <- here::here()

# GOTM scenarios output
if (!dir.exists(file.path(file.path(directory,"GOTM",'Output')))) {
  dir.create(file.path(file.path(directory,"GOTM",'Output')), recursive = T)
}

#this download can take a while to download
download.file(url = "https://zenodo.org/records/10309370/files/Experiment_output.zip?download=1",
              destfile = file.path(directory,"GOTM",'Output', "Experiment_output.zip"), method = 'curl')

setwd(file.path(file.path(directory,"GOTM",'Output')))
unzip(file.path(directory,"GOTM",'Output', "Experiment_output.zip"))


# output from GOTM based on observations
download.file(url = "https://zenodo.org/records/10309370/files/Mod_z.txt?download=1",
              destfile = file.path(directory,"GOTM",'Output', 'Mod_z.txt'), method = 'curl')
