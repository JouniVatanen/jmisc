# What you need to do, for install() to work
# 1. Install Rtools 4.0 and R 4.0.3
# 2. Set ...\Rtools\usr\bin to your PATH e.g. with Powershell
# https://stackoverflow.com/questions/714877/setting-windows-powershell-environment-variables
# 3. Set ...\Rtools\mingw`$(WIN)\bin to your BINPREF e.g. with Powershell
# 4. Check RTOOLS40_HOME points to Rtools ...\Rtools directory at ..\R\etc\x64\Makeconf

# Checkpoint installs packages
if (!require("checkpoint")) install.packages("checkpoint")
# use a date few weeks after R.version was released
checkpoint::checkpoint("2020-10-26", checkpoint_location = Sys.getenv("USERPROFILE"))

# Document and install package
if (!require(devtools)) install.packages("devtools")
devtools::document()
devtools::install(upgrade = FALSE)

# Commit changes and push the files to the github
# 1. Commit changes shell "git add .;git commit -m 'comment'" OR Rstudio UI
# 2. Push changes either shell "git push" OR Rstudio UI
# 3. Install from github with devtools::install_github(JouniVatanen/stools)
