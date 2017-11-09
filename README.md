# AutoNeta
This is the GitHub page for AutoNeta, the semi-automatic data transformation manager.
The AutoNeta is a R/Shiny application allowing the user to perform transformations on a data set, such that variables are tranformed to symmetric distributions.

Tranforming variables such that they are symmetric is used as a preprocessing step before constructing regression models. If variables are skewed and no preprocessing is performed, models built may have large variances for model parameters or poor prediction capability. The leverage of data points will also not be distributed evenly (some observations will have substantially more impact on model parameters than others).

# Sounds great! where do I start?
1. Download the latest version of R and Rstudio from
   https://www.r-project.org/
   and
   https://www.rstudio.com/
2. Download the project by clicking the green button 'Clone or download' and then clicking 'Download ZIP'.
3. Unpack the ZIP file and open the Rstudio project by clicking on the project file named    'R_Project.Rproj'.
4. In Rstudio, Open the files ui.R or server.Rand click the 'Run App' button on the top of the screen (with a little green 'start' sign next to it).
5. The App will load. The first execution will install the package 'DT' from CRAN. this will require an internet connection. If the App does not open in browser, click on the 'Open in Browser' button at the top of the screen.
6. Click on the menu item named 'Tutorial'. This will bring up a PDF file showing a first usage of the program. The data files for the example are found in the 'Example' directory, also downloaded in ZIP. The tutorial PDf is also found here: https://github.com/barakbri/AutoNeta/blob/master/AutoTransMan/www/Tutorial.pdf


# OK I got through the tutorial, how do I use my own data?
You will need to create a variable definition file, as descibed in the Help file (see Help menu in program, or PDf for help file at https://github.com/barakbri/AutoNeta/blob/master/AutoTransMan/www/project_help.pdf
. Variable definitions consist of the variable name, type and other parameters. You will need to perform


# Need help? Found a bug? Additional features?

We are always glad to help.
We are available at Tzviel Frostig, tfrostig at gmail dot com and Barak Brill, barakbri at mail dot tau dot ac dot il.
You can also report issues via the following link:

https://github.com/barakbri/AutoNeta/issues
