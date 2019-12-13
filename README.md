Data information

My data is coming from a Super Zip shiny example. It’s data is from the census in 2010. I followed part of this example and extended to meet the need of this project.

The main data has about 31,000 records. There is another data table have to be used to make the map. I use the prepared data of that example.

The main variables are include:
* zipzode: all the zipcode in the United States, one zipcode one record,
* state, all the States in the United States including Washington D.C.,
* county,
* city,
* population(adult population in the zipcode area),
* college(the percentage of adult college with college level of the zipcode area),
* income (the median income of this zipcode),
* rank (the rank of the percentile of the population greater than 5)
* centile (percentile of the income of this zipcode)


Techenique used in the app
My App include these tabs

About my app (use the html file show the basic information and one link adn mathjax was used here)
Data
Data Summeries (dynamic UI,plot click and hover,plot save, scatter, boxplot and common numeric summary were used)
Data exploration (You can subset the data,explore and save the data here,download button was used here)
Data Analysis
PCA Analysis (This is the unsupervised page, I include a biplot and the user can select the PCS and the aspects of the alforithm)
Supervised analysis (for my data, the linear regression, random forest and KNN can be used. The user can select the model and predictors)
Predict (User can select the model type,the predictors to predict the Income. I use an action button and shiny progress to let user kindly wait here)
Interaction Map (Used a absluted sidepannel let user choose the variable and show in the map, also have a histogram showing the density of the college persentile)
