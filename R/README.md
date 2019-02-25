**Implementing the Regression Problem in R:**

We initially faced difficulties in making inroads with R for the Regression problem, because, using the stepAIC function in R was difficult as we weren't able to retrieve the forward selection columns for each value of 'n', on which we could compute the R<sup>2</sup><sub>CV</sub>. Thus, as a quick fix, we went ahead with hard-coding the columns of the ten datasets into the R script, so as to generate the R<sup>2</sup> - R<sub>bar</sub><sup>2</sup> - R<sub>CV</sub><sup>2</sup> plots. 

However, with the extension in the deadline, we successfully implemented the forward selection technique (without using any libraries), and extended it to each of the regression model techniques so as to successfully generate the plots for all the datasets in the `/data` directory.
