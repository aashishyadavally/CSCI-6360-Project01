# CSCI-6360-Project01 : Regression Problem in Scalation and R

This project covers implementations of Simple Regression, Regression Weighted Least Squares, Ridge Regression, Lasso Regression, Quad Regression, and Response Surface in Scalation and R , over 10 datasets downloaded from the UCI Machine earning Repository. 

## Getting Started
These instructions describe the prerequisites and steps to get the project up and running.

### Prerequisites
This project has the following requirements for Scalation:
* Scala 2.12.8 +
* Java 8
* sbt_1.0 +

It also has the following requirements for executing the R scripts:
* R 3.5.2 +
* Packages: PARTY, CARET, glmnet

### Usage
After cloning the repository, to generate the  R<sup>2</sup> - R<sub>bar</sub><sup>2</sup> - R<sub>CV</sub><sup>2</sup> plots, one can navigate to the Scalation folder which contains the build.sbt file. Here, open the terminal and run the command:`sbt run`
This will build the Scalation project, and the user will get a prompt to select from the 10 datasets. The user can enter his choice by enterining a number between '1' to '10', each corresponding to the respective dataset. 

If the user wishes to use this project for their own dataset, they will have to enter '11' as their choice, which will prompt them to enter the path of their dataset (in CSV format). However, there are a few guidelines for the dataset that the user chooses to experiment on:
* it has to be a numeric dataset (data-encoding hasn't been implemented yet!)
* the first column of the dataset needs to be the 'Y' attribute.
If the user chooses to add their own dataset to the list, they will have to navigate one step back, to the `/data` directory and move the dataset there. The naming convention followed in the project is, "x.csv" where 'x' is the choice that the user inputs.

### Contributors
See CONTRIBUTORS file for more details.

### Authors
* [Aashish Yadavally](https://github.com/aashishyadavally)
* [Jayant Parashar](https://github.com/Jayant1234)
### License
This project is licensed under the __MIT License__. See [LICENSE](https://github.com/aashishyadavally/CSCI-6360-Project01/blob/master/LICENSE) for more details.


