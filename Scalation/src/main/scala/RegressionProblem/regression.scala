//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aashish Yadavally
 *  @version 1.0
 *  @date    Feb 18, 2019
 *  @see     LICENSE (MIT style license file)
 */

package RegressionProblem
import scalation.columnar_db.Relation
import scalation.util.banner
import scala.collection.mutable.Set
import scalation.linalgebra._
import scalation.analytics._
import scalation.plot.PlotM


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Defining IllegalChoiceException class
*/
class IllegalChoiceException(s: String) extends Exception(s){}

class Exception1{
	@throws(classOf[IllegalChoiceException])
	def validate(choice: Int){
		if((choice < 0) || (choice > 11)) {
			throw new IllegalChoiceException("Invalid Choice.")
		}
	}
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Defining a class for cross-validation, which has different regression members, 
* which return RCV-Squared averaged across all folds for that model.
*/
class cross_validation(x: MatriD, y: VectorD) {
	val m = x.dim1 // Number of data instances
	val k = 10 // Number of folds
	val indices = VectorI(0 until m).split(k)  // Splitting into folds
	val rSq_vector = new VectorD (10)
	var ctr = 0
	// Cross-validation member for simple regression model
	def simple_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new Regression(XTr, YTr)	// Instantiating a regression model for different folds
			rg.train().eval(XTe, YTe)	
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble  // Saving R-Squared for different folds in rSq_vector
			ctr += 1
		}
		return rSq_vector.mean  // Returning the mean of the R-Squared for different folds, i.e, RCV-Squared
	}
	// Cross-validation member for regression_wls model
	def regression_wls_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new Regression_WLS(XTr, YTr)	// Instantiating a regression model for different folds
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble  // Saving R-Squared for different folds in rSq_vector
			ctr += 1
		}
		return rSq_vector.mean // Returning the mean of the R-Squared for different folds, i.e, RCV-Squared
	}
	// Cross-validation member for ridge regression model
	def ridge_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new RidgeRegression(XTr, YTr)	// Instantiating a regression model for different folds
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble  // Saving R-Squared for different folds in rSq_vector
			ctr += 1
		}
		return rSq_vector.mean  // Returning the mean of the R-Squared for different folds, i.e, RCV-Squared
	}
	// Cross-validation member for quad regression model	
	def quad_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new QuadRegression(XTr, YTr)	// Instantiating a regression model for different folds
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble  // Saving R-Squared for different folds in rSq_vector
			ctr += 1
		}
		return rSq_vector.mean  // Returning the mean of the R-Squared for different folds, i.e, RCV-Squared
	}
	// Cross-validation member for lasso regression model
	def lasso_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new LassoRegression(XTr, YTr)	// Instantiating a regression model for different folds
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble  // Saving R-Squared for different folds in rSq_vector
			ctr += 1
		}
		return rSq_vector.mean  // Returning the mean of the R-Squared for different folds, i.e, RCV-Squared
	}
	// Cross-validation member for Response Surface Model
	def response_surface_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new Regression(XTr, YTr)	// Instantiating a regression model for different folds
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble  // Saving R-Squared for different folds in rSq_vector
			ctr += 1
		}
		return rSq_vector.mean  // Returning the mean of the R-Squared for different folds, i.e, RCV-Squared
	}
}	


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'RegressionTest' object uses the defined cross-validation class, pre-defined 
* MatrixD and Regression classes to perform multiple regressions and subsequent analysis 
* on different numerical datasets, in the 'data' folder.  
*  > "sbt run" in the Scalation folder containing the build file to run the program.
* User gets two choices, once, to run on the dataset of his/her choice and again, to 
* choose the model to build the R2-RBar2-RCV2 graph on.
*/			
object RegressionTest extends App {

	// Method to implement the Simple Regression Model for R2-RBar2-RCV2 plot
	def regression_sim (x: MatriD, y: VectorD)
	{
		banner ("Implementing Simple Regression... ")
		val rg_sim = new Regression (x, y)	// Instantiating a simple regression model
		val fs_cols = Set(0)				// Selected features
		val RSqNormal = new VectorD (x.dim2)
		val RSqAdj = new VectorD (x.dim2) 
		val RSqCV = new VectorD (x.dim2)
		val n = VectorD.range(1, x.dim2)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_sim.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal(j) = 100*new_qof(0)	
			RSqAdj(j) = 100*new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)	// Obtaining X-matrix for selected features
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = 100*cv.simple_regression_cv()		// Calculating RSqCV for each feature selected
		}
		// Saving R-Squared, RBar-Squared, RCV-Squared vectors into a matrix, to plot them concurrently
		val plot_mat = new MatrixD (3, x.dim2-1)
		plot_mat.update(0, RSqAdj(1 until x.dim2))
		plot_mat.update(1, RSqNormal(1 until x.dim2))
		plot_mat.update(2, RSqCV(1 until x.dim2))
		new PlotM(n, plot_mat, lines=true).saveImage("regression_simple.png")
		banner ("Successfully implemented Simple Regression!")
	}
	
	// Method to implement the Regression_WLS Model for R2-RBar2-RCV2 plot
	def regression_WLS (x: MatriD, y: VectorD)
	{
		banner ("Implementing Regression WLS... ")
		val rg_WLS = new Regression_WLS (x, y)	// Instantiating a regression_WLS model
		val fs_cols = Set(0)					// Selected features
		val RSqNormal = new VectorD (x.dim2)	
		val RSqAdj = new VectorD (x.dim2) 
		val RSqCV = new VectorD (x.dim2)
		val n = VectorD.range(1, x.dim2)
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_WLS.forwardSel(fs_cols, false)
			fs_cols += add_var	
			RSqNormal(j) = 100*new_qof(0)
			RSqAdj (j) = 100*new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)	// Obtaining X-matrix for selected features
			val cv = new cross_validation(x_cv, y)		
			RSqCV(j) = 100*cv.regression_wls_cv()			// Calculating RSqCV for each feature selected
		}
		// Saving R-Squared, RBar-Squared, RCV-Squared vectors into a matrix, to plot them concurrently
		val plot_mat = new MatrixD (3, x.dim2-1)
		plot_mat.update(0, RSqAdj(1 until x.dim2))
		plot_mat.update(1, RSqNormal(1 until x.dim2))
		plot_mat.update(2, RSqCV(1 until x.dim2))
		new PlotM(n, plot_mat, lines=true).saveImage("regression_WLS.png")
		banner ("Successfully implemented Regression WLS!")
	}

	// Method to implement the Ridge Regression Model for R2-RBar2-RCV2 plot
	def ridge_regression (x: MatriD, y: VectorD)
	{
		banner ("Implementing Ridge Regression... ")
		val rg_rid = new RidgeRegression (x, y)	// Instantiating a Ridge regression model
		val fs_cols = Set.empty[Int]			// Selected features
		val RSqNormal = new VectorD(x.dim2)
		val RSqAdj = new VectorD(x.dim2)
		val RSqCV = new VectorD(x.dim2)
		val n = VectorD.range(3, x.dim2 + 2)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_rid.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = 100*new_qof (0)
			RSqAdj (j) = 100*new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)	// Obtaining X-matrix for selected features
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = 100*cv.ridge_regression_cv()			// Calculating RSqCV for each feature selected
		}
		// Saving R-Squared, RBar-Squared, RCV-Squared vectors into a matrix, to plot them concurrently
		val plot_mat = new MatrixD(3, x.dim2-1)
		plot_mat.update(0, RSqAdj(1 until x.dim2))
		plot_mat.update(1, RSqNormal(1 until x.dim2))
		plot_mat.update(2, RSqCV(1 until x.dim2))
		new PlotM(n, plot_mat, lines=true).saveImage("ridge_regression.png")
		banner ("Successfully implemented Ridge Regression!")
	}

	
	// Method to implement the Quad Regression Model for R2-RBar2-RCV2 plot
	def quad_regression (x_initial: MatriD, y: VectorD)
	{
		banner ("Implementing Quadratic Regression... ")
		val rg_quad = new QuadRegression (x_initial, y)	// Instantiating a Quadratic regression model
		val x = rg_quad.getX							// Selected features
		val fs_cols = Set.empty[Int]
		val num_terms = x.dim2
		val RSqNormal = new VectorD(num_terms)
		val RSqAdj = new VectorD(num_terms)
		val RSqCV = new VectorD(num_terms)
		val n = VectorD.range(3, num_terms+2)
		
		for (j <- 1 until num_terms){
			val (add_var, new_param, new_qof) = rg_quad.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = 100*new_qof (0)
			RSqAdj (j) = 100*new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)	// Obtaining X-matrix for selected features
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = 100*cv.quad_regression_cv()			// Calculating RSqCV for each feature selected
		}
		// Saving R-Squared, RBar-Squared, RCV-Squared vectors into a matrix, to plot them concurrently
		val plot_mat = new MatrixD(3, num_terms-1)
		plot_mat.update(0, RSqAdj(1 until num_terms))
		plot_mat.update(1, RSqNormal(1 until num_terms))
		plot_mat.update(2, RSqCV(1 until num_terms))
		new PlotM(n, plot_mat, lines=true).saveImage("quad_regression.png")

		banner ("Successfully implemented Quadratic Regression!")		
	}


	// Method to implement the Lasso Regression Model for R2-RBar2-RCV2 plot
	def lasso_regression(x: MatriD, y: VectorD)
	{
		banner ("Implementing Lasso Regression...")
		val rg_lasso = new LassoRegression (x, y)	// Instantiating a Lasso regression model
		//val fs_cols = Set.empty[Int]				
		val fs_cols = Set(0)						// Selected features
		val RSqNormal = new VectorD(x.dim2)
		val RSqAdj = new VectorD(x.dim2)
		val RSqCV = new VectorD(x.dim2)
		val n = VectorD.range(3, x.dim2 + 3)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_lasso.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = 100*new_qof (0)
			RSqAdj (j) = 100*new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)	// Obtaining X-matrix for selected features
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = 100*cv.lasso_regression_cv()			// Calculating RSqCV for each feature selected
		}
		// Saving R-Squared, RBar-Squared, RCV-Squared vectors into a matrix, to plot them concurrently
		val plot_mat = new MatrixD(3, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).saveImage("lasso_regression.png")
		banner ("Successfully implemented Lasso Regression!")
	}
	
	// Method to implement the Response Surface model for R2-RBar2-RCV2 plot
	def response_surface (x_initial: MatriD, y: VectorD)
	{
		banner ("Implementing Response Surface... ")
		val rg_rs = new ResponseSurface (x_initial, y)	// Instantiating a Reponse Surface model
		val x = rg_rs.getX
		val fs_cols = Set.empty[Int]					// Selected features
		val num_terms = x.dim2
		val RSqNormal = new VectorD(num_terms)
		val RSqAdj = new VectorD(num_terms)
		val RSqCV = new VectorD(num_terms)
		val n = VectorD.range(1, num_terms+1)
		
		for (j <- 1 until num_terms){
			val (add_var, new_param, new_qof) = rg_rs.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = 100*new_qof (0)
			RSqAdj (j) = 100*new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)	// Obtaining X-matrix for selected features
			println(x_cv.dim1, x_cv.dim2)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = 100*cv.response_surface_cv()	
		}
		// Saving R-Squared, RBar-Squared, RCV-Squared vectors into a matrix, to plot them concurrently
		val plot_mat = new MatrixD(3, num_terms)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).saveImage("response_surface.png")
		banner ("Successfully implemented Response Surface!") 
	}
	
	def main(){
		// Giving user the choice to select from ten datasets, or to give data path to own CSV file
		println("-"*75)
		println (" Select dataset: \n\t 1. Auto MPG \n\t 2. Beijing PM2.5 Dataset \n\t 3. Concrete Compressive Strength Dataset \n\t 4. Real Estate Valuation Dataset \n\t 5. Parkinson's Tele Monitoring \n\t 6. GPS Trajectories")
		println("\t 7. Appliances Energy Prediction  \n\t 8. Combined Cycle Powerplant \n\t 9. CSM Dataset \n\t 10. Naval Propulsion Dataset \n\t 11. For other datasets, enter: /correct/path/to/data/csv")
		println("-"*75)
		
		val choice	 = scala.io.StdIn.readLine()
		// Exception, to alert if user choice is not between 1 and 11
		var e = new Exception1()
		try {
			e.validate(choice.toInt)
		} catch {
			case ex: Exception => println("Exception Occured : " + ex)
		}
						
		val filename = if(choice != "11") {choice + ".csv"} else {scala.io.StdIn.readLine()}  // Reads user's input for data path if user enters '11'
		val dataset = Relation (filename, "dataset", null, -1) 			// Saving CSV as a relation
		val column_names = dataset.colNames			// Array of column names in relation
		val num_cols = dataset.cols					// Number of columns in dataset

		// Implementation for Mean Imputation 		
		for(i <- 0 to (num_cols - 1)){
			val selected = dataset.sigmaS(column_names(i), (x) => x!="")	// Filtering rows which have a missing value, as a no entry, i.e, ""
			val v_selected = selected.toVectorS(column_names(i))			// Converting remaining elements in column into a vector
			val v_seld = v_selected.map((x) => x.toDouble)					// Converting each element in filtered column to Double data type 
			val mean_col = (v_seld.sum) / selected.count(column_names(i))	// Computing mean of filtered column elements
			dataset.update(column_names(i), mean_col.toString(), "") 		// Updating blank spaces with mean of column
		} 
		
		// Giving user choice to execute regression model of their choice
		println("-"*75)
		println ("Select model:\n\t 1. Simple Regression \n\t 2. Regression WLS \n\t 3. Quadratic Regression \n\t 4. Ridge Regression \n\t 5. Lasso Regression \n\t 6. Response Surface")
		println("-"*75)
		
		val model = scala.io.StdIn.readLine()
		if (model == "1") {
			val (x_initial, y) = dataset.toMatriDD(1 until num_cols, 0)	// Y vector is the first column of Relation
			val x = VectorD.one (x_initial.dim1) +^: x_initial	// Appending 1 column to x
			regression_sim(x, y)	// Implementing Simple Regression Model when user's choice is '1'
		} else if (model == "2") {
			val (x_initial, y) = dataset.toMatriDD(1 until num_cols, 0) // Y vector is the first column of Relation
			val x = VectorD.one (x_initial.dim1) +^: x_initial	// Appending 1 column to x
			regression_WLS(x, y)	// Implementing Regression_WLS Model when user's choice is '1'
		} else if (model == "3") {
			val (x, y) = dataset.toMatriDD(1 until num_cols, 0)	// Y vector is the first column of Relation
			quad_regression(x, y)	// Implementing Quad Regression Model when user's choice is '1'
		} else if (model == "4") {
			val (x_initial, y_initial) = dataset.toMatriDD(1 until num_cols, 0)	// Y vector is the first column of Relation
			val mean_vector = x_initial.mean	// Mean vector containing means of all columns of x
			val y = y_initial - y_initial.mean	//	Mean value of y vector
			val x = x_initial - mean_vector	// Performing mean-centering as it is required for Ridge Regression
			ridge_regression(x, y)	// Implementing Ridge Regression Model when user's choice is '1'
		} else if (model == "5") {
			val (x_initial, y) = dataset.toMatriDD(1 until num_cols, 0)	// Y vector is the first column of Relation
			val x = VectorD.one (x_initial.dim1) +^: x_initial	// Appending 1 column to x
			lasso_regression(x, y)	// Implementing Simple Regression Model when user's choice is '1'
		} else if (model == "6") {
			val (x, y) = dataset.toMatriDD(1 until num_cols, 0)	// Y vector is the first column of Relation
			response_surface(x, y)	// Implementing Simple Regression Model when user's choice is '1'
		}
	}

	main()
}
