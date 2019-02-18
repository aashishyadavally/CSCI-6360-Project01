/*Code for AUTO_MPG dataset */

package package_autompg
import scalation.columnar_db.Relation
import scalation.util.banner
import scala.collection.mutable.Set
import scalation.linalgebra._
import scalation.analytics._
import scalation.plot.PlotM


class IllegalChoiceException(s: String) extends Exception(s){}

class Exception1{
	@throws(classOf[IllegalChoiceException])
	def validate(choice: Int){
		if((choice < 0) || (choice > 11)) {
			throw new IllegalChoiceException("Invalid Choice.")
		}
	}
}

class cross_validation(x: MatriD, y: VectorD) {
	val m = x.dim1
	val k = 10
	val indices = VectorI(0 until m).split(k)
	val rSq_vector = new VectorD (10)
	var ctr = 0
	def simple_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new Regression(XTr, YTr)
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble
			ctr += 1
		}
		return rSq_vector.mean
	}
	def regression_wls_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new Regression_WLS(XTr, YTr)
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble
			ctr += 1
		}
		return rSq_vector.mean
	}
	def ridge_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new RidgeRegression(XTr, YTr)
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble
			ctr += 1
		}
		return rSq_vector.mean
	}
	def quad_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new QuadRegression(XTr, YTr)
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble
			ctr += 1
		}
		return rSq_vector.mean
	}
	def lasso_regression_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new LassoRegression(XTr, YTr)
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble
			ctr += 1
		}
		return rSq_vector.mean
	}
	def response_surface_cv(): Double = {
		for (idx <- indices) {
			val idx_array = idx.toArray
			val XTe = x(idx)
			val YTe = y(idx)
			val XTr = x.selectRowsEx(idx_array)
			val YTr = y.selectEx(idx_array)
			val rg = new Regression(XTr, YTr)
			rg.train().eval(XTe, YTe)
			rSq_vector (ctr) = rg.fitMap.get("rSq").get.toDouble
			ctr += 1
		}
		return rSq_vector.mean
	}
}	
			
object autompg extends App {
	def regression_sim (x: MatriD, y: VectorD)
	{
		banner ("Implementing Simple Regression... ")
		val rg_sim = new Regression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD (x.dim2)
		val RSqAdj = new VectorD (x.dim2) 
		val RSqCV = new VectorD (x.dim2)
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_sim.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal(j) = new_qof(0)
			RSqAdj(j) = new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = cv.simple_regression_cv()
		}
		val plot_mat = new MatrixD (3, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).setTitle("Simple Regression : Comparison of R-Squared, RBar-Squared, RCV-Squared")
		banner ("Successfully implemented Simple Regression!")
	}
	
	
	def regression_WLS (x: MatriD, y: VectorD)
	{
		banner ("Implementing Regression WLS... ")
		val rg_WLS = new Regression_WLS (x, y)
		val fs_cols = Set(0)
		val RSqNormal = new VectorD (x.dim2)
		val RSqAdj = new VectorD (x.dim2) 
		val RSqCV = new VectorD (x.dim2)
		val n = VectorD.range(0, x.dim2 - 1)
			println("*"*50)
			println(x.dim2)
			println("*"*50)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_WLS.forwardSel(fs_cols, false)
			fs_cols += add_var	
			RSqNormal(j) = new_qof(0)
			RSqAdj (j) = new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = cv.regression_wls_cv()
		}
		val plot_mat = new MatrixD (3, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).setTitle("Regression_WLS : Comparison of R-Squared, RBar-Squared, RCV-Squared")
		banner ("Successfully implemented Regression WLS!")
	}
	
	def ridge_regression (x: MatriD, y: VectorD)
	{
		banner ("Implementing Ridge Regression... ")
		val rg_rid = new RidgeRegression (x, y)
		val fs_cols = Set.empty[Int]
		val RSqNormal = new VectorD(x.dim2)
		val RSqAdj = new VectorD(x.dim2)
		val RSqCV = new VectorD(x.dim2)
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_rid.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
			RSqAdj (j) = new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = cv.ridge_regression_cv()
		}
		val plot_mat = new MatrixD(3, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).setTitle("Ridge Regression : Comparison of R-Squared, RBar-Squared, RCV-Squared")
		banner ("Successfully implemented Ridge Regression!")
	}
	
	def quad_regression (x_initial: MatriD, y: VectorD)
	{
		banner ("Implementing Quadratic Regression... ")
		val rg_quad = new QuadRegression (x_initial, y)
		val x = rg_quad.getX
		val fs_cols = Set.empty[Int]
		val num_terms = x.dim2
		val RSqNormal = new VectorD(num_terms)
		val RSqAdj = new VectorD(num_terms)
		val RSqCV = new VectorD(num_terms)
		val n = VectorD.range(1, num_terms)
		
		for (j <- 1 until num_terms){
			val (add_var, new_param, new_qof) = rg_quad.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
			RSqAdj (j) = new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = cv.quad_regression_cv()
		}
		val plot_mat = new MatrixD(3, num_terms)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).setTitle("Quad Regression : Comparison of R-Squared, RBar-Squared, RCV-Squared")

		banner ("Successfully implemented Quadratic Regression!")		
	}
	
	def lasso_regression(x: MatriD, y: VectorD)
	{
		banner ("Implementing Lasso Regression...")
		val rg_lasso = new LassoRegression (x, y)
		//val fs_cols = Set.empty[Int]
		val fs_cols = Set(0)
		val RSqNormal = new VectorD(x.dim2)
		val RSqAdj = new VectorD(x.dim2)
		val RSqCV = new VectorD(x.dim2)
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var, new_param, new_qof) = rg_lasso.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
			RSqAdj (j) = new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = cv.lasso_regression_cv()
		}
		val plot_mat = new MatrixD(3, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).setTitle("Lasso Regression : Comparison of R-Squared, RBar-Squared, RCV-Squared")
		banner ("Successfully implemented Lasso Regression!")
	}
	
	def response_surface (x_initial: MatriD, y: VectorD)
	{
		banner ("Implementing Response Surface... ")
		val rg_rs = new ResponseSurface (x_initial, y)
		val x = rg_rs.getX
		val fs_cols = Set.empty[Int]
		val num_terms = x.dim2
		val RSqNormal = new VectorD(num_terms)
		val RSqAdj = new VectorD(num_terms)
		val RSqCV = new VectorD(num_terms)
		val n = VectorD.range(0, num_terms)
		
		for (j <- 1 until num_terms){
			val (add_var, new_param, new_qof) = rg_rs.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
			RSqAdj (j) = new_qof(7)
			val x_cv = x.selectCols(fs_cols.toArray)
			println(x_cv.dim1, x_cv.dim2)
			val cv = new cross_validation(x_cv, y)
			RSqCV(j) = cv.response_surface_cv()	
		}
		val plot_mat = new MatrixD(3, num_terms)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true).setTitle("Response Surface : Comparison of R-Squared, RBar-Squared, RCV-Squared")
		banner ("Successfully implemented Response Surface!") 
	}
	
	def main(){
		println("-"*75)
		println (" Select dataset: \n\t 1. Auto MPG \n\t 2. Lorem Ipsum \n\t 3. Concrete Compressive Strength Dataset \n\t 4. Concrete Slump Dataset \n\t 5. Forest Fires \n\t 6. \n\t 7. \n\t 8. \n\t 9. \n\ 10. \n\t 11. For other datasets, enter: /correct/path/to/data/csv")
		println("-"*75)
		
		val choice	 = scala.io.StdIn.readLine()
		var e = new Exception1()
		try {
			e.validate(choice.toInt)
		} catch {
			case ex: Exception => println("Exception Occured : " + ex)
		}
						
		val filename = if(choice != "11") {choice + ".csv"} else {scala.io.StdIn.readLine()}
		val dataset = Relation (filename, "dataset", null, -1)
		val column_names = dataset.colNames
		val num_cols = dataset.cols

		// Mean Imputation 		
		for(i <- 0 to (num_cols - 1)){
			val selected = dataset.sigmaS(column_names(i), (x) => x!="")
			val v_selected = selected.toVectorS(column_names(i))
			val v_seld = v_selected.map((x) => x.toDouble)
			val mean_col = (v_seld.sum) / selected.count(column_names(i))
			dataset.update(column_names(i), mean_col.toString(), "") 
		} 
		
		println("-"*75)
		println ("Select model:\n\t 1. Simple Regression \n\t 2. Regression WLS \n\t 3. Quadratic Regression \n\t 4. Ridge Regression \n\t 5. Lasso Regression \n\t 6. Response Surface")
		println("-"*75)
		
		val model = scala.io.StdIn.readLine()
		if (model == "1") {
			val (x_initial, y) = dataset.toMatriDD(1 until num_cols, 0)
			val x = VectorD.one (x_initial.dim1) +^: x_initial
			regression_sim(x, y)
		} else if (model == "2") {
			val (x_initial, y) = dataset.toMatriDD(1 until num_cols, 0)
			val x = VectorD.one (x_initial.dim1) +^: x_initial
			regression_WLS(x, y)
		} else if (model == "3") {
			val (x, y) = dataset.toMatriDD(1 until num_cols, 0)
			quad_regression(x, y)
		} else if (model == "4") {
			val (x_initial, y_initial) = dataset.toMatriDD(1 until num_cols, 0)
			val mean_vector = x_initial.mean
			val y = y_initial - y_initial.mean
			val x = x_initial - mean_vector
			ridge_regression(x, y)
		} else if (model == "5") {
			val (x_initial, y) = dataset.toMatriDD(1 until num_cols, 0)
			val x = VectorD.one (x_initial.dim1) +^: x_initial
			lasso_regression(x, y)
		} else if (model == "6") {
			val (x, y) = dataset.toMatriDD(1 until num_cols, 0)
			response_surface(x, y)
		}
	}

	main()
}
