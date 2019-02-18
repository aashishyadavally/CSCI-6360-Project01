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
			
object autompg extends App {
	def regression_sim (x: MatriD, y: VectorD)
	{
		banner ("Implementing Simple Regression... ")
		val rg_sim = new Regression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD (x.dim2)
		val RSqAdj = new VectorD (x.dim2) 
		val RSqCV = new VectorD(x.dim2)
		val n = VectorD.range(0, x.dim2 - 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_sim.forwardSel(fs_cols, true)
			fs_cols_adj  += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_sim.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal(j) = new_qof(0)
			val x_cv = x.selectCols(fs_cols.toArray)
			val rg_cv = new Regression(x_cv, y)
			val crossval_array = rg_cv.crossVal()
			val RSq_cv = crossval_array(rg_cv.index_rSq)
			RSqCV(j) = RSq_cv.mean
		}
		val plot_mat = new MatrixD (3, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		plot_mat.update(2, RSqCV)
		new PlotM(n, plot_mat, lines=true)
		banner ("Successfully implemented Simple Regression!")
	}
	
	
	def regression_WLS (x: MatriD, y: VectorD)
	{
		banner ("Implementing Regression WLS... ")
		val rg_WLS = new Regression_WLS (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD (x.dim2)
		val RSqAdj = new VectorD (x.dim2) 
		val n = VectorD.range(0, x.dim2 - 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_WLS.forwardSel(fs_cols, true)
			fs_cols_adj  += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_WLS.forwardSel(fs_cols, false)
			fs_cols += add_var	
			RSqNormal(j) = new_qof(0)
		}
		val plot_mat = new MatrixD (2, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Regression WLS!")
	}
	
	def ridge_regression (x: MatriD, y: VectorD)
	{
		banner ("Implementing Ridge Regression... ")
		val rg_rid = new RidgeRegression (x, y)
		val fs_cols = Set.empty[Int]
		val fs_cols_adj = Set.empty[Int]
		val RSqNormal = new VectorD(x.dim2)
		val RSqAdj = new VectorD(x.dim2)
		val n = VectorD.range(0, x.dim2)
		
		for (j <- 0 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_rid.forwardSel(fs_cols, true)
			println(fs_cols_adj)
			fs_cols_adj += add_var_adj
			println(fs_cols_adj)
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_rid.forwardSel(fs_cols, false)
			println("Hello False")
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
		}
		val plot_mat = new MatrixD(2, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Ridge Regression!")
	}
	
	def quad_regression (x: MatriD, y: VectorD)
	{
		banner ("Implementing Quadratic Regression... ")
		val rg_quad = new QuadRegression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val num_terms = QuadRegression.numTerms(x.dim2)
		val RSqNormal = new VectorD(num_terms)
		val RSqAdj = new VectorD(num_terms)
		val n = VectorD.range(0, num_terms)
		
		for (j <- 0 until (2*x.dim2 + 1)){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_quad.forwardSel(fs_cols, true)
			fs_cols_adj += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_quad.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
		}
		val plot_mat = new MatrixD(2, num_terms)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)

		banner ("Successfully implemented Quadratic Regression!")		
	}
	
	def lasso_regression(x: MatriD, y: VectorD)
	{
		banner ("Implementing Lasso Regression...")
		val rg_lasso = new LassoRegression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD(x.dim2)
		val RSqAdj = new VectorD(x.dim2)
		val n = VectorD.range(0, x.dim2 - 1)
		
		for (j <- 0 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_lasso.forwardSel(fs_cols, true)
			fs_cols_adj += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_lasso.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
		}
		val plot_mat = new MatrixD(2, x.dim2)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Lasso Regression!")
	}
	
	def response_surface (x: MatriD, y: VectorD)
	{
		banner ("Implementing Response Surface... ")
		val rg_rs = new ResponseSurface (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val num_terms = ResponseSurface.numTerms(x.dim2)
		val RSqNormal = new VectorD(num_terms)
		val RSqAdj = new VectorD(num_terms)
		val n = VectorD.range(0, num_terms)
		
		for (j <- 0 until num_terms){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_rs.forwardSel(fs_cols, true)
			fs_cols_adj += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_rs.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof (0)
		}
		val plot_mat = new MatrixD(2, num_terms)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Response Surface!") 
	}
	
	def main(){
		println("-"*75)
		println (" Select dataset: \n\t 1. Auto MPG \n\t 2. Lorem Ipsum \n\t 11. For other datasets, enter: /correct/path/to/data/csv")
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
