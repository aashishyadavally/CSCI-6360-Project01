/*Code for AUTO_MPG dataset */

package package_autompg
import scalation.columnar_db.Relation
import scalation.columnar_db.ImputeMean
import scalation.util.banner
import scala.collection.mutable.Set
import scalation.linalgebra._
import scalation.analytics._
import scalation.plot.PlotM


object autompg extends App {
	def regression_sim (x: MatriD, y: VectorD)
	{
		banner ("Implementing Simple Regression... ")
		val rg_sim = new Regression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD (7)
		val RSqAdj = new VectorD (7) 
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_sim.forwardSel(fs_cols, true)
			fs_cols_adj  += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_sim.forwardSel(fs_cols, false)
			fs_cols += add_var	
			RSqNormal(j) = new_qof(0)
		}
		val plot_mat = new MatrixD (2, 7)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Simple Regression!")
	}
	
	
	def regression_WLS (x: MatriD, y: VectorD)
	{
		banner ("Implementing Regression WLS... ")
		val rg_WLS = new Regression_WLS (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD (7)
		val RSqAdj = new VectorD (7) 
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_WLS.forwardSel(fs_cols, true)
			fs_cols_adj  += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_WLS.forwardSel(fs_cols, false)
			fs_cols += add_var	
			RSqNormal(j) = new_qof(0)
		}
		val plot_mat = new MatrixD (2, 7)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Regression WLS!")
	}
	
	def ridge_regression (x: MatriD, y: VectorD)
	{
		banner ("Implementing Ridge Regression... ")
		val rg_rid = new RidgeRegression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal = new VectorD(7)
		val RSqAdj = new VectorD(7)
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_rid.forwardSel(fs_cols, true)
			fs_cols_adj += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_rid.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof(0)
		}
		val plot_mat = new MatrixD(2, 7)
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
		val RSqNormal = new VectorD(7)
		val RSqAdj = new VectorD(7)
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_quad.forwardSel(fs_cols, true)
			fs_cols_adj += add_var_adj
			RSqAdj(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_quad.forwardSel(fs_cols, false)
			fs_cols += add_var
			RSqNormal (j) = new_qof(0)
		}
		val plot_mat = new MatrixD(2, 7)
		plot_mat.update(0, RSqAdj)
		plot_mat.update(1, RSqNormal)
		new PlotM(n, plot_mat)
		banner ("Successfully implemented Quadratic Regression!")		
	}
	
	def main(){
		banner (" Select dataset: \n\t1. Auto MPG \n\t2. Lorem Ipsum \n\t11. Enter: /correct/path/to/data/csv")
		val choice	 = scala.io.StdIn.readLine()
		val folder = "..\\CSCI-6360-Project01\\data\\"
/*		Solve bug for choice = 11			*/
		val filename = if(choice != "11") {folder + choice + ".csv"} else {scala.io.StdIn.readLine()}
		val dataset = Relation (filename, "dataset", null, -1)
		val column_names = dataset.colNames
		val num_cols = dataset.cols
		/*
		for(i <- 0 to (num_cols - 1)){
			val val_mean = dataset.toVectorD(column_names(i)).mean
			println(val_mean)
			dataset.update(column_names(i), val_mean, "?") 
		}
		*/
		banner ("Extracting X matrix and Y vector: ")
		val (x_initial, y) = dataset.toMatriDD(1 to 6, 0)
		val x = VectorD.one (x_initial.dim1) +^: x_initial
		quad_regression (x, y)
	}

	main()
}
