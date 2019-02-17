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
	banner ("Reading Autompg dataset as Relation: ")
	val mpg = Relation ("C:\\Users\\aashi\\scalation_autompg\\data\\auto-mpgg.csv", "mpg", null, -1)
	val column_names = mpg.colNames
	val num_cols = mpg.cols
	/*
	for(i <- 0 to (num_cols - 1)){
		val val_mean = mpg.toVectorD(column_names(i)).mean
		println(val_mean)
		mpg.update(column_names(i), val_mean, "?") 
	}
	*/
	banner ("Extracting X matrix and Y vector: ")
	val (x_initial, y) = mpg.toMatriDD(1 to 6, 0)
	val x = VectorD.one (x_initial.dim1) +^: x_initial
	

	def regression_sim (x: MatrixD, y: VectorD)
	{
		banner ("Implementing Simple Regression: ")
		val rg_sim = new Regression (x, y)
		val fs_cols = Set(0)
		val fs_cols_adj = Set(0)
		val RSqNormal_sim = new VectorD (7)
		val RSqAdj_sim = new VectorD (7) 
		val n = VectorD.range(1, x.dim2 + 1)
		
		for (j <- 1 until x.dim2){
			val (add_var_adj, new_param_adj, new_qof_adj) = rg_sim.forwardSel(fs_cols, true)
			fs_cols_adj  += add_var_adj
			RSqAdj_sim(j) = new_qof_adj (0)
			
			val (add_var, new_param, new_qof) = rg_sim.forwardSel(fs_cols, false)
			fs_cols += add_var	
			RSqNormal_sim(j) = new_qof(0)
		}
		val plot_mat_sim = new MatrixD (2, 7)
		plot_mat_sim.update(0, RSqAdj_sim)
		plot_mat_sim.update(1, RSqNormal_sim)
		new PlotM(n, plot_mat_sim)
	}
	
}
