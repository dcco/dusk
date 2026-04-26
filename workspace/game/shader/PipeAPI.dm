references Sys modules Sulfur

	(*
		pipeline API (frontend). functionally the same as regular code,
			but packaged with the main pipeline for modularity.
	*)

globals RV{
	mvMat = newMat4()
}

	(*
		sulfur hooks
	*)

fn passRenderVars()
	var rd = Sulfur.renderData()
	rd.set(0, GLMat4V(RV.mvMat))
end