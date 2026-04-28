references Sys modules Sulfur, Os

	(*
		pipeline API (frontend). functionally the same as regular code,
			but packaged with the main pipeline for modularity.
	*)

fn lightMat() Mat4
	var lightPos = (-16.0, -30.0, -30.0)
	var targetPos = (0.0, 0.0, -10.0)
	var lMat = newMat4()
	lookAt(lMat, lightPos, targetPos, (0.0, 1.0, 0.0))
	return lMat
end

globals RV{
	mvMat = newMat4(),
	lightMat = lightMat()
}

	(*
		sulfur hooks
	*)

fn passRenderVars()
	var rd = Sulfur.renderData()
	rd.set(0, GLMat4V(RV.mvMat))
	rd.set(1, GLMat4V(RV.lightMat))
end