references Sys modules Sulfur

	(*
		the pipeline frontend contains memory, and its memory may be
		 referenced from the rest of the code
	*)

global mvMat = nullMat4()

fn initShaderFront()
	mvMat = newMat4()
	mvMat.rotateX(toRadians(-30))
	mvMat.translate(-0.5, 4.5, -8.5)
end

fn updateCamMat(Int angle, Float x, Float y, Float z)
	idMat4(mvMat)
	mvMat.rotateX(toRadians(angle))
	mvMat.translate(x, y, z)
end

fn passRenderVars(RenderData rd)
	rd.set(0, GLMat4V(mvMat))
end