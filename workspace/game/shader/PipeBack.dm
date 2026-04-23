references Sys modules Sulfur

global lightShader = nullShader()

fn initShaderBack()
	lightShader = newShader(cLoad("light.vs"), cLoad("light.fs"), new 1d[~Int], new 1d[("uMVMat", GLMat4, 1)])
end

	(*
		TODO:
			- implement safe shader memory init (both halves)
			- enforce no GC memory used in shader (thread-safety)
			- enforce backend not using external memory at all (thread-safety)
	*)

fn runShader(RenderData rd)
	lightShader.setUniform(0, rd.get(0))
	lightShader.render(rd)
end