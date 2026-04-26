references Sys modules Sulfur

	(*
		main pipeline code. doesn't function like regular code.
			in particular, it cannot dynamically allocate memory (no access to the GC)

		TODO:
		- implement safe shader memory init (both halves)
		- enforce no GC memory used in shader (thread-safety)
		- enforce backend not using external memory at all (thread-safety)
	*)

	(*
		the pipeline backend globals require the RenderThread to be initialized.
		- the special "1v" array type (dynamically size tuple, treated as a value)
	*)

globals Pipeline in RenderThread{
	light = newShader(cLoad("light.vs"), cLoad("light.fs"), 1v[0], 1v[("uMVMat", GLMat4, 1)])
}

fn runShader(RenderData rd)
	Pipeline.light.setUniform(0, rd.get(0))
	Pipeline.light.render(rd)
end