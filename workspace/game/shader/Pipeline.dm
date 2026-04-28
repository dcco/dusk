references Sys modules Sulfur, Os

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
	shadow = newFrameBuffer(cLoad("shadow.vs"), cLoad("shadow.fs"), 2048, 2048, 1v[FBODepth],
		1v[("uMVMat", GLMat4, 0), ("uLightMat", GLMat4, 0)]),
	light = newShader(cLoad("light.vs"), cLoad("light.fs"), 1v[0],
		1v[("uMVMat", GLMat4, 0), ("uLightMat", GLMat4, 0)], 1v["uShadowMap"])
}

fn runShader(RenderData rd)
	-- shadow render
	Pipeline.shadow.setUniform(0, rd.get(0))
	Pipeline.shadow.setUniform(1, rd.get(1))
	Pipeline.shadow.render(rd)
	-- ending render
	Pipeline.light.setUniform(0, rd.get(0))
	Pipeline.light.setUniform(1, rd.get(1))
	Pipeline.light.loadTexture(0, Pipeline.shadow, 0)
	Pipeline.light.render(rd)
end