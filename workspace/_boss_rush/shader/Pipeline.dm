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
	shadow = newFrameBuffer(cLoad("shadow.vs"), cLoad("shadow.fs"), 4096, 4096, 1v[FBODepth],
		"uPMat", 1v[("uMVMat", @GLMat4, 0), ("uLightMat", @GLMat4, 0)], 1v["uSampler"]),

	geometry = newFrameBuffer(cLoad("geometry.vs"), cLoad("geometry.fs"), 960, 640,
		1v[FBOColor, FBOColor, FBOColor, FBOColor, FBORender],
		"uPMat", 1v[("uMVMat", @GLMat4, 0), ("uLightMat", @GLMat4, 0)], 1v["uSampler", "uShadowMap"]),

	light = newShader(cLoad("light.vs"), cLoad("light.fs"), 1v[0],
		"null", 1v[], 1v["null", "gPos", "gNorm", "gColor", "gSpec"])
}

fn runShader(RenderData rd)
	-- shadow render
	Pipeline.shadow.setUniform(0, rd.get(0))
	Pipeline.shadow.setUniform(1, rd.get(1))
	Pipeline.shadow.render(rd)
	-- geometry render
	Pipeline.geometry.setUniform(0, rd.get(0))
	Pipeline.geometry.setUniform(1, rd.get(1))
	Pipeline.geometry.loadTexture(1, Pipeline.shadow, 0)
	Pipeline.geometry.render(rd)
	-- ending render
	Pipeline.light.loadTexture(1, Pipeline.geometry, 0)
	Pipeline.light.loadTexture(2, Pipeline.geometry, 1)
	Pipeline.light.loadTexture(3, Pipeline.geometry, 2)
	Pipeline.light.loadTexture(4, Pipeline.geometry, 3)
	Pipeline.light.renderQuad()
end
