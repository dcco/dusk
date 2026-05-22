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

fn noiseTex() FixImage
	var noiseList = new 1d(16 * 4)[.. 0.0]
	for i < 16 do
		noiseList[i * 4] = (randomFloat() * 2.0) - 1.0
		noiseList[i * 4 + 1] = (randomFloat() * 2.0) - 1.0
		noiseList[i * 4 + 2] = 0.0
		noiseList[i * 4 + 3] = 1.0
	end
	return fixedTexImageFloat(4, 4, noiseList)
end

fn initAttrVars()
	Sulfur.setAttr(0, GLFloat4(0.0, 0.0, 0.0, 0.0))
end

globals Pipeline in RenderThread{
	shadow = newFrameBuffer(cLoad("shadow.vs"), cLoad("shadow.fs"), 4096, 4096, 1v[FBODepth],
		1v[(@GLFloat, 4)],
		"uPMat", 1v[("uMVMat", @GLMat4, 0), ("uLightMat", @GLMat4, 0)], 1v["uSampler"]),

	geometry = newFrameBuffer(cLoad("geometry.vs"), cLoad("geometry.fs"), 960, 640,
		1v[FBOColor, FBOColor, FBOColor, FBOColor, FBORender],
		1v[(@GLFloat, 4)],
		"uPMat", 1v[("uMVMat", @GLMat4, 0), ("uLightMat", @GLMat4, 0)], 1v["uSampler", "uShadowMap"]),

	ssao = newFrameBuffer(cLoad("ssao.vs"), cLoad("ssao.fs"), 960, 640, 1v[FBOColor],
		1v[], "uPMat", 1v[("samples", @GLFloatVec3, 0)], 1v["null", "gPos", "gNorm", "texNoise"]),

	(*blur = newFrameBuffer(cLoad("blur.vs"), cLoad("blur.fs"), 960, 640, 1v[FBOColor],
		"null", 1v[], 1v["null", "gColor"]),*)

	light = newShader(cLoad("light.vs"), cLoad("light.fs"),
		1v[], "null", 1v[], 1v["null", "gPos", "gNorm", "gColor", "gSpec", "occlusion"]),

	noiseTex = noiseTex()
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
	-- ssao render
	Pipeline.ssao.setUniform(0, rd.get(2))
	Pipeline.ssao.loadTexture(1, Pipeline.geometry, 0)
	Pipeline.ssao.loadTexture(2, Pipeline.geometry, 1)
	Pipeline.ssao.loadTextureLit(3, Pipeline.noiseTex)
	Pipeline.ssao.renderQuad()
	-- ssao blur render
	(*Pipeline.blur.loadTexture(1, Pipeline.ssao, 0)
	Pipeline.blur.renderQuad()*)
	-- ending render
	Pipeline.light.loadTexture(1, Pipeline.geometry, 0)
	Pipeline.light.loadTexture(2, Pipeline.geometry, 1)
	Pipeline.light.loadTexture(3, Pipeline.geometry, 2)
	Pipeline.light.loadTexture(4, Pipeline.geometry, 3)
	Pipeline.light.loadTexture(5, Pipeline.ssao, 0)
	Pipeline.light.renderQuad()
end


(*
globals Pipeline in RenderThread{
	shadow = newShader(cLoad("shadow.vs"), cLoad("shadow.fs"), 1v[],
		1v[("uMVMat", GLMat4, 0), ("uLightMat", GLMat4, 0)], 1v[])
}

fn runShader(RenderData rd)
	-- shadow render
	Pipeline.shadow.setUniform(0, rd.get(0))
	Pipeline.shadow.setUniform(1, rd.get(1))
	Pipeline.shadow.render(rd)
end*)