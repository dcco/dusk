references Sys modules Sulfur, Os

	(*
		builtin meshes
	*)

fn meshBox(Int w, Int h, Int d) Mesh
	var fw = toFloat(w) /. 8.0
	var fh = toFloat(h) /. 8.0
	var fd = toFloat(d) /. 8.0
	var m = newMesh()
	m.addQuadZ(0.0, 0.0, fw, fh, 0.0)
	return m
end

globals GMesh{
	dice = meshBox(3, 3, 3)
}

	(*
		pipeline API (frontend). functionally the same as regular code,
			but packaged with the main pipeline for modularity.
	*)

fn lightMat() Mat4
	var lightPos = (-16.0, -30.0, -12.0)
	var targetPos = (0.0, 0.0, -10.0)
	var lMat = newMat4()
	lookAt(lMat, lightPos, targetPos, (0.0, 1.0, 0.0))
	return lMat
end

fn lerp(Float a, Float b, Float r) Float
	return a + (r * (b - a))
end

fn ssaoSamples() 1d[Float]
	var samples = new 1d(64 * 3)[.. 0.0]
	for i < 64 do
		var v = (randomFloat() * 2.0 - 1.0, randomFloat() * 2.0 - 1.0, randomFloat())
		var r = i /. 64
		v.normalize()
		v.scale(lerp(0.1, 1.0, r * r))
		samples[i * 3] = v.1
		samples[i * 3 + 1] = v.2
		samples[i * 3 + 2] = v.3
	end
	return samples
end

globals RV{
	mvMat = newMat4(),
	lightMat = lightMat(),
	samples = ssaoSamples()
}

	(*
		sulfur hooks
	*)

fn passRenderVars()
	var rd = Sulfur.renderData()
	rd.set(0, GLMat4(RV.mvMat))
	rd.set(1, GLMat4(RV.lightMat))
	rd.set(2, GLFloatVec3(RV.samples))
end

fn resetFloorSkew()
	Sulfur.setAttr(0, GLFloat4(0.0, 0.0, 0.0, 0.0))
end

fn setFloorSkew(Float f1, Float f2, Float f3, Float f4)
	Sulfur.setAttr(0, GLFloat4(f1, f2, f3, f4))
end

fn setSPFlag(Float f)
	Sulfur.setAttr(1, GLFloat(f))
end