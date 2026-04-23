
struct DataLayer{
	Int width,
	Int height,
	2d[Float] data
}

fn newDataLayer(Int w, Int h) DataLayer
	var data = new 2d(w, h)[.. 0.0]
	return new DataLayer{
		width = w,
		height = h,
		data = data
	}
end

fn compLayerMul(DataLayer self, DataLayer other)
	for i < self.width, j < self.height do
		self.data[i, j] = self.data[i, j] * other.data[i, j]
	end
end

fn compLayerMin(DataLayer self, DataLayer other)
	for i < self.width, j < self.height do
		var v = other.data[i, j]
		if v < self.data[i, j] then self.data[i, j] = v end
	end
end

fn fillCircle(DataLayer self, Float cx, Float cy, Float radius, Float falloff, Bool invert)
	for i < self.width, j < self.height do
		var dist = distPt(cx, cy, toFloat(i), toFloat(j))
		var ratio = dist /. radius
		if ratio > 1.0 then ratio = 1.0 end
		if invert then ratio = 1.0 - ratio end
		self.data[i, j] = ratio ** falloff
	end
end