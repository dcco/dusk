
fn asRatio(Float v, Float n) Float
	if v > 1.0 then v = 1.0 end
	return (1.0 - v) ** n
end

fn dist(Float x, Float y) Float
	return sqrt(x * x + y * y)
end

fn distPt(Float x1, Float y1, Float x2, Float y2) Float
	var xx = abs(x1 - x2)
	var yy = abs(y1 - y2)
	return sqrt(xx * xx + yy * yy)
end

fn distPt3(Float x1, Float y1, Float z1, Float x2, Float y2, Float z2) Float
	var xx = abs(x1 - x2)
	var yy = abs(y1 - y2)
	var zz = abs(z1 - z2)
	return sqrt(xx * xx + yy * yy + zz * zz)
end

fn max(Int i1, Int i2) Int
	if i1 > i2 then return i1 end
	return i2
end

fn min(Int i1, Int i2) Int
	if i1 < i2 then return i1 end
	return i2
end

fn max(Float f1, Float f2) Float
	if f1 > f2 then return f1 end
	return f2
end

fn min(Float f1, Float f2) Float
	if f1 < f2 then return f1 end
	return f2
end
