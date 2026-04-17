
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