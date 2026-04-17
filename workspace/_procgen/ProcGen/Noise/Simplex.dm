
const _grad = new 1d[
	1, 1, -1, 1, 1, -1,
	-1, -1, 1, 0, -1, 0,
	1, 0, -1, 0, 0, 1,
	0, -1, 0, 1, 0, -1
]

struct Simplex{
	PermTable permTable
}

fn newSimplex(PRNG prng) Simplex
	return new Simplex{
		permTable = newPermTable(prng)
	}
end

fn gradX(Simplex s, Int i) Int
	var v = s.permTable.data[i % 256]
	return _grad[(v % 12) * 2]
end

fn gradY(Simplex s, Int i) Int
	var v = s.permTable.data[i % 256]
	return _grad[(v % 12) * 2 + 1]
end

fn noise(Simplex s, Float fx, Float fy) Float
	-- initial skew
	-- (sqrt(3) - 1) / 2
	var _skew = 0.366025
	var fSkew = (fx + fy) * _skew
	var i = floor(fx + fSkew)
	var j = floor(fy + fSkew)
	-- unskew
	var _unskew = 0.21132
	var fRev = (i + j) * _unskew
	var a0 = fx - (i - fRev)
	var b0 = fy - (j - fRev)
	-- we are either in simplex [(0, 0) > (1, 0) > (1, 1)] OR [(0, 0) > (0, 1) > (1, 1)]
	-- this calculates the third coordinate
	var a2 = (a0 - 1.0) + (2.0 * _unskew)
	var b2 = (b0 - 1.0) + (2.0 * _unskew)
	-- this calculates the second coordinate
	var simX = 1
	var simY = 0
	if a0 < b0 then simX = 0 simY = 1 end
	var a1 = (a0 - toFloat(simX)) + _unskew
	var b1 = (b0 - toFloat(simY)) + _unskew
	-- calculate index into hash
	var ix = toInt(i) % 256
	var jx = toInt(j) % 256
	-- calculate a "contribution" from corner 1
	var d0 = (0.5 - a0 * a0) - b0 * b0
	var n0 = 0.0
	if d0 >= 0.0 then
		var gx = ix + s.permTable.data[jx]
		var dsq = d0 * d0
		n0 = dsq * dsq * (toFloat(s.gradX(gx)) * a0 + toFloat(s.gradY(gx)) * b0)
	end
	-- corner 2
	var d1 = (0.5 - a1 * a1) - b1 * b1
	var n1 = 0.0
	if d1 >= 0.0 then
		var gx = ix + simX + s.permTable.data[jx + simY]
		var dsq = d1 * d1
		n1 = dsq * dsq * (toFloat(s.gradX(gx)) * a1 + toFloat(s.gradY(gx)) * b1)
	end
	-- corner 3
	var d2 = (0.5 - a2 * a2) - b2 * b2
	var n2 = 0.0
	if d2 >= 0.0 then
		var gx = ix + 1 + s.permTable.data[jx + 1]
		var dsq = d2 * d2
		n2 = dsq * dsq * (toFloat(s.gradX(gx)) * a2 + toFloat(s.gradY(gx)) * b2)
	end
	-- final result
	return 70.0 * (n0 + n1 + n2)
end

fn addSimplex(DataLayer layer, Simplex s, Float weight, Float resX, Float resY, Float offset)
	for i < layer.width, j < layer.height do
		var raw = s.noise(offset + toFloat(i) * resX, offset + toFloat(j) * resY)
		layer.data[i, j] = layer.data[i, j] + (raw + 1.0) * 0.5 * weight
	end
end