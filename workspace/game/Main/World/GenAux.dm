
struct GenAuxInfo{
	PRNG prng,
	Int size,
	Int holeFX,
	Int holeFY,
	1d[Int] colorTotals,
	Int mainColor
}

struct AuxTile{
	Int color
}

	(*
		generation of auxiliary map
	*)

fn _colorMap(2d[GameTile] grid, 2d[AuxTile] aux, Int sx, Int sy, Int color) Int
	var xList = newIntQueue()
	var yList = newIntQueue()
	xList.add(sx)
	yList.add(sy)
	var total = 0
	while !xList.empty() do
		var i = xList.remove()
		var j = yList.remove()
		if grid[i, j].canWalk() && aux[i, j].color = 0 then
			aux[i, j].color = color
			xList.add(i - 1) yList.add(j)
			xList.add(i + 1) yList.add(j)
			xList.add(i) yList.add(j - 1)
			xList.add(i) yList.add(j + 1)
			total = total + 1
		end
	end
	return total
end

fn genAuxMap(GenAuxInfo cont, 2d[GameTile] grid) 2d[AuxTile]
	var aux = new 2d(cont.size, cont.size)[..
		new AuxTile{ color = 0 }
	]
	-- map each contiguous (walkable) landmass with a "color"
	var color = 1
	for i < cont.size, j < cont.size do
		if grid[i, j].canWalk() && aux[i, j].color = 0 then
			var total = _colorMap(grid, aux, i, j, color)
			cont.colorTotals += total
			color = color + 1
		end
	end
	-- determines the color of the "main" landmass
	var mainColor = 1
	for i < |cont.colorTotals| do
		if cont.colorTotals[i] > cont.colorTotals[mainColor] then
			mainColor = i
		end	
	end
	cont.mainColor = mainColor
	return aux
end

	(*
		READ: reads information from locations
	*)

fn avgElevation(2d[GameTile] grid, Int px, Int py, Int w, Int h) Float
	var sum = 0.0
	for i < w, j < h do
		var dx = px + i
		var dy = py + j
		sum = sum + grid[dx, dy].elevBase
	end
	return sum /. toFloat(w * h)
end

	(*
		FIND: finds acceptable location for placement of objects/doors/etc
		- obtains a random (acceptable) map location
	*)

fn rawRandomLoc(GenAuxInfo cont, 2d[AuxTile] aux, 2d[GameTile] grid) (Int, Int)
	var prng = cont.prng
	loop
		(* any angle, 0.3-0.45 away from the center *)
		var rAngle = prng.randomInt(360)
		var rMag = prng.randomInt(floor(toFloat(cont.size) * 0.15)) + floor(toFloat(cont.size) * 0.3)
		var rx = floor(cos(toRadians(rAngle)) * toFloat(rMag)) + cont.size / 2
		var ry = floor(-sin(toRadians(rAngle)) * toFloat(rMag)) + cont.size / 2
		if aux[rx, ry].color = cont.mainColor then
			return (rx, ry)
		end
	end
	return (0, 0)
end

	(*
		- obtains a random (acceptable) location near another location
	*)

fn rawProximaLoc(GenAuxInfo cont, 2d[AuxTile] aux, 2d[GameTile] grid, Int sx, Int sy, Int mag) (Int, Int)
	var prng = cont.prng
	var drift = 0
	loop
		var rAngle = prng.randomInt(360)
		var rx = floor(cos(toRadians(rAngle)) * toFloat(mag)) + sx
		var ry = floor(-sin(toRadians(rAngle)) * toFloat(mag)) + sy
		if rx >= 0 && rx < cont.size && ry >= 0 && ry < cont.size && aux[rx, ry].color = cont.mainColor then
			return (rx, ry)
		end
		drift = drift + 1
		if drift = 6 then
			drift = 0
			mag = mag + 1
		end
	end
	return (0, 0)
end

	(*
		PLACE: places objects/structures on the map
	*)

fn placeBuilding(GenAuxInfo cont, 2d[AuxTile] aux, 2d[GameTile] grid, Int px, Int py, Int w, Int h)
	var prng = cont.prng
	var e = avgElevation(grid, px, py, w, h)
	for i < w + 2, j < h + 3 do
		var dx = px + i - 1
		var dy = py + j - 1
		grid[dx, dy].baseType = Basement
		grid[dx, dy].elevType = FlatElev
		grid[dx, dy].elevBase = e
	end
	for i < w, j < h do
		var dx = px + i
		var dy = py + j - 1
		for k < 3 do
			grid[dx, dy].brickList += new GameBrick{
				type = Brick
			}
		end
	end
	grid[px + 1, py + h - 2].brickList[1].type = Window
	grid[px + 2, py + h - 2].brickList[1].type = Window
	grid[px + 4, py + h - 2].brickList[1].type = Door1
	grid[px + 4, py + h - 2].brickList[0].type = Door2
end