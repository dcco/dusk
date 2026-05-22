
fn generateRoom() GameRoom
	var _size = 600
	--var prng = newPRNG(30461)
	var prng = newPRNG(7889)
	var simplex = newSimplex(prng)

	var data = newDataLayer(_size, _size)
	var humid = newDataLayer(_size, _size)
	-- random second circle
	var cx = (prng.randomFloat() * 0.5) + 0.25
	var cy = (prng.randomFloat() * 0.5) + 0.25
	-- elevation calc
	var _frac = 0.333
	data.addSimplex(simplex, 0.7, 0.03 * _frac, 0.03 * _frac, 0.0)
	data.addSimplex(simplex, 0.2, 0.08 * _frac, 0.08 * _frac, 1.0)
	data.addSimplex(simplex, 0.1, 0.2 * _frac, 0.2 * _frac, 4.0)
	var mask1 = newDataLayer(_size, _size)
	var mask2 = newDataLayer(_size, _size)
	mask1.fillCircle(toFloat(_size) * 0.5, toFloat(_size) * 0.5, toFloat(_size) * 0.5, 1.5, true)
	mask2.fillCircle(toFloat(_size) * cx, toFloat(_size) * cy, toFloat(_size) * 0.25, 2.0, false)
	mask1.compLayerMul(mask2)
	data.compLayerMul(mask1)
	-- humidity calc
	humid.addSimplex(simplex, 0.8, 0.08 * _frac, 0.07 * _frac, 3.0)
	humid.addSimplex(simplex, 0.4, 0.5 * _frac, 0.5 * _frac, 2.0)

	-- distribution calcs
	var gMax = 0.0
	var avg = 0.0
	var _n = 0.0
	for i < _size, j < _size do
		if data.data[i, j] > gMax then gMax = data.data[i, j] end
		if data.data[i, j] > 0.0 then
			avg = avg + data.data[i, j]
			_n = _n + 1.0
		end
	end
	avg = avg /. _n

	-- room creation
	var grid = new 2d(_size, _size)[..
		new GameTile{
			baseType = DeepSea,
			frontDiff = 0,
			leftDiff = 0,
			rightDiff = 0,
			elevType = FlatElev,
			elevBase = 0.0,
			e1 = 0.0,
			e2 = 0.0,
			e3 = 0.0,
			e4 = 0.0
			(*elevType = 0,
			e1 = 0.0,
			e2 = 0.0,
			e3 = 0.0,
			e4 = 0.0*)
		}
	]
	var room = new GameRoom{
		width = _size,
		height = _size,
		grid = grid,
		objList = new 1d[~MoveObj]
	}
	for i < _size, j < _size do
		var h = floor(data.data[i, j] * 255.0)
		var hm = humid.data[i, j]
		-- land type 1
		var landType = DeepSea -- deep sea
		if h > floor(255.0 * (gMax * 0.6)) then landType = High -- high
		elsif h > floor(255.0 * (avg * 0.2 + gMax * 0.3)) then landType = Mid -- mid
		elsif h > floor(255.0 * avg * 1.2) then landType = Low -- low
		elsif h > floor(255.0 * avg * 0.8) then landType = Beach -- beach
		-- h > 20
		elsif h > floor(255.0 * avg * 0.5) then landType = Sea end -- sea
		-- humidity
		-- (hm > 0.7 && landType is High) ||
		if (hm > 0.6 && landType is Mid) || (hm > 0.7 && landType is Low) then
			landType = Forest -- forest
		elsif hm > 0.7 && landType is Beach then
			landType = Mud -- mud
		end
		grid[i, j].baseType = landType
		grid[i, j].elevBase = toFloat(h) /. 8.0
		--grid[i, j].elevation = toFloat(h) /. 16.0
	end

	-- elevation normalizing
	for i < _size, j < _size do
		var landType = grid[i, j].baseType
		grid[i, j].elevBase = grid[i, j].elevBase * 1.3
		if landType is DeepSea || landType is Sea then
			grid[i, j].elevBase = 1.0
		elsif landType is Beach || landType is Mud then
			grid[i, j].elevType = HillElev
		(*elsif landType is Beach || landType is Mud then
			grid[i, j].elevation = 1.0*)
			--grid[i, j].elevType = 1
		end
	end

	-- final elevation reification
	for i < _size, j < _size do
		var landType = grid[i, j].baseType
		var eType = grid[i, j].elevType
		-- set initial elevation
		var e = grid[i, j].elevBase
		grid[i, j].e1 = e
		grid[i, j].e2 = e
		grid[i, j].e3 = e
		grid[i, j].e4 = e
		if eType is FlatElev then
			-- enable walls when necessary
			if !landType is DeepSea && !landType is Sea then
				var eF = room.elevation(i, j + 1)
				if eF < e then
					grid[i, j].frontDiff = ceil(e - eF)
				end
				var eL = room.elevation(i - 1, j)
				if eL < e then
					grid[i, j].leftDiff = ceil(e - eL)
				end
				var eR = room.elevation(i + 1, j)
				if eR < e then
					grid[i, j].rightDiff = ceil(e - eR)
				end
			end
		else
			-- connect hill corners
			var adjX = grid[i - 1, j].elevType
			var adjY = grid[i, j - 1].elevType
			var ab1 = grid[i + 1, j].elevType
			var ab2 = grid[i, j + 1].elevType
			var ab3 = grid[i + 1, j + 1].elevType
			var ac1 = grid[i + 1, j - 1].elevType
			var ac2 = grid[i - 1, j + 1].elevType
			if adjX is HillElev then
				grid[i, j].e1 = max(e, grid[i - 1, j].e2)
				grid[i, j].e3 = max(e, grid[i - 1, j].e4)
			end
			if adjY is HillElev then
				grid[i, j].e1 = max(grid[i, j].e1, grid[i, j - 1].e3)
				grid[i, j].e2 = max(e, grid[i, j - 1].e4)
			end
			if ab1 is HillElev then
				grid[i, j].e4 = max(e, room.elevation(i + 1, j))
				grid[i, j].e2 = max(grid[i, j].e2, room.elevation(i + 1, j))
			end
			if ab2 is HillElev then
				grid[i, j].e4 = max(grid[i, j].e4, room.elevation(i, j + 1))
				grid[i, j].e3 = max(grid[i, j].e3, room.elevation(i, j + 1))
			end
			if ab3 is HillElev then
				grid[i, j].e4 = max(grid[i, j].e4, room.elevation(i + 1, j + 1))
			end
			if ac1 is HillElev then
				grid[i, j].e2 = max(grid[i, j].e2, grid[i + 1, j - 1].e3)
			elsif ac2 is HillElev then
				grid[i, j].e3 = max(grid[i, j].e3, grid[i - 1, j + 1].e3)
			end
		end
	end

	return room
end

