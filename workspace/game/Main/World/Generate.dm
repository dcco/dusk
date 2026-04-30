
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
	var max = 0.0
	var avg = 0.0
	var _n = 0.0
	for i < _size, j < _size do
		if data.data[i, j] > max then max = data.data[i, j] end
		if data.data[i, j] > 0.0 then
			avg = avg + data.data[i, j]
			_n = _n + 1.0
		end
	end
	avg = avg /. _n

	-- room creation
	var grid = new 2d(_size, _size)[..
		new GameTile{
			baseType = 0,
			elevation = 0.0,
			frontDiff = 0,
			leftDiff = 0,
			rightDiff = 0
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
		grid = grid
	}
	for i < _size, j < _size do
		var h = floor(data.data[i, j] * 255.0)
		var hm = humid.data[i, j]
		-- land type 1
		var landType = DeepSea -- deep sea
		if h > floor(255.0 * (max * 0.6)) then landType = High -- high
		elsif h > floor(255.0 * (avg * 0.2 + max * 0.3)) then landType = Mid -- mid
		elsif h > floor(255.0 * avg * 1.2) then landType = Low -- low
		elsif h > floor(255.0 * avg * 0.8) then landType = Beach -- beach
		-- h > 20
		elsif h > floor(255.0 * avg * 0.5) then landType = Sea end -- sea
		-- humidity
		-- (hm > 0.7 && landType is High) ||
		if (hm > 0.6 && landType = Mid) || (hm > 0.7 && landType = Low) then
			landType = Forest -- forest
		elsif hm > 0.7 && landType = Beach then
			landType = Mud -- mud
		end
		grid[i, j].baseType = landType
		grid[i, j].elevation = h /. 16.0
	end

	-- elevation normalizing
	for i < _size, j < _size do
		var landType = grid[i, j].baseType
		if landType = 0 || landType = 1 then
			grid[i, j].elevation = 0.0
		elsif landType = 2 || landType = 3 then
			grid[i, j].elevation = 1.0
			--grid[i, j].elevType = 1
		end
	end

	-- final elevation reification
	for i < _size, j < _size do
		var landType = grid[i, j].baseType
		if landType != 0 && landType != 1 then
			var e = grid[i, j].elevation
			var eF = room.elevation(i, j + 1)
			if eF < e then
				grid[i, j].frontDiff = toInt(ceil(e - eF))
			end
			var eL = room.elevation(i - 1, j)
			if eL < e then
				grid[i, j].leftDiff = toInt(ceil(e - eL))
			end
			var eR = room.elevation(i + 1, j)
			if eR < e then
				grid[i, j].rightDiff = toInt(ceil(e - eR))
			end
		end
	end

	return room
end

