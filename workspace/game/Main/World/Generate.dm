
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
	var grid = new 2d(_size, _size)[.. new GameTile{ baseType = 0, elevation = 0.0 }]
	var room = new GameRoom{
		width = _size,
		height = _size,
		grid = grid
	}
	for i < _size, j < _size do
		var h = floor(data.data[i, j] * 255.0)
		var hm = humid.data[i, j]
		-- land type 1
		var landType = 0 -- deep sea
		if h > floor(255.0 * (max * 0.6)) then landType = 7 -- high
		elsif h > floor(255.0 * (avg * 0.2 + max * 0.3)) then landType = 5 -- mid
		elsif h > floor(255.0 * avg * 1.2) then landType = 4 -- low
		elsif h > floor(255.0 * avg * 0.8) then landType = 2 -- beach
		-- h > 20
		elsif h > floor(255.0 * avg * 0.5) then landType = 1 end -- sea
		-- humidity
		-- (hm > 0.7 && landType is High) ||
		if (hm > 0.6 && landType = 5) || (hm > 0.7 && landType = 4) then
			landType = 6 -- forest
		elsif hm > 0.7 && landType = 2 then
			landType = 3 -- mud
		end
		grid[i, j].baseType = landType
		grid[i, j].elevation = h /. 16.0
	end

	return room
end

