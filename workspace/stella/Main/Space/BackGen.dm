
	(*
		generic math functions
	*)

fn max(Float a, Float b) Float
	if a < b then return b else return a end
end

fn max(Int a, Int b) Int
	if a < b then return b else return a end
end

fn lerp(Float a, Float b, Float x) Float
	return a + (b - a) * x
end

fn inverseLerp(Float a, Float b, Float x) Float
	return (x - a) /. (b - a)
end

fn clamp(Int a, Int min, Int max) Int
	if a < min then return min
	elsif a > max then return max end
	return a
end

fn clamp(Float a, Float min, Float max) Float
	if a < min then return min
	elsif a > max then return max end
	return a
end

fn smoothstep(Float a) Float
	return a * a * (3.0 - 2.0 * a)
end

fn smoothstepFull(Float e1, Float e2, Float a) Float
	var b = (a - e1) /. (e2 - e1)
	b = clamp(b, 0.0, 1.0)
	return b * b * (3.0 - 2.0 * b)
end

const _PI = 3.14159265

fn randomGauss2D() (Float, Float)
	var r = sqrt(-2.0 * ln(randomFloat()))
	var theta = 2.0 * _PI * randomFloat()
	return (r * cos(theta), r * sin(theta))
end

	(*
		generic noise generation
	*)

fn fracBrownMotion(Simplex s, Float x, Float y) Float
	var v = 0.0
	var amp = 1.0
	var freq = 1.0
	var totalAmp = 0.0

	for i < 7 do
		v = v + (amp * s.noise(x * freq, y * freq))
		totalAmp = totalAmp + amp
		amp = amp * 0.5
		freq = freq * 2.0
	end

	return v /. totalAmp
end



	(*
		nebula (dust) generation
	*)

enum DustColor attrs { Int r, Int g, Int b } =
	BlueDust{ 80, 120, 255 }
	| PurpleDust{ 180, 100, 255 }
	| RedDust{ 255, 120, 80 }
	| TealDust{ 80, 255, 220 }

struct Dust{
	Float dust,
	Float cloud,
	(Int, Int, Int) color,
	Float darkDust
}

fn lerpColor((Int, Int, Int) c1, (Int, Int, Int) c2, Float f) (Int, Int, Int)
	var r = lerp(toFloat(c1.1), toFloat(c2.1), f)
	var g = lerp(toFloat(c1.2), toFloat(c2.2), f)
	var b = lerp(toFloat(c1.3), toFloat(c2.3), f)
	return (floor(r), floor(g), floor(b))
end

fn newDustLayer(PRNG prng) 2d[Dust]
	var s = newSimplex(prng)
	var layer = new 2d(256, 256)[..
		new Dust{
			dust = 0.0,
			cloud = 0.0,
			color = (255, 255, 255),
			darkDust = 0.0
		}
	]
	-- dust coloring
	for i < 256, j < 256 do
		var c = fracBrownMotion(s, toFloat(i) * 0.003 + 1000.0, toFloat(j) * 0.003 + 1000.0)
		c = (c + 1.0) * 0.5
		var c1 = BlueDust
		var c2 = TealDust
		if c < 0.4 then
			c1 = BlueDust c2 = PurpleDust c = c /. 0.4
		elsif c < 0.7 then
			c1 = PurpleDust c2 = RedDust c = (c - 0.4) /. 0.3
		else
			c1 = RedDust c2 = TealDust c = (c - 0.7) /. 0.3
		end
		layer[i, j].color = lerpColor((c1.r, c1.g, c1.b), (c2.r, c2.g, c2.b), c)
	end
	return layer
end

	(* step must be from 0-1 *)

fn sample(Dust d, Simplex s, Int sampleScale, Int x, Int y, Float step)
	var fs1 = 0.003 *  256.0 /. toFloat(sampleScale)
	var fs2 = 0.008 * 256.0 /. toFloat(sampleScale)
	var fs3 = 0.012 * 256.0 /. toFloat(sampleScale)
	var fsw = 45.0 * 256.0 /. toFloat(sampleScale)
	-- domain warp
	var stepAngle = 2.0 * _PI * step
	var dx = fracBrownMotion(s,	(toFloat(x) * fs1) + 17.3 + (cos(stepAngle) * 40.0), (toFloat(y) * fs1) + 41.2)
	var dy = fracBrownMotion(s, (toFloat(x) * fs1) + 93.7, (toFloat(y) * fs1) + 12.8 + (cos(stepAngle + 0.34) * 40.0))
	dx = (dx + 1.0) * 0.5
	dy = (dy + 1.0) * 0.5
	-- dust + detail
	var dust = fracBrownMotion(s, (toFloat(x) + fsw * dx) * fs2, (toFloat(y) + fsw * dy) * fs2)
	dust = (dust + 1.0) * 0.5
	dust = smoothstepFull(0.4, 0.8, dust)
	var detail = fracBrownMotion(s, toFloat(x) * fs3, toFloat(y) * fs3)
	detail = (detail + 1.0) * 0.5
	dust = clamp(dust * lerp(0.7, 1.3, detail), 0.0, 1.0)
	-- return dust
	d.dust = dust
end

--const _TICK_TOTAL = 170034
const _TICK_TOTAL = 60034

fn sampleDustLayer(2d[Dust] layer, PRNG prng, Int tick)
	var s = newSimplex(prng)
	var step = tick /. _TICK_TOTAL
	--var step = toFloat((Os.time() / 30L) % 150034L) /. 150034.0
	for i < 256, j < 256 do
		layer[i, j].sample(s, 256, i, j, step)
	end
end

fn drawDustLayer(2d[Dust] layer)
	for i < 256, j < 256 do
		var dust = layer[i, j]
		var (r, g, b) = dust.color
		r = floor(dust.dust * toFloat(r) * 0.1)
		g = floor(dust.dust * toFloat(g) * 0.1)
		b = floor(dust.dust * toFloat(b) * 0.1)
		if dust.cloud > 0.001 then
			r = clamp(r + floor(dust.cloud * 150.0 * 0.15), 0, 255)
			g = clamp(g + floor(dust.cloud * 150.0 * 0.15), 0, 255)
			b = clamp(b + floor(dust.cloud * 255.0 * 0.15), 0, 255)
		end
		if dust.darkDust > 0.05 then
			r = clamp(r - floor(dust.darkDust * 255.0 * 0.05), 0, 255)
			g = clamp(g - floor(dust.darkDust * 255.0 * 0.02), 0, 255)
			b = clamp(b - floor(dust.darkDust * 255.0 * 0.05), 0, 255)
		end
		var c = (r * 0x10000) + (g * 0x100) + b
		Sulfur.drawBox(c, i * 2, j * 2, 2, 2)
	end
end

	(*
		galaxy density generation
	*)

const _E = 2.7182818

	(* range of [0, 1] *)

fn spaceBackGen(PRNG prng, 2d[Dust] dustLayer) DataLayer
	var s = newSimplex(prng)
		(* 128 x 128 galaxy density generation *)
	var density = newDataLayer(128, 128)
	for i < 128, j < 128 do
		density.data[i, j] = fracBrownMotion(s, toFloat(i) * 0.03, toFloat(j) * 0.03)
		density.data[i, j] = (density.data[i, j] + 1.0) * 0.5
		density.data[i, j] = smoothstepFull(0.2, 0.95, density.data[i, j])
	end
		(* 
			milky way banding
			- band direction
		*)
	var (bdx, bdy) = (3.0 /. sqrt(10.0), -1.0 /. sqrt(10.0))
	var (bcx, bcy) = (64, 64)
	for i < 128, j < 128 do
			(* - main density banding *)
		var (diffX, diffY) = (i - bcx, j - bcy)
		var dot = toFloat(diffX) * -bdy + toFloat(diffY) * bdx
		--var d = (j - 64) /. 15
		var p = (dot * dot) /. (2.0 * 15.0 * 15.0)
		--var p = ((j - 64) /. 15) ** 2.0
		var band = clamp(_E ** -p, 0.0, 1.0)
		--if abs(j - 64) < 2 then band = 1.0 end
		density.data[i, j] = density.data[i, j] * lerp(0.15, 1.0, band)
			--clamp(1.0 * lerp(0.3, 2.0, band), 0.0, 1.0)
			(* - dust banding *)
		for ax < 2, ay < 2 do
			var zx = i * 2 + ax
			var zy = j * 2 + ay
				(* - main milky way cloud *)
			var mwNoise = fracBrownMotion(s, toFloat(zx) * 0.003 + 300.0, toFloat(zy) * 0.003 + 200.0)
			mwNoise = ((mwNoise + 1.0) * 0.5) ** 1.2
			var mwn2 = fracBrownMotion(s, toFloat(zx) * 0.02 + 100.0, toFloat(zy) * 0.02 + 150.0)
			mwn2 = (mwn2 + 1.0) * 0.5
			mwNoise = clamp((mwNoise * 0.8) + (mwn2 * 0.3), 0.0, 1.0)
			var cloud = band * smoothstepFull(0.35, 0.75, mwNoise)
			dustLayer[zx, zy].cloud = cloud ** 0.9
				(* - milky way dark dust *)
			var ldx = fracBrownMotion(s, (toFloat(zx) * 0.03) + 57.8, (toFloat(zy) * 0.03) + 61.1)
			var ldy = fracBrownMotion(s, (toFloat(zx) * 0.03) + 88.5, (toFloat(zy) * 0.03) + 3.9)
			ldx = (ldx + 1.0) * 0.5
			ldy = (ldy + 1.0) * 0.5
			var lane = fracBrownMotion(s,
				(toFloat(zx) + ldx * 30.0) * 0.006 + 179.0,
				(toFloat(zy) + ldy * 30.0) * 0.006 + 189.4)
			lane = (lane + 1.0) * 0.5
			var ln2 = fracBrownMotion(s, toFloat(zx) * 0.01 + 44.2, toFloat(zy) * 0.01 + 61.0)
			ln2 = (ln2 + 1.0) * 0.5
			lane = clamp((lane * 0.7) + (ln2 * 0.35), 0.0, 1.0)
			lane = (band ** 1.4) * smoothstepFull(0.4, 0.75, lane)
			dustLayer[zx, zy].darkDust = lane ** 1.3
		end
	end
	return density
end

fn drawDensity(DataLayer layer, Int scale)
	var size = layer.width
	for i < size, j < size do
		var z = floor(layer.data[i, j] * 255.0)
		var c = z
		Sulfur.drawBox(c, i * scale, j * scale, scale, scale)
	end
end

	(*
		star generation
	*)

fn randColor(PRNG prng) StarColor
	var f = prng.randomFloat()
	if f < 0.7 then return WhiteStar
	elsif f < 0.85 then return BlueStar
	elsif f < 0.95 then return YellowStar
	elsif f < 0.99 then return OrangeStar
	else return RedStar end
end

fn randStar(PRNG prng, Int x, Int y) Star
	var star = new Star{
		pos = (x, y),
		brightness = prng.randomFloat() ** 12.0,
		color = randColor(prng),
		twinkle = 0.0,
		twinkPeriod = 0,
		twinkPhase = 0.0
	}
	if star.brightness > 0.7 then
		star.twinkle = 0.4 + (prng.randomFloat() * 0.1)
		star.twinkPeriod = 2000 + prng.randomInt(6000)
		star.twinkPhase = prng.randomFloat() * 2.0 * _PI
	end
	return star
end

fn starListGen(PRNG prng, DataLayer density, Int total) 1d[Star]
	var starList = new 1d[~Star]
	while |starList| < total do
		var x = prng.randomInt(512)
		var y = prng.randomInt(480)
		var d = density.data[x / 4, y / 4]
		if prng.randomFloat() <= d + 0.05 then
			starList += randStar(prng, x, y)
		end
	end
	for i < 20 do
		var cx = prng.randomInt(512)
		var cy = prng.randomInt(480)
		var radius = toFloat(prng.randomInt(40) + 10)
		var count = prng.randomInt(100) + 50
		for j < count do
			var (ox, oy) = randomGauss2D()
			var rx = cx + floor(ox * radius)
			var ry = cy + floor(oy * radius)
			starList += randStar(prng, rx, ry)
		end
	end
	return starList
end

fn drawStarList(1d[Star] starList, 2d[Dust] dustLayer)
	var time = Os.time()
	for i < |starList| do
		var star = starList[i]
		-- dust calc
		var ddDark = 0.0
		var ddLight = 0.0
		var ddDust = 0.0
		var dx = star.pos.1 / 2
		var dy = star.pos.2 / 2
		var dustColor = (0, 0, 0)
		if dx >= 0 && dx < 256 && dy >= 0 && dy < 256 then
			var dust = dustLayer[dx, dy]
			ddDark = clamp(dust.dust + dust.cloud, 0.0, 1.0)
			if dust.cloud < 0.05 then ddLight = dust.dust end
			ddDust = dust.darkDust
			dustColor = dust.color
		end
		-- twinkle attenuation
		var br = star.brightness
		if star.twinkle != 0.0 then
			var v1 = toInt(time % toU64(star.twinkPeriod)) /. star.twinkPeriod
			var p2 = floor(toFloat(star.twinkPeriod) * 1.73)
			var v2 = toInt(time % toU64(p2)) /. p2
			var phase2 = star.twinkPhase * 1.73
			var amp = sin(v1 * 2.0 * _PI + star.twinkPhase + v2 * 2.0 * _PI + phase2)
			var twinkle = 0.2 + ((1.0 + amp) * 0.5 * 0.8)
			br = br * twinkle
		end
		br = br * (1.0 - 0.8 * ddLight)
		br = br * (1.0 - 0.5 * ddDust)
		if ddDark > 0.2 && br < ddDark * 0.12 then
			br = ddDark * 0.125
			br = clamp(br - (ddDust * 0.05), 0.0, 1.0)
		end
		-- raw color calc
		var colorType = star.color
		var color = (colorType.r, colorType.g, colorType.b)
		if ddDark > 0.2 then
			color = lerpColor(color, dustColor, 0.5)
		end
		var r = floor(clamp(toFloat(color.1) * br, 0.0, 255.0))
		var g = floor(clamp(toFloat(color.2) * br, 0.0, 255.0))
		var b = floor(clamp(toFloat(color.3) * br, 0.0, 255.0))
		var c = (r * 0x10000) + (g * 0x100) + b
		Sulfur.drawBox(c, star.pos.1, star.pos.2, 1, 1)
	end
end