
-- const _PIXEL_E = 9
-- const _TILE_E = 12

fn toTile(Int x) Int
	if x < 0 then return ((x + 1) / 4096) - 1 end
	return x / 4096
end

fn fromTile(Int x) Int
	return x * 4096
end

fn toPixel(Int x) Int
	if x < 0 then return ((x + 1) / 512) - 1 end
	return x / 512
end

fn fromPixel(Int x) Int
	return x * 512
end

fn pixelTile(Int x) Int
	if x < 0 then return ((x + 1) / 8) - 1 end
	return x / 8
end

fn tilePixel(Int x) Int
	return x * 8
end

fn toFP(Int x) Float
	return x /. 4096
end