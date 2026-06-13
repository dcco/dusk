
struct TilePal{
	Int z
}

fn newTilePal() TilePal
	return new TilePal{
		z = 0
	}
end

fn draw(TilePal pal)
	var dx = canvasWidth() - 120
	var dy = 0
	Sulfur.drawBox(0x77554f, dx, dy, 120, canvasHeight())
	-- draw initial tile palette
	var p1x = dx + 8
	var p1y = dy + 8
	for i < 2 do
		var tt = TType(i + 1)
		var sf = tt.sFrame
		if tt.rt isnt NoReify then sf = sf + 1 end
		Sulfur.drawSprite(p1x + (i * 8), p1y, tset, sf, false)
	end
end
