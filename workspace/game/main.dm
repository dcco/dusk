references Sys modules Os, Sulfur, Rom

fn main()
	var x = 0
	var xspd = 1
	loop
		if x > 240 then xspd = -1 elsif x <= 0 then xspd = 1 end
		x = x + xspd
		for i < 10, j < 10 do
			Sulfur.draw(Sprite(i * 8, j * 8, tset, 2))
		end
			Sulfur.draw(Sprite(x, 120, tset, 2))
		Sulfur.refresh()
	end
end