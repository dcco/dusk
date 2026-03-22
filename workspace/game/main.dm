references Sys modules Os, Sulfur, Rom

fn main()
	var x = 0
	var xspd = 1
	loop
		if x > 512 then xspd = -1 elsif x <= 0 then xspd = 1 end
		x = x + xspd
		Sulfur.draw(Sprite(x1, 66))
		Sulfur.refresh()
	end
end