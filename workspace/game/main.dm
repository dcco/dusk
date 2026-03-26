references Sys modules Os, Sulfur, Rom

fn main()
	var x = 0
	var xspd = 1
	loop
		if x > 512 then xspd = -1 elsif x <= 0 then xspd = 1 end
		x = x + xspd
		Sulfur.draw(Sprite(0, 0, tset, 1))
		(*for i < 10, j < 10 do
			Sulfur.draw(Sprite(i * 8, j * 8, tset, 1))
		end*)
		Sulfur.refresh()
	end
end