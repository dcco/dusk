references Sys modules Os, Sulfur

fn main()
	var x = 0
	var xspd = 1
	loop
		if x > 512 then xspd = -1 elsif x <= 0 then xspd = 1 end
		x = x + xspd
		Sulfur.draw(Box(x, 11, 10, 10))
		Sulfur.draw(Box(11, x, 10, 10))
		Sulfur.refresh()
	end
end