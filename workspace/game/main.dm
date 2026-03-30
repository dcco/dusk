references Sys modules Os, Sulfur, Rom

struct MoveObj{ Int x, Int xspd }

fn main()
	var obj = new MoveObj{ x = 0, xspd = 1 }
	var x = 0
	loop
		if obj.x > 240 then obj.xspd = -1 elsif obj.x <= 0 then obj.xspd = 1 end
		obj.x = obj.x + obj.xspd
		for i < 10, j < 10 do
			Sulfur.draw(Sprite(i * 8, j * 8, tset, 2))
		end
		Sulfur.draw(Sprite(obj.x, 120, tset, 2))
		Sulfur.refresh()
	end
end