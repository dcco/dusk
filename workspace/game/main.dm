references Sys modules Os, Input, Sulfur, Rom

struct Waffle{ Int x }

struct MoveObj{ Int x, Int xspd, Waffle z }

lin main()
	-- build map
	var grid = new 2d[0 .. 10 by 20]
	for i < 10, j < 20 do
		if randomInt(2) < 1 then
			grid[i, j] = 1
		else
			grid[i, j] = 0
		end
	end
	-- random test object
	var obj = new MoveObj{ x = 0, xspd = 1, z = new Waffle{ x = 3 } }
	-- timing
	var fps = 60L
	var nsPerFrame = 1000000000L / fps
	var prevTime = Os.time()
	loop
		var curTime = Os.time()
		var framesPassed = toInt((curTime - prevTime) / nsPerFrame)
		for i < framesPassed do
			-- bouncing
			if obj.x > 240 then obj.xspd = -1 elsif obj.x <= 0 then obj.xspd = 1 end
			obj.x = obj.x + obj.xspd
			obj.z = new Waffle{ x = obj.x }
			-- controls
			Input.update()
			if Input.keyDown(^left) then
				obj.xspd = -1
			end
		end
		if framesPassed >= 1 then
			-- draw map
			for i < 10, j < 20 do
				if grid[i, j] = 1 then
					Sulfur.draw(Sprite(i * 8, j * 8, tset, 2))
				end
			end
			-- draw test object
			Sulfur.draw(Sprite(obj.x, obj.z.x + 20, tset, 2))
			-- finish
			Sulfur.refresh()
			gc_collect
			prevTime = prevTime + (toLong(framesPassed) * nsPerFrame)
		end
	end
end