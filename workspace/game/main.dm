references Sys modules Os, Sulfur, Input, Rom
references Commons modules Core
references Main modules World

lin main()
	-- timing
	var clock = newClock(60)
	var world = generateRoom()
	var px = 300.0
	var pz = 520.0
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			if keyDown(^left) then
				px = px - 0.12
			elsif keyDown(^right) then
				px = px + 0.12
			end
			if keyDown(^up) then
				pz = pz - 0.12
			elsif keyDown(^down) then
				pz = pz + 0.12
			end
			Input.update()
		end
		if framesPassed >= 1 then
			var tx = toInt(floor(px))
			var tz = toInt(floor(pz))
			var elevation = world.elevation(tx, tz)
			--updateCamMat(-40, -px - 0.5, elevation + 2.8, -pz - 8.5)
			updateCamMat(-45, -px - 0.5, elevation + 8.0, -pz - 10.5)
			
			Sulfur.draw(GText(sfont, 16, 16, 4, "HELLO WORLD!"))
			world.draw(tx, tz)
			(*for i < 10, j < 10, k < 6 do
				Sulfur.draw(G3Test(toFloat(i * 2), toFloat(j * 2), toFloat(-(k + 4) * 2), tset, 2))
			end*)
			Sulfur.refresh()
			gc_collect
		end
	end
end