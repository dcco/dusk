references Sys modules Os, Sulfur, Input, Rom
references Commons modules Core
references Main modules World

lin main()
	-- timing
	var clock = newClock(60)
	var world = newWorld()
	Sulfur.waitRom()
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			world.update()
			Input.inUpdate()
		end
		if framesPassed >= 1 then
			world.draw()
			Sulfur.draw(GText(sfont, 16, 16, 4, "HELLO WORLD!"))
			Sulfur.passRenderVars()
			Sulfur.refresh()
			gc_collect
		end
	end
end

	(*
		simple forests
		dungeon generation

		"home base"
		
		the underwater cave
		the tunnel across sides
	*)