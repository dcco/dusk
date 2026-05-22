references Sys modules Os, Sulfur, Input, Rom
references Commons modules Core
references Phys modules Main
references Main modules World

lin main()
	-- timing
	var clock = newClock(60)
	Sulfur.waitRom()
	var world = newWorld()
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			world.update()
			Input.inUpdate()
		end
		if framesPassed >= 1 then
			world.draw()
			Sulfur.passRenderVars()
			Sulfur.refresh()
			gc_collect
		end
	end
end
