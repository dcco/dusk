references Sys modules Os, Sulfur, Input, Rom
references Commons modules Core
references Main modules System

lin main()
	-- timing
	var clock = newClock(60)
	var sys = newSystem()
	Sulfur.waitRom()
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			sys.update()
			Input.inUpdate()
		end
		if framesPassed >= 1 then
			sys.draw()
			sys.drawMini()
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