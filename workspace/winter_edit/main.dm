references Sys modules Sulfur, Input
references Commons.core

fn main()
	var clock = newFPSClock(60)
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			Input.inUpdate()
		end
		if framesPassed >= 1 then
			(*world.draw()
			Sulfur.passRenderVars()
			Sulfur.refresh()
			gc_collect*)
		end
	end
end