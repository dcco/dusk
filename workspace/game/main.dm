references Sys modules Sulfur
references Commons modules System

lin main()
	-- timing
	var clock = newClock(60)
	loop
		var framesPassed = clock.tick()
		(*for i < framesPassed do
		end*)
		if framesPassed >= 1 then
			Sulfur.refresh()
			gc_collect
		end
	end
end