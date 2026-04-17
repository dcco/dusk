references Sys modules Os, Sulfur, Rom
references Commons modules Sys

lin main()
	-- timing
	var clock = newClock(60)
	loop
		var framesPassed = clock.tick()
		if framesPassed >= 1 then
			Sulfur.draw(GText(sfont, 16, 16, 4, "HELLO WORLD!"))
			Sulfur.refresh()
			gc_collect
		end
	end
end