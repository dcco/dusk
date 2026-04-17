references Sys modules Os, Sulfur, Rom
references Commons modules Sys
references Main modules World

lin main()
	-- timing
	var clock = newClock(60)
	var room = generateRoom()
	loop
		var framesPassed = clock.tick()
		(*for i < framesPassed do
		end*)
		if framesPassed >= 1 then
			room.draw()
			Sulfur.draw(GText(sfont, 16, 16, 4, "HELLO WORLD!"))
			Sulfur.refresh()
			gc_collect
		end
	end
end