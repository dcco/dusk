references Sys modules Os, Sulfur, Rom
references Commons modules Sys

lin main()
	-- timing
	var clock = newClock(60)
	loop
		var framesPassed = clock.tick()
		if framesPassed >= 1 then
			Sulfur.draw(GText(sfont, 16, 16, 4, "HELLO WORLD!"))
			for i < 10, j < 10, k < 6 do
				Sulfur.draw(G3Test(toFloat(i * 2), toFloat(j * 2), toFloat(-(k + 4) * 2), tset, 2))
			end
			Sulfur.refresh()
			gc_collect
		end
	end
end