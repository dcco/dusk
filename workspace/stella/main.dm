references Sys modules Os, Input, Sulfur
references Commons.Core
references Main.Space

lin main()
	-- timing
	var clock = newClock(60)
	Sulfur.waitRom()
	-- generation
	var prng = newPRNG(99701)
	var dust = newDustLayer(prng)
	var density = spaceBackGen(prng, dust)
	var t = 0
	--var dust = dustBackGen(prng)
	var starList = starListGen(prng, density, 1600)
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			--sys.update()
			t = t + 1
			if t > _TICK_TOTAL then t = 0 end
			Input.inUpdate()
		end
		if framesPassed >= 1 then
			Sulfur.refresh()
			Sulfur.drawBox(0, 0, 0, 512, 480)
			--drawDensity(density, 2)
			sampleDustLayer(dust, newPRNG(99701), t)
			drawDustLayer(dust)
			drawStarList(starList, dust)
			gc_collect
		end
	end
end