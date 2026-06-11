references Sys modules Os, Sulfur, Input, Rom
references Commons.Core
references Editor.Main

lin main()
	-- timing
	var clock = newClock(60)
	var editor = newEditor()
	Sulfur.waitRom()
	loop
		var framesPassed = clock.tick()
		for i < framesPassed do
			editor.update()
			Input.inUpdate()
		end
		if framesPassed >= 1 then
			editor.draw()
			Sulfur.refresh()
			gc_collect
		end
	end
end