
struct FPSClock{
	Long nsPerFrame,
	Long prevTime
}

fn newClock(Int fps)
	var nspf = 1000000000L / toLong(fps)
	var clock = new FPSClock{
		nsPerFrame = nspf,
		prevTime = Os.time()
	}
	return clock
end

fn tick(FPSClock clock) Int
	var curTime = Os.time()
	var framesPassed = toInt((curTime - clock.prevTime) / clock.nsPerFrame)
	clock.prevTime = clock.prevTime + (toLong(framesPassed) * clock.nsPerFrame)
	return framesPassed
end