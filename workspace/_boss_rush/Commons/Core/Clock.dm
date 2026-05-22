
struct FPSClock{
	Uint64 nsPerFrame,
	Uint64 prevTime
}

fn newClock(Int fps) FPSClock
	var nspf = 1000000000L / toUint64(fps)
	var clock = new FPSClock{
		nsPerFrame = nspf,
		prevTime = Os.time()
	}
	return clock
end

fn tick(FPSClock clock) Int
	var curTime = Os.time()
	var framesPassed = toInt((curTime - clock.prevTime) / clock.nsPerFrame)
	clock.prevTime = clock.prevTime + (toUint64(framesPassed) * clock.nsPerFrame)
	return framesPassed
end