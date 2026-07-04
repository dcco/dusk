
struct FPSClock{
	U64 nsPerFrame,
	U64 prevTime
}

fn newClock(Int fps) FPSClock
	var nspf = 1000L / toU64(fps)
	var clock = new FPSClock{
		nsPerFrame = nspf,
		prevTime = Os.time()
	}
	return clock
end

fn tick(FPSClock clock) Int
	var curTime = Os.time()
	var framesPassed = toInt((curTime - clock.prevTime) / clock.nsPerFrame)
	clock.prevTime = clock.prevTime + (toU64(framesPassed) * clock.nsPerFrame)
	return framesPassed
end