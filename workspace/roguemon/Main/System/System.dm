
struct System{
	GameWorld world,
	1d[1d[SysCmd]] cmdStack,
	Int pc,
	U64 lastTime
}

fn newSystem() System
	return new System{
		world = newWorld(),
		cmdStack = new 1d[
			new 1d[~SysCmd]
		],
		pc = 0,
		lastTime = Os.time()
	}
end

(*fn procCmd(System sys, SysCmd cmd) Bool
	if cmd is SetBattleModeCmd then
		sys.world.mode = BattleMode
	elsif cmd is ZoomCamCmd then
		sys.world.camera.mode = ZoomCam
	elsif cmd is WaitCmd then 
		sys.lastTime = Os.time()
		return true
	end
	return false
end*)

fn update(System sys)
	sys.world.update()
	-- flag handling
	(*var flag = sys.world.flag
	var fs = flag.state
	if fs is BattleFlag then
		cl += SetBattleModeCmd
		cl += ZoomCamCmd
		cl += WaitCmd(500)
		cl += BattleCamCmd
	end
	flag.state = NoFlag
	-- process commands
	var frameId = |sys.cmdStack| - 1
	var cl = sys.cmdStack[frameId]
	var endFlag = false 
	while sys.pc < |cl| && !endFlag do
		endFlag = sys.procCmd(cl[sys.pc])
		sys.pc = sys.pc + 1
	end
	-- clear stack frame if finished
	if sys.pc = |cl| then
		if |sys.cmdStack| = 1 then
			cl.clear()
		else
			sys.cmdStack /= frameId
		end
	end*)
end

fn draw(System sys)
	sys.world.draw()
end

fn drawMini(System sys)
	sys.world.drawMini()
end

