
union SysCmd =
		-- mode commands
	SetBattleModeCmd
		-- camera commands
	| ZoomCamCmd
	| BattleCamCmd
		-- menu commands
	| BattleMenuCmd
		-- timing commands
	| WaitCmd(Int)