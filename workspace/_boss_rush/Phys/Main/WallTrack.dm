
struct WallTrack{
	Bool left,
	Bool right,
	Bool top,
	Bool bot
}

fn newWallTrack() WallTrack
	return new WallTrack{
		left = false,
		right = false,
		top = false,
		bot = false
	}
end

fn reset(WallTrack wall)
	wall.left = false
	wall.right = false
	wall.top = false
	wall.bot = false
end

