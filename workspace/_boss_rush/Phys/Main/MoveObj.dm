
struct MoveObj{
	Rect hitbox,
	WallTrack wall,
	Bool active,
	Int xspd,
	Int yspd
}

fn newMoveObj(Int w, Int h) MoveObj
	return new MoveObj{
		hitbox = newRect(fromPixel(w), fromPixel(h)),
		wall = newWallTrack(),
		active = true,
		xspd = 0,
		yspd = 0
	}
end

fn updateSpd(MoveObj obj)
	if !obj.active then return end
	obj.yspd = min(obj.yspd + 0xF0, 0x1000)
end