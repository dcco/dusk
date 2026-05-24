
struct MoveObj{
	Box3D hitbox,
	Bool active,
	Bool standFlag,
	Int xspd,
	Int zspd,
	Int yspd
}

fn newMoveObj(Int w, Int h, Int d) MoveObj
	return new MoveObj{
		hitbox = newBox3D(fromPixel(w), fromPixel(h), fromPixel(d)),
		active = true,
		standFlag = false,
		xspd = 0,
		zspd = 0,
		yspd = 0
	}
end

fn updateSpd(MoveObj obj)
	if !obj.active then return end
	obj.yspd = min(obj.yspd + 0xA0, 0x800)
end