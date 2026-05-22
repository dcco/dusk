
struct MoveObj{
	Box3D hitbox,
	Bool active,
	Int xspd,
	Int zspd,
	Int yspd
}

fn newMoveObj(Int w, Int h, Int d) MoveObj
	return new MoveObj{
		hitbox = newBox3D(fromPixel(w), fromPixel(h), fromPixel(d)),
		active = true,
		xspd = 0,
		zspd = 0,
		yspd = 0
	}
end
