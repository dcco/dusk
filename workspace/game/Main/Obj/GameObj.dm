
--enum+ ObjClass

struct GameObj{
	--ObjClass type,
	MoveObj body,
	Bool facingX,
	Int facingY
}

fn newGameObj(Int w, Int h, Int d) GameObj
	return new GameObj{
		--type = type,
		body = newMoveObj(w, h, d),
		facingX = false,
		facingY = 0
	}
end

fn draw(GameObj obj)
	var fx = toFP(obj.body.hitbox.x)
	var fy = toFP(obj.body.hitbox.y)
	var fz = toFP(obj.body.hitbox.centerZ())
	var frame = 1
	if obj.facingY = -1 then frame = 7
	elsif obj.facingY = 1 then frame = 4 end
	if !obj.body.standFlag then
		frame = frame + 1
	elsif abs(obj.body.xspd) > 0 || abs(obj.body.zspd) > 0 then
		var mf = toInt((Os.time() / 200L) % 4L)
		if mf = 1 then frame = frame + 1
		elsif mf = 3 then frame = frame + 2
		end
	end
	Sulfur.drawSprite(fx, fy, fz, heidi, frame, obj.facingX)
end