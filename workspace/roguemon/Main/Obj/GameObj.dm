
--enum+ ObjClass

enum ObjClass =
	Player | DiceObj | CritMon | CrabMon

enum ColClass =
	PlayerCol | DiceCol | EnemyCol

struct GameObj{
	ObjClass type,
	ColClass cType,
	MoveObj body,
	Int mode,
	Bool facingX,
	Int facingY
}

fn newGameObj(ObjClass t) GameObj
	var w = 1
	var h = 1
	var d = 6
	var cc = EnemyCol
	if t is Player then
		w = 3 h = 7 d = 3
		cc = PlayerCol
	elsif t is DiceObj then
		w = 3 h = 3 d = 3
		cc = DiceCol
	elsif t is CritMon then
		w = 7 h = 5
	elsif t is CrabMon then
		w = 8 h = 6
	end
	return new GameObj{
		type = t,
		cType = cc,
		body = newMoveObj(w, h, d),
		mode = 0,
		facingX = false,
		facingY = 0
	}
end

fn draw(GameObj obj)
	var type = obj.type
	var fx = toFP(obj.body.hitbox.x)
	var fy = toFP(obj.body.hitbox.y)
	var fz = toFP(obj.body.hitbox.centerZ())
	var sp = heidi
	var frame = 1
	if type is Player then
		if obj.facingY = -1 then frame = 41
		elsif obj.facingY = 1 then frame = 21 end
		if !obj.body.standFlag then
			frame = frame + 1
		elsif abs(obj.body.xspd) > 0 || abs(obj.body.zspd) > 0 then
			var mf = toInt((Os.time() / 200L) % 4L)
			if mf = 1 then frame = frame + 1
			elsif mf = 3 then frame = frame + 2
			end
		end
		if obj.mode != 0 then
			frame = frame + 3
		end
	elsif type is CritMon then
		sp = crit
		if !obj.body.standFlag then
			if obj.body.yspd < 0 then frame = 2
			else frame = 3 end
		end
	elsif type is CrabMon then
		sp = crab
		frame = 1 + toInt(Os.time() / 500L) % 2
	end
	Sulfur.drawSprite(fx, fy, fz, sp, frame, obj.facingX)
end

fn persMat() Mat4
	var m = newMat4()
	m.ixUpdate(0, 1.3333333)
	m.ixUpdate(5, -2.0)
	m.ixUpdate(10, -1.0020020)
	m.ixUpdate(11, -1.0)
	m.ixUpdate(14, -0.2002002)
	m.ixUpdate(15, 0.0)
	return m
end

globals WorldToScreen{
	pMat = persMat()
}

fn drawTarget(GameObj obj)
	-- target coords
	var tx = toFP(obj.body.hitbox.centerX()) - 0.5
	var ty = toFP(obj.body.hitbox.y) - 1.25
	var tz = toFP(obj.body.hitbox.centerZ())
	-- draw target
	var tf = 4 - toInt((Os.time() / 150L) % 5L)
	setSPFlag(0.0)
	Sulfur.drawSprite(tx, ty - (toFloat(tf) * 0.125), tz, targetA, tf, false)
	setSPFlag(1.0)
end

(*fn drawTarget(GameObj obj)
	-- target coords
	var tarX = toFP(obj.body.hitbox.centerX())
	var tarY = toFP(obj.body.hitbox.y)
	var tarZ = toFP(obj.body.hitbox.centerZ())
	-- go back to screen space
	var t = (tarX, tarY, tarZ, 1.0)
	t = mult(RV.mvMat, t)
	t = mult(WorldToScreen.pMat, t)
	var nx = ((t.1 /. t.4) + 1.0) /. 2.0
	var ny = (1.0 - (t.2 /. t.4)) /. 2.0
	-- draw targeter
	var tf = toInt((Os.time() / 200L) % 6L)
	if tf = 4 then tf = 2
	elsif tf = 5 then tf = 1 end
	-- - divided for zoom
	Sulfur.draw(GSprite(toInt(nx * 480.0) - 8, toInt(ny * 320.0) - 44, targetA, tf, false))
end*)

(*fn drawTarget(GameObj obj)
	-- target coords
	var tarX = toFP(obj.body.hitbox.centerX())
	var tarY = toFP(obj.body.hitbox.centerY())
	var tarZ = toFP(obj.body.hitbox.centerZ())
	-- go back to screen space
	var t = (tarX, tarY, tarZ, 1.0)
	t = mult(RV.mvMat, t)
	t = mult(WorldToScreen.pMat, t)
	var nx = ((t.1 /. t.4) + 1.0) /. 2.0
	var ny = (1.0 - (t.2 /. t.4)) /. 2.0
	--Sulfur.draw(GText(sfont, 8, 168, 4, toString(nx)))
	--Sulfur.draw(GText(sfont, 8, 176, 4, toString(ny)))
	-- draw targeter
	var tf = toInt((Os.time() / 200L) % 4L) + 1
	-- - divided for zoom
	Sulfur.draw(GSprite(toInt(nx * 480.0) - 14 - (tf * 2), toInt(ny * 320.0) - 12, target1, tf, false))
	Sulfur.draw(GSprite(toInt(nx * 480.0) + 6 + (tf * 2), toInt(ny * 320.0) - 12, target1, tf, true))
end*)